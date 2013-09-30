%% Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
%% Property tests for Piqi Erlang Runtime library
%% 
-module(piqirun_props).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").


%% reference implementation for piqirun:encode_varint_value/1
-spec encode_varint_value/1 :: (
    I :: non_neg_integer()) -> binary().

encode_varint_value(I) ->
    encode_varint_value(I, []).


encode_varint_value(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint_value(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint_value(First_X_Bits, [With_Leading_Bit|Acc]).


%% reference implementation for piqirun:decode_varint/1
decode_varint(Bytes) ->
    decode_varint(Bytes, []).
decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Acc1 = [I|Acc],
    Result = 
        lists:foldl(
            fun(X, Acc0) ->
                (Acc0 bsl 7 bor X)
            end, 0, Acc1),
    {Result, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]);
decode_varint(Bytes, _Acc) when is_binary(Bytes) ->
    % Return the same error as returned by my_split_binary/2 when parsing
    % fields (see below). This way, when there's not enough data for parsing a
    % field, the returned error is the same regardless of where it occured.
    piqirun:throw_error('not_enough_data').


varint_range() -> range(0, 16#ffffffffffffffff). % uint64


% check that in varint reference implementation: decode(encode(X)) == X
prop_varint_reference_enc_dec() ->
    ?FORALL(I, varint_range(),
        begin
            Enc = encode_varint_value(I),
            Bin = iolist_to_binary(Enc),
            {I, <<>>} =:= decode_varint(Bin)
        end
    ).


% check that the actual varint implementation: decode(encode(X)) == X
prop_varint_enc_dec() ->
    ?FORALL(I, varint_range(),
        begin
            Enc = piqirun:encode_varint_value(I),
            Bin = iolist_to_binary(Enc),
            {I, <<>>} =:= piqirun:decode_varint(Bin)
        end
    ).


% check piqirun varint encoder over reference implementation
prop_varint_enc() ->
    ?FORALL(I, varint_range(),
        begin
            Enc = piqirun:encode_varint_value(I),
            Enc_ref = encode_varint_value(I),
            iolist_to_binary(Enc) =:= iolist_to_binary(Enc_ref)
        end
    ).


% check piqirun varint decoder over reference implementation
prop_varint_dec() ->
    ?FORALL(I, varint_range(),
        begin
            Enc = piqirun:encode_varint_value(I),
            Bin = iolist_to_binary(Enc),
            piqirun:decode_varint(Bin) =:= decode_varint(Bin)
        end
    ).


-endif. % PROPER
