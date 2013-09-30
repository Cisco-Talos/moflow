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
%% Unit-tests for Piqi Erlang Runtime library
%% 
-module(piqirun_tests).

-ifndef(DIALYZER).

-include_lib("eunit/include/eunit.hrl").


int_input() ->
  [0, 1, 2, 3, -1, -2, -3].


int32_input() ->
  MinInt = -16#80000000,
  MaxInt = 16#7fffffff,
  int_input() ++
  [
    MinInt, MinInt + 1, MinInt + 2, MinInt + 3,
    MaxInt, MaxInt - 1, MaxInt - 2, MaxInt - 3
  ].


int64_input() ->
  MinInt = -16#8000000000000000,
  MaxInt = 16#7fffffffffffffff,
  int32_input() ++
  [
    MinInt, MinInt + 1, MinInt + 2, MinInt + 3,
    MaxInt, MaxInt - 1, MaxInt - 2, MaxInt - 3
  ].


uint32_input() ->
  MaxInt = 16#ffffffff,
  [0, 1, 2, MaxInt, MaxInt - 1, MaxInt - 2, MaxInt - 3].


uint64_input() ->
  MaxInt = 16#ffffffffffffffff,
  uint32_input() ++
  [MaxInt, MaxInt - 1, MaxInt - 2, MaxInt - 3].


gen_test_type(X, Encoder, Decorder) ->
    ?_assert(
    begin
        Code = 12345,
        Iolist = piqirun:Encoder(Code, X),
        Bytes = iolist_to_binary(Iolist),
        [{Code, Value}] = piqirun:parse_record_buf(Bytes),
        X =:= piqirun:Decorder(Value)
    end).


signed_varint_test_() ->
    [ gen_test_type(X, integer_to_signed_varint, integer_of_signed_varint)
      || X <- int64_input() ].


zigzag_varint_test_() ->
    [ gen_test_type(X, integer_to_zigzag_varint, integer_of_zigzag_varint)
      || X <- int64_input() ].


varint_test_() ->
    [ gen_test_type(X, non_neg_integer_to_varint, non_neg_integer_of_varint)
      || X <- uint64_input() ].


signed_fixed32_test_() ->
    [ gen_test_type(X, integer_to_signed_fixed32, integer_of_signed_fixed32)
      || X <- int32_input() ].


fixed32_test_() ->
    [ gen_test_type(X, non_neg_integer_to_fixed32, non_neg_integer_of_fixed32)
      || X <- uint32_input() ].


signed_fixed64_test_() ->
    [ gen_test_type(X, integer_to_signed_fixed64, integer_of_signed_fixed64)
      || X <- int64_input() ].


fixed64_test_() ->
    [ gen_test_type(X, non_neg_integer_to_fixed64, non_neg_integer_of_fixed64)
      || X <- uint64_input() ].

-endif.
