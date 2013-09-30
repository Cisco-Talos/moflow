%
% runtime support for Erlang custom types defined in example.piqi
%
-module(piqirun_custom).

-export_type([bigint/0]).
-export([bigint_to_string/1, bigint_of_string/1]).

-export_type([term_t/0]).
-export([term_t_to_binary/1, term_t_of_binary/1]).


-type bigint() :: integer().
-type term_t() :: any().


-spec bigint_of_string/1 :: (string() | binary()) -> integer().
-spec bigint_to_string/1 :: (integer()) -> list().


bigint_of_string(X) when is_list(X) -> list_to_integer(X);
bigint_of_string(X) when is_binary(X) ->
  bigint_of_string(binary_to_list(X)).

bigint_to_string(X) -> integer_to_list(X).


-spec term_t_of_binary/1 :: (binary()) -> any().
-spec term_t_to_binary/1 :: (any()) -> binary().

term_t_of_binary(X) -> binary_to_term(X).

term_t_to_binary(X) -> term_to_binary(X).

