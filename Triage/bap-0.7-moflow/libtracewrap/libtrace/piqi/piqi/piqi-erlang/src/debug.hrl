-ifndef(__PIQI_DEBUG_HRL__).
-define(__PIQI_DEBUG_HRL__, 1).


-ifdef(DEBUG).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-else.
-define(PRINT(Var), ok).
-endif.


-endif.
