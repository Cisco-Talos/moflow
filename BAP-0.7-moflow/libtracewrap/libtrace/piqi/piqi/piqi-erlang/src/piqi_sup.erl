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
%% @doc OTP supervisor behavior for Piqi
%%
-module(piqi_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([pick_piqi_server/0, force_pick_piqi_server/0, get_piqi_servers/0]).

% OTP supervisor callbacks
-export([init/1]).


-define(SUPERVISOR, ?MODULE).


start_link(WorkerPoolSize) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [WorkerPoolSize]).


% randomly pick a piqi tools Pid from the [cached] list of servers
pick_piqi_server() ->
    case get('piqi_servers') of
        'undefined' ->
            Pids = get_piqi_servers(),
            PidsTuple = list_to_tuple(Pids),
            put('piqi_servers', PidsTuple),
            do_pick_piqi_server(PidsTuple);
        PidsTuple ->
            do_pick_piqi_server(PidsTuple)
    end.


% randomly pick a piqi tools and refresh the cache
force_pick_piqi_server() ->
    erase('piqi_servers'),
    pick_piqi_server().


get_piqi_servers() ->
    L = supervisor:which_children(?SUPERVISOR),
    [ Child || {_Id,Child,_Type,_Modules} <- L, is_pid(Child) ].


do_pick_piqi_server(PidsTuple) ->
    Size = tuple_size(PidsTuple),
    true = Size > 0,
    Index0 = erlang:phash2({self(), now()}, Size),
    _Pid = element(Index0 + 1, PidsTuple).


%
% Supervisor callback
%

init([WorkerPoolSize]) ->
    % Piqi-Tools bindings for Erlang
    PiqiServers = [ piqi_server_spec(I) || I <- lists:seq(1, WorkerPoolSize) ],

    {ok, {{one_for_one, 10, 1}, PiqiServers}}.


piqi_server_spec(N) ->
    {{piqi_tools, N},
        {piqi_tools, start_link, []},
        permanent, 1000, worker,
        [piqi_tools]
    }.
