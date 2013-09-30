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
%% @doc OTP supervisor behavior for Piqi-RPC 
%%
-module(piqi_rpc_sup).

-behaviour(supervisor).

-export([start_link/0]).
% OTP supervisor callbacks
-export([init/1]).


-define(SUPERVISOR, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).


%
% Supervisor callback
%

init(_Args) ->
    PiqiRpcMonitor =
        {piqi_rpc_monitor,
            {piqi_rpc_monitor, start_link, []},
            permanent, 1000, worker,
            [piqi_rpc_monitor]
        },

    PiqiRpcMochiweb =
        {piqi_rpc_http, % Piqi-RPC http server
            {piqi_rpc_http, start_link, []},
            permanent, 5000, worker,
            dynamic % XXX
        },

    {ok, {{one_for_one, 1, 60}, [PiqiRpcMonitor, PiqiRpcMochiweb]}}.

