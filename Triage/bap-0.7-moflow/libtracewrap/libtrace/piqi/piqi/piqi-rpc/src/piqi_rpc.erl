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
%% @doc Piqi-RPC high-level interface
%%
-module(piqi_rpc).

-export([start/0, stop/0, stop_all/0]).
-export([add_service/1, remove_service/1, get_services/0]).


-include("piqi_rpc.hrl").


% @doc start Piqi-RPC
start() ->
    ensure_started(piqi),
    ensure_started(inets), % inets is listed as a mochiweb dependency
    ensure_started(crypto),
    ensure_started(webmachine),
    ensure_started(mochiweb),
    application:start(piqi_rpc).


% @doc stop Piqi-RPC
stop() ->
    Res = application:stop(piqi_rpc),
    % NOTE: can't call this cleanup from piqi_rpc_app:stop() or :prep_stop(),
    % because of a dead-lock caused by the fact that piqi_rpc_http stores its
    % routes configuration in application environmnent, and deletion of an entry
    % from there needs to go through the application controller which is already
    % busy stopping the application
    catch piqi_rpc_http:cleanup(),
    Res.


% @doc stop Piqi-RPC and all dependencies that may have been started by start/0
stop_all() ->
    Res = stop(),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(piqi),
    Res.


ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.


-spec normalize_service_def/1 :: ( piqi_rpc_service_def() ) -> piqi_rpc_service().

normalize_service_def(RpcService = {_ImplMod, _RpcMod, _UrlPath, _Options}) ->
    RpcService;

normalize_service_def(_RpcServiceDef = {ImplMod, RpcMod, UrlPath}) ->
    _RpcService = {ImplMod, RpcMod, UrlPath, _Options = []}.


-spec add_service/1 :: ( piqi_rpc_service_def() ) -> ok.

add_service(RpcServiceDef) ->
    RpcService = normalize_service_def(RpcServiceDef),
    % atomic service addition to both piqi_rpc_monitor and piqi_rpc_http
    try
        % adding Piqi-RPC service first, then adding HTTP resource binding
        ok = piqi_rpc_monitor:add_service(RpcService),
        ok = piqi_rpc_http:add_service(RpcService),
        ServiceDefs = get_service_defs(),
        ok = set_service_defs([RpcService | ServiceDefs])
    catch
        Class:Reason ->
            catch remove_service(RpcServiceDef),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.


-spec remove_service/1 :: ( piqi_rpc_service_def() ) -> ok.

remove_service(RpcServiceDef) ->
    RpcService = normalize_service_def(RpcServiceDef),
    % removing HTTP resource binding first, then removing Piqi-RPC service
    ok = piqi_rpc_http:remove_service(RpcService),
    ok = piqi_rpc_monitor:remove_service(RpcService),
    ServiceDefs = get_service_defs(),
    ok = set_service_defs(ServiceDefs -- [RpcServiceDef]).


-spec get_services/0 :: () -> [piqi_rpc_service()].
get_services() ->
    [normalize_service_def(X) || X <- get_service_defs()].


-spec get_service_defs/0 :: () -> [piqi_rpc_service_def()].
get_service_defs() ->
    get_env('rpc_services').


-spec set_service_defs/1 :: ( [piqi_rpc_service_def()] ) -> ok.
% @hidden
set_service_defs(RpcServiceDefs) ->
    set_env('rpc_services', RpcServiceDefs).


% @hidden
get_env(Key) ->
    case application:get_env(piqi_rpc, Key) of
        {ok, X} -> X;
        'undefined' -> []
    end.


% @hidden
set_env(Key, Value) ->
    ok = application:set_env(piqi_rpc, Key, Value).

