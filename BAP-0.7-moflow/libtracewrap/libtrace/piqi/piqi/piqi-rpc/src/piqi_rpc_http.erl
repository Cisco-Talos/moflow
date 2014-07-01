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
%% @doc Piqi-RPC HTTP interface
%%
%% Currently it is implemented on top of Webmachine
%%

-module(piqi_rpc_http).

-export([start_link/0, cleanup/0]).
-export([add_service/1, remove_service/1]).
%-compile(export_all).


-include("piqi_rpc.hrl").


start_link() ->
    RpcServices = piqi_rpc:get_services(),
    Config = make_webmachine_config(RpcServices),
    % this function eventually calls mochiweb_http:start(Config) which starts
    % mochiweb_socket_server using gen_server:start_link()
    webmachine_mochiweb:start(Config).


make_webmachine_config(RpcServices) ->
    Config0 = get_env('http_server'),
    Config1 =
        case proplists:get_value('enable_access_logger', Config0) of
            true ->
                application:set_env(
                    webmachine, webmachine_logger_module, webmachine_logger),
                proplists:delete('enable_access_logger', Config0);
            _ ->
                application:unset_env(webmachine, webmachine_logger_module),
                Config0
        end,
    Dispatch = [ rpc_service_to_webmachine_route(X) || X <- RpcServices ],
    [ {dispatch, Dispatch} | Config1 ].


% @hidden
get_env(Key) ->
    case application:get_env(piqi_rpc, Key) of
        {ok, X} -> X;
        'undefined' -> []
    end.


% unregister all Piqi-RPC services from Webmachine when stopping Piqi-RPC
% applicaton (called from piqi_rpc_app:stop/1)
cleanup() ->
    RpcServices = piqi_rpc:get_services(),
    lists:foreach(fun remove_service/1, RpcServices).


-spec add_service/1 :: ( piqi_rpc_service() ) -> ok.

add_service(RpcService = {_ImplMod, _RpcMod, _UrlPath, _Options}) ->
    Route = rpc_service_to_webmachine_route(RpcService),
    ok = webmachine_router:add_route(Route).


-spec remove_service/1 :: ( piqi_rpc_service() ) -> ok.

remove_service(RpcService = {_ImplMod, _RpcMod, _UrlPath, _Options}) ->
    Route = rpc_service_to_webmachine_route(RpcService),
    ok = webmachine_router:remove_route(Route).


% see Webmachine documentation and webmachine_route.erl for format description.
-type webmachine_route() :: {_, _, _}.

-spec rpc_service_to_webmachine_route/1 :: (
    piqi_rpc_service() ) -> webmachine_route().

% @hidden
% make a Webmachine route from a Piqi-RPC service definition
rpc_service_to_webmachine_route(_RpcService = {ImplMod, RpcMod, UrlPath, Options}) ->
    % { PathPattern, WebmachineResource, InitArguments}
    PathElements = string:tokens(UrlPath, "/"),
    PathSpec = PathElements ++ ['*'],
    {PathSpec, piqi_rpc_webmachine_resource, [ImplMod, RpcMod, Options]}.

