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

%
% Header file for Piqi-RPC
%
-ifndef(__PIQIRUN_HRL__).
-define(__PIQI_RPC_HRL__, 1).

-include_lib("piqi/include/piqi_tools.hrl").


% Piqi-RPC service
-type piqi_rpc_service() :: {
    ImplMod :: atom(),
    RpcMod :: atom(),
    UrlPath :: string(),
    Options :: piqi_rpc_options() }.


% Piqi-RPC service definition; see `piqi_rpc.app.src` file for details.
-type piqi_rpc_service_def() ::
     {ImplMod :: atom(), RpcMod :: atom(), UrlPath :: string()}
    | piqi_rpc_service().


% Piqi-RPC options that can be applied at service-level as well as globally; see
% `piqi_rpc.app.src` file for details.
-type piqi_rpc_option() ::
      piqi_convert_option()
    | 'omit_internal_error_details' | {'omit_internal_error_details', boolean()}.


-type piqi_rpc_options() :: [ piqi_rpc_option() ].


-endif.
