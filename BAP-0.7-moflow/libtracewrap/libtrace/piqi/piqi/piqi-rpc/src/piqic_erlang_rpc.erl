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

-module(piqic_erlang_rpc).
-compile(export_all).

-include_lib("piqi/include/piqi_piqi.hrl").


main(Args) ->
    % call piqic_erlang_ext with command-line arguments and ?MODULE as the
    % callback module.
    piqic_erlang_ext:piqic_erlang_ext(?MODULE, Args).


%
% Callback functions: custom_args() & gen_piqi(Piqi)
%

custom_args() ->
    "--embed-piqi --gen-defaults ".


gen_piqi(Piqi) ->
    gen_rpc_erl(Piqi),
    gen_impl_hrl(Piqi),
    gen_default_impl_erl(Piqi),
    ok.

%
% Generating Piqi-RCP server stubs: <ErlMod>_rpc.erl
%

gen_rpc_erl(Piqi) ->
    Mod = Piqi#piqi.module,
    ErlMod = Piqi#piqi.erlang_module,
    FuncList = Piqi#piqi.func,

    Filename = binary_to_list(ErlMod) ++ "_rpc.erl",

    Code = iod("\n\n", [
        [
            "-module(", ErlMod, "_rpc).\n",
            "-compile(export_all).\n"
        ],
        piqic_erlang_ext:gen_embedded_piqi(ErlMod),
        gen_get_piqi(ErlMod),
        gen_server_stubs(Mod, ErlMod, FuncList)
    ]),
    ok = file:write_file(Filename, Code).


gen_get_piqi(_ErlMod) ->
    [
        "get_piqi(OutputFormat, Options) ->\n",
        "    piqi_rpc_runtime:get_piqi(piqi(), OutputFormat, Options).\n"
    ].


gen_server_stubs(Mod, ErlMod, FuncList) ->
    FuncClauses = [ gen_func_clause(X, Mod, ErlMod) || X <- FuncList ],
    [
        "rpc(Mod, Name, InputData, _InputFormat, _OutputFormat, Options) ->\n",
        "    try\n",
        "    case Name of\n",
            iod("\n", FuncClauses), "\n",
            gen_default_clause(),
        "    end\n",
        "    catch\n",
        "        Class:Reason -> piqi_rpc_runtime:handle_runtime_exception(Class, Reason, Options)\n",
        "    end.\n"
    ].


gen_default_clause() ->
    [
"        _ ->\n",
"            piqi_rpc_runtime:handle_unknown_function()\n"
    ].


gen_func_clause(F, Mod, ErlMod) ->
    Name = F#func.name,
    ErlName = F#func.erlang_name,
    ScopedName = [ Mod, "/", Name ],
    InputCode =
        case F#func.input of
            'undefined' -> % the function doesn't have input
                [
"            piqi_rpc_runtime:check_empty_input(InputData),\n",
"            case piqi_rpc_runtime:call(Mod, ",  ErlName, ", 'undefined') of\n"
                ];
            _ ->
                [
"            Input = piqi_rpc_runtime:decode_input(?MODULE, fun ", ErlMod, ":parse_", ErlName, "_input/1, <<\"", ScopedName, "-input\">>, _InputFormat, InputData, Options),\n"
"            case piqi_rpc_runtime:call(Mod, ", ErlName, ", Input) of\n"
                ]
        end,

    OutputCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce output
"                ok -> ok";
            _ ->
                [
"                {ok, Output} ->\n"
"                    piqi_rpc_runtime:encode_output(?MODULE, fun ", ErlMod, ":gen_", ErlName, "_output/1, <<\"", ScopedName, "-output\">>, _OutputFormat, Output, Options)"
                ]
        end,

    ErrorCode =
        case F#func.error of
            'undefined' -> % the function doesn't produce errors
                [];
            _ ->
                [[
"                {error, Error} ->\n"
"                    piqi_rpc_runtime:encode_error(?MODULE, fun ", ErlMod, ":gen_", ErlName, "_error/1, <<\"", ScopedName, "-error\">>, _OutputFormat, Error, Options)"
                ]]
        end,

    DefaultCaseCode = [
"                X -> piqi_rpc_runtime:handle_invalid_result(Name, X)"
    ],

    Code = [
"        <<\"", Name, "\">> ->\n",
                InputCode,
                iod(";\n", [OutputCode] ++ ErrorCode ++ [DefaultCaseCode]),
                "\n",
"            end;\n"
    ],
    Code.


iod(_Delim, []) -> [];
iod(Delim, [H|T]) ->
    lists:foldl(fun (X, Accu) -> [Accu, Delim, X] end, H, T).


%
% Generating Piqi-RCP function specs: <ErlMod>_impl.hrl
%

gen_impl_hrl(Piqi) ->
    ErlMod = Piqi#piqi.erlang_module,
    ErlTypePrefix = Piqi#piqi.erlang_type_prefix,
    FuncList = Piqi#piqi.func,

    Filename = binary_to_list(ErlMod) ++ "_impl.hrl",

    ErlModStr = binary_to_list(ErlMod),
    HeaderMacro = ["__", string:to_upper(ErlModStr), "_IMPL_HRL__" ],
    Code =
        [
            "-ifndef(", HeaderMacro, ").\n"
            "-define(", HeaderMacro, ", 1).\n\n"
            "-include(\"", ErlMod, ".hrl\").\n\n",

            gen_function_specs(ErlTypePrefix, FuncList),

            "-endif.\n"
        ],
    ok = file:write_file(Filename, iolist_to_binary(Code)).


gen_function_specs(ErlTypePrefix, FuncList) ->
    [ gen_function_spec(ErlTypePrefix, X) || X <- FuncList ].


gen_function_spec(ErlTypePrefix, F) ->
    ErlName = F#func.erlang_name,
    Input =
        case F#func.input of
            'undefined' -> "'undefined'";
            _ -> [ ErlTypePrefix, ErlName, "_input()" ]
        end,

    Output =
        case F#func.output of
            'undefined' -> "ok";
            _ -> [ "{ok, ", ErlTypePrefix, ErlName, "_output()}" ]
        end,

    Error =
        case F#func.error of
            'undefined' -> "";
            _ -> [ " | {error, ",  ErlTypePrefix, ErlName, "_error()}" ]
        end,

    [ "-spec ", ErlName, "/1 :: (", Input, ") -> ", Output, Error, ".\n\n" ].


%
% Generating Piqi-RCP default implementation: <ErlMod>_default_impl.erl
%

gen_default_impl_erl(Piqi) ->
    ErlMod = Piqi#piqi.erlang_module,
    FuncList = Piqi#piqi.func,

    Filename = binary_to_list(ErlMod) ++ "_default_impl.erl",

    Code =
        [
            "-module(", ErlMod, "_default_impl).\n"
            "-compile(export_all).\n\n"
            "-include(\"", ErlMod, "_impl.hrl\").\n\n",

            gen_default_impls(ErlMod, FuncList)
        ],
    ok = file:write_file(Filename, iolist_to_binary(Code)).


gen_default_impls(ErlMod, FuncList) ->
    [ gen_default_impl(ErlMod, X) || X <- FuncList ].


gen_default_impl(ErlMod, F) ->
    ErlName = F#func.erlang_name,
    Input =
        case F#func.input of
            'undefined' -> "'undefined'";
            _ -> "_Input"
        end,

    Output =
        case F#func.output of
            'undefined' -> "ok";
            _ -> [ "{ok, ", ErlMod, ":default_", ErlName, "_output()}" ]
        end,

    [ ErlName, "(", Input, ") -> ", Output, ".\n\n" ].

