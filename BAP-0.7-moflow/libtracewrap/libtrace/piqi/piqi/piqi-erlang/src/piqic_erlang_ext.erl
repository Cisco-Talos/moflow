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

%% Extended Piqi compiler for Erlang
%%
%% This module contains extended version of the Piqi compiler for Erlang. It
%% calls the base compiler ("piqic erlang") as a first step, and then generates
%% `<mod>_piqi_ext.erl` file that contains extended codecs.
%%
%% Extended codecs are capable for serializing/deserialyzing Erlang data
%% structures into several more data formats (Json, Piq, XML), in addition to
%% Google Protocol Buffers binary format which is the only data format supported
%% by the base codecs.

-module(piqic_erlang_ext).
-compile(export_all).

-include("piqi_piqi.hrl").


% TODO: check that the version of this plugin and "piqic version" are exactly
% the same


% Escript entry point
main(Args) ->
    % call piqic_erlang_ext with command-line arguments and ?MODULE as the
    % callback module.
    piqic_erlang_ext(?MODULE, Args).


usage() ->
    ScriptName = escript:script_name(),
    io:format(
"Usage: " ++ ScriptName ++ " [options] <.piqi file>\n"
"Options:
  -I <dir> add directory to the list of imported .piqi search paths
  --no-warnings don't print warnings
  --trace turn on tracing
  --debug <level> debug level; any number greater than 0 turns on debug messages
  --no-builtin-types don't include built-in type definitions
  -C <output directory> specify output directory
  --normalize <true|false> normalize identifiers (default: true)
  --gen-defaults generate default value constructors for generated types
  -help  Display this list of options
  --help  Display this list of options
"
    ).


% extract filename (last argument) and output directory (argument following -C)
parse_args([]) ->
    usage(),
    erlang:halt(1);

parse_args([X]) when X == "-help" orelse X == "--help" ->
    usage(),
    erlang:halt(0);

parse_args(Args) ->
    parse_args(Args, _Odir = 'undefined').


parse_args([Filename], Odir) ->
    {Filename, Odir};

parse_args(["-C", Odir |T], _) ->
    parse_args(T, Odir);

parse_args([_|T], Odir) ->
    parse_args(T, Odir).


piqic_erlang(Args, CustomArgs) ->
    PiqicErlang = lists:concat([
            piqi:get_command("piqic"), " erlang ", CustomArgs, join_args(Args)
        ]),
    command(PiqicErlang).


join_args(Args) ->
    Args1 = [escape_arg(X) || X <- Args],
    string:join(Args1, " ").


escape_arg(X) ->
    case lists:member($\ , X) of
        true -> "'" ++ X ++ "'";
        false -> X
    end.


set_cwd('undefined') -> ok;
set_cwd(Dir) ->
    ok = file:set_cwd(Dir).


% `Mod` parameter is the name of the Erlang module that exports two callback
% functions:
%
%       gen_piqi(Piqi)
%
%       custom_args()
%
piqic_erlang_ext(Mod, Args) ->
    {Filename, Odir} = parse_args(Args),

    % call the base compiler "piqic erlang"
    piqic_erlang(Args, Mod:custom_args()),

    ExpandedPiqi = Filename ++ ".expanded.pb",
    try
        PiqicExpand = lists:concat([
            piqi:get_command("piqic"),
            " expand --erlang -b -o ", ExpandedPiqi, " ", join_args(Args)
        ]),
        command(PiqicExpand),

        set_cwd(Odir),

        Piqi = read_piqi(ExpandedPiqi),

        Mod:gen_piqi(Piqi)
    after
        file:delete(ExpandedPiqi)
    end.


command(Cmd) ->
    %os:cmd(Cmd).
    case eunit_lib:command(Cmd) of
        {0, X} ->
            io:put_chars(X),
            ok;
        {_Code, Error} ->
            io:format("command \"~s\" failed with error: ~s~n", [Cmd, Error]),
            erlang:halt(1)
    end.


read_piqi(Filename) ->
    {ok, Bytes} = file:read_file(Filename),
    Buf = piqirun:init_from_binary(Bytes),
    Piqi = piqi_piqi:parse_piqi(Buf),
    Piqi.


%
% behavior-specific callbacks
%
custom_args() -> "--embed-piqi ".


gen_piqi(Piqi) ->
    Mod = Piqi#piqi.module,
    ErlMod = Piqi#piqi.erlang_module,
    Defs = Piqi#piqi.typedef,
    Functions = Piqi#piqi.func,

    Filename = binary_to_list(ErlMod) ++ "_ext.erl",

    Code = iod("\n\n", [
        [
            "-module(", ErlMod, "_ext).\n",
            "-compile(export_all).\n"
        ],
        gen_embedded_piqi(ErlMod),
        [ gen_parse_def(Mod, ErlMod, X) || X <- Defs ],
        [ gen_gen_def(Mod, ErlMod, X) || X <- Defs ],
        [ gen_parse_func(Mod, ErlMod, X) || X <- Functions ],
        [ gen_gen_func(Mod, ErlMod, X) || X <- Functions ]
    ]),
    ok = file:write_file(Filename, Code).


gen_embedded_piqi(ErlMod) ->
    [
        "piqi() ->\n",
        "    ", ErlMod, ":piqi().\n"
    ].


typedef_name({piqi_record, X}) -> X#piqi_record.name;
typedef_name({variant, X}) -> X#variant.name;
typedef_name({enum, X}) -> X#enum.name;
typedef_name({alias, X}) -> X#alias.name;
typedef_name({piqi_list, X}) -> X#piqi_list.name.


typedef_erlname({piqi_record, X}) -> X#piqi_record.erlang_name;
typedef_erlname({variant, X}) -> X#variant.erlang_name;
typedef_erlname({enum, X}) -> X#enum.erlang_name;
typedef_erlname({alias, X}) -> X#alias.erlang_name;
typedef_erlname({piqi_list, X}) -> X#piqi_list.erlang_name.


gen_parse(Mod, ErlMod, Name, ErlName) ->
    [
        gen_parse_2(Mod, ErlMod, Name, ErlName),
        gen_parse_3(Mod, ErlMod, Name, ErlName)
    ].


gen_parse_2(Mod, ErlMod, Name, ErlName) ->
    [
        "parse_", ErlName, "(X, Format) ->\n",
        "    ", ErlMod, ":parse_", ErlName, "(\n",
        "        ", gen_convert(Mod, Name, "Format", "'pb'", "X"), ").\n\n"
    ].


gen_parse_3(Mod, ErlMod, Name, ErlName) ->
    [
        "parse_", ErlName, "(X, Format, Options) ->\n",
        "    ", ErlMod, ":parse_", ErlName, "(\n",
        "        ", gen_convert(Mod, Name, "Format", "'pb'", "X, Options"), ").\n\n"
    ].


gen_gen(Mod, ErlMod, Name, ErlName) ->
    [
        gen_gen_2(Mod, ErlMod, Name, ErlName),
        gen_gen_3(Mod, ErlMod, Name, ErlName)
    ].


gen_gen_2(Mod, ErlMod, Name, ErlName) ->
    [
        "gen_", ErlName, "(X, Format) ->\n",
        "    Iolist = ", ErlMod, ":gen_", ErlName, "(X),\n",
        "    ", gen_convert(Mod, Name, "'pb'", "Format", "iolist_to_binary(Iolist)"), ".\n\n"
    ].

gen_gen_3(Mod, ErlMod, Name, ErlName) ->
    [
        "gen_", ErlName, "(X, Format, Options) ->\n",
        "    Iolist = ", ErlMod, ":gen_", ErlName, "(X),\n",
        "    ", gen_convert(Mod, Name, "'pb'", "Format", "iolist_to_binary(Iolist), Options"), ".\n\n"
    ].


gen_convert(Mod, Name, InputFormat, OutputFormat, Data) ->
    ScopedName = [ Mod, "/", Name ],
    [
        "piqirun_ext:convert(?MODULE, ",
        iod(", ", [
            ["<<\"", ScopedName, "\">>"], InputFormat, OutputFormat, Data
        ]),
        ")"
    ].


gen_parse_def(Mod, ErlMod, Def) ->
    Name = typedef_name(Def),
    ErlName = typedef_erlname(Def),
    gen_parse(Mod, ErlMod, Name, ErlName).


gen_gen_def(Mod, ErlMod, Def) ->
    Name = typedef_name(Def),
    ErlName = typedef_erlname(Def),
    gen_gen(Mod, ErlMod, Name, ErlName).


gen_parse_func(Mod, ErlMod, Func) ->
    [ gen_parse(Mod, ErlMod, Name, ErlName) || {Name, ErlName} <- make_params(Func) ].


gen_gen_func(Mod, ErlMod, Func) ->
    [ gen_gen(Mod, ErlMod, Name, ErlName) || {Name, ErlName} <- make_params(Func) ].


make_params(Func) ->
    Name = Func#func.name,
    ErlName = Func#func.erlang_name,
    [
        make_param_names(Name, ErlName, Func#func.input, "input"),
        make_param_names(Name, ErlName, Func#func.output, "output"),
        make_param_names(Name, ErlName, Func#func.error, "error")
    ].


make_param_names(_Name, _ErlName, 'undefined', _ParamType) -> 'undefined';
make_param_names(Name, ErlName, _Param, ParamType) ->
    {[Name, "-", ParamType], [ErlName, "_", ParamType]}.


iod(_Delim, []) -> [];
iod(Delim, [H|T]) ->
    lists:foldl(fun (X, Accu) -> [Accu, Delim, X] end, H, T).

