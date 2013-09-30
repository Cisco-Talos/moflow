-module(test).
-compile(export_all).

-include("addressbook_piqi.hrl").
-include("piqi_obj_piqi.hrl").


main() ->
    run(1).


main([NParallel]) ->
    Factor = list_to_integer(NParallel),
    run(Factor).


run(Factor) ->
    put(parallel_factor, Factor),

    test_piqi_server(),
    test_addressbook(),
    test_piqi(),
    ok.


test_piqi_server() ->
    io:format("*** testing piqi_tools:ping() i.e. 'piqi server' roundtrip ***~n~n"),
    N = 500000,
    F = fun () -> piqi_tools:ping() end,
    test(F, N).


test_addressbook() ->
    io:format("*** testing Erlang serialization of medium objects ***~n~n"),
    Filename = "addressbook.piq.pb",

    % Read the addressbook encoded in Protobuf format
    {ok, Bytes} = file:read_file(Filename),

    Reader = fun addressbook_piqi_ext:parse_address_book/2,
    Writer = fun addressbook_piqi_ext:gen_address_book/2,

    N = 100000,

    %test_rw(Reader, Writer, 'pb', Bytes, N),
    %test_rw(Reader, Writer, 'json', Bytes, N),
    %test_rw(Reader, Writer, 'json_pretty', Bytes, N),
    %test_rw(Reader, Writer, 'xml', Bytes, N),
    %test_rw(Reader, Writer, 'xml_pretty', Bytes, N),
    %test_rw(Reader, Writer, 'piq', Bytes, N),

    test_rw_all(Reader, Writer, Bytes, N),

    ok.


test_piqi() ->
    io:format("*** testing Erlang serialization of big objects ***~n~n"),
    Filename = "piqi.piq.pb",

    % Read the Piqi-self specification encoded in Protobuf format
    {ok, Bytes} = file:read_file(Filename),

    Reader = fun piqi_obj_piqi_ext:parse_piqi/2,
    Writer = fun piqi_obj_piqi_ext:gen_piqi/2,

    N = 20000,

    %test_rw(Reader, Writer, 'pb', Bytes, N),
    %test_rw(Reader, Writer, 'json', Bytes, N),
    %test_rw(Reader, Writer, 'xml', Bytes, N),
    %test_rw(Reader, Writer, 'piq', Bytes, N),

    test_rw_all(Reader, Writer, Bytes, N),

    ok.


test_rw_all(Reader, Writer, Bytes, N) ->
    Formats = ['pb', 'json', 'json_pretty', 'xml', 'xml_pretty', 'piq'],
    lists:foreach(fun (X) -> test_rw(Reader, Writer, X, Bytes, N) end, Formats).


test_rw(Reader, Writer, Format, Bytes, N) ->
    io:format("size of Protobuf binary: ~w~n", [size(Bytes)]),

    % read the object into Erlang term representation
    Output = Reader(Bytes, 'pb'),

    % write the object into desired test input format
    Input = Writer(Output, Format),
    %io:format("input: ~p~n", [Input]),

    InputFormat =
        case Format of
            'json_pretty' -> 'json';
            'xml_pretty' -> 'xml';
            F -> F
        end,

    io:format("reading ~w objects...~n", [Format]),
    IRate = test_convert(Reader, InputFormat, Input, N),
    io:format("writing ~w objects...~n", [Format]),
    ORate = test_convert(Writer, Format, Output, N),
    io:format("~w read/write rate: ~w/~w~n~n", [Format, IRate, ORate]),
    ok.


test_convert(Codec, Format, Input, N) ->
    Fun = fun () -> Codec(Input, Format) end,
    test(Fun, N).


test(Fun, N) ->
    io:format("count: ~w~n", [N]),
    {Time, _} = timer:tc(?MODULE, repeat_n, [Fun, N]),

    Seconds = Time / 1000000,
    PerSecond = (N * 1000000) div Time,

    io:format("time: ~w seconds~n", [Seconds]),
    io:format("rate: ~w calls per second~n~n", [PerSecond]),

    PerSecond.


repeat_n(Fun, N) ->
    Factor = get(parallel_factor),
    io:format("parallel_factor: ~w~n", [Factor]),
    repeat_n(Factor, Fun, N).


repeat_n(Factor, Fun, N) ->
    SpawnFun =
        fun () ->
            spawn(?MODULE, repeat_spawned, [self(), Fun, N div Factor]),
            %timer:sleep(1), % sleep for 1 millisecond
            ok
        end,
    WaitFun =
        fun () ->
            receive done -> ok end
        end,
    do_n(SpawnFun, Factor),
    do_n(WaitFun, Factor).


do_n(_Fun, 0) -> ok;
do_n(Fun, N) ->
    Fun(),
    do_n(Fun, N-1).


repeat_spawned(Parent, Fun, N) ->
    init_time(N),
    repeat(Fun, N),
    Parent ! done.


repeat(_Fun, 0) -> ok;
repeat(Fun, N) ->

    T1 = now(),
    Fun(),
    T2 = now(),
    update_time(T1, T2, N),

    repeat(Fun, N-1).


init_time(N) ->
    reset_time(N, now()).


reset_time(N, Now) ->
    put(min_time, 1000000000000),
    put(max_time, 0),
    put(prev_now, Now),
    put(prev_count, N + 1),
    ok.


update_time(T1, T2 = Now, Count) ->

    T = timer:now_diff(T2, T1),

    case T > get(max_time) of
        true -> put(max_time, T);
        false -> ok
    end,
    case T < get(min_time) of
        true -> put(min_time, T);
        false -> ok
    end,

    TimeDiff = timer:now_diff(Now, get(prev_now)),
    case TimeDiff > 3 * 1000000 of
        true ->
            CountDiff = get(prev_count) - Count,
            io:format("min: ~w, max: ~w, avg: ~.1f, count: ~w~n", [
                get(min_time), get(max_time),
                TimeDiff / CountDiff,
                CountDiff
            ]),
            reset_time(Count, Now);
        false -> ok
    end.
