-module(test).
-compile(export_all).


run() ->
    {ok, Bytes} = file:read_file("piqi.piqi.pb"),
    Piqi = piqi_piqi:parse_piqi(Bytes),

    IOList = piqi_piqi:gen_piqi(Piqi),
    ok = file:write_file("piqi.piqi.pb.pb", IOList),
    ok.

