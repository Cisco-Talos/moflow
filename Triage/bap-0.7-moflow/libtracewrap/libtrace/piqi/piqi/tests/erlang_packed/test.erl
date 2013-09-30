-module(test).
-compile(export_all).


run() ->
    {ok, Bytes} = file:read_file("test-all.piq.pb"),
    R = packed_piqi:parse_r_all(Bytes),

    Iolist = packed_piqi:gen_r_all(R),
    ok = file:write_file("test-all.piq.pb.pb", iolist_to_binary(Iolist)),
    ok.

