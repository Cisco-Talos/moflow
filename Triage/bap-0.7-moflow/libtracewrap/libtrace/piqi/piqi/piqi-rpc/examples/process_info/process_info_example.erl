-module(process_info_example).
-compile(export_all).

-include("process_info_piqi_impl.hrl").


list_processes('undefined') ->
    L = erlang:processes(),
    Res = [pid_to_list(X) || X <- L],
    {ok, Res}.


process_info(Input) ->
    try do_process_info(Input)
    catch
        {error, _} = Error -> Error
    end.


do_process_info(Input) ->
    PidBin = Input#process_info_input.pid,
    Pid =
        try
            list_to_pid(binary_to_list(PidBin))
        catch _:badarg ->
            throw({error, "invalid Pid format: " ++ PidBin})
        end,

    case erlang:process_info(Pid) of
        'undefined' ->
            {error, "no such process: " ++ PidBin};
        I ->
            Res = encode_process_info(Pid, I),
            io:format("~p~n", [I]),
            {ok, Res}
    end.


encode_process_info(Pid, L) ->
    #process_info{
        pid = str(Pid),
        registered_name = encode_reg_name(v(registered_name, L)),
        current_function = encode_func(v(current_function, L)),
        initial_call = encode_func(v(initial_call, L)),
        status = v(status, L), % atom

        message_queue_len = v(message_queue_len, L), % int

        messages = encode_messages(v(messages, L)),

        links = encode_links(v(links, L)),

        dictionary = encode_dictionary(v(dictionary, L)),

        trap_exit = v(trap_exit, L), % bool
        error_handler = get_str(error_handler, L), % atom
        priority = v(priority, L), % atom
        group_leader = get_str(group_leader, L), % pid
        total_heap_size = v(total_heap_size, L), % int
        heap_size = v(heap_size, L), % int
        stack_size = v(stack_size, L), % int
        reductions = v(reductions, L), % int

        suspending = encode_suspendee_list(v(suspending, L))
    }.


v(Key, List) ->
    proplists:get_value(Key, List).


get_str(K, L) ->
    V = v(K, L),
    str(V).


% for some reason process_info(Pid, registered_name) returns [] instead of
% undefined so handling it separately.
encode_reg_name([]) -> 'undefined';
encode_reg_name(X) -> str(X).


str('undefined') -> 'undefined';
str(X) -> s(X).


s(X) when is_atom(X) -> atom_to_list(X);
s(X) when is_pid(X) -> pid_to_list(X);
s(X) ->
    lists:flatten(io_lib:format("~w", [X])).


encode_func('undefined') -> 'undefined';
encode_func({M, F, A}) ->
    lists:concat([M, ":", F, "/", A]).


encode_links('undefined') -> 'undefined';
encode_links(L) ->
    [s(X) || X <- L].


encode_dictionary('undefined') -> 'undefined';
encode_dictionary(L) ->
    [ #dictionary_item{key = s(K), value = s(V)} || {K, V} <- L ].


encode_messages('undefined') -> 'undefined';
encode_messages(L) ->
    [ s(X) || X <- L].


encode_suspendee_list('undefined') -> 'undefined';
encode_suspendee_list(L) ->
    [ encode_suspendee(X) || X <- L ].


encode_suspendee({Suspendee, ActiveSuspendCount, OutstandingSuspendCount}) ->
    #suspendee{
        suspendee = Suspendee,
        active_suspend_count = ActiveSuspendCount,
        outstanding_suspend_count = OutstandingSuspendCount
    }.


list_process_info(Properties0) ->
    L = erlang:processes(),

    Properties =
        case lists:member(all, Properties0) of
            true ->
                % NOTE: this list doesn't include the 'messages' and
                % 'dictionary' properties
                DefaultProps = [
                    registered_name, current_function, initial_call, status,
                    message_queue_len, links, trap_exit, error_handler, priority,
                    group_leader, total_heap_size, heap_size, stack_size,
                    reductions
                ],
                lists:usort((Properties0 -- [all]) ++ DefaultProps);
            false ->
                Properties0
        end,
    Res = [ make_process(X, Properties) || X <- L],
    {ok, Res}.


make_process(Pid, Properties) ->
    I = erlang:process_info(Pid, Properties),
    encode_process_info(Pid, I).

