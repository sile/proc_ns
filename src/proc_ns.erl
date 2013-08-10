-module(proc_ns).

-export([
         start_name_server/0, start_name_server/1,
         stop_name_server/1,
         list_name_servers/0,
         
         register_name/2,
         unregister_name/1,
         whereis_name/1,
         registered_names/1,
         send/2
        ]).

start_name_server() ->
    supervisor:start_child(proc_ns_server_sup, []).
    
start_name_server(ServerName) ->
    supervisor:start_child(proc_ns_server_sup, [ServerName]).

stop_name_server(ServerRef) when is_pid(ServerRef) ->
    supervisor:terminate_child(proc_ns_server_sup, ServerRef);
stop_name_server(ServerRef) ->
    case whereis(ServerRef) of
        undefined -> ok;
        Pid       -> stop_name_server(Pid)
    end.

list_name_servers() ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(proc_ns_server_sup)].

register_name({NameServerRef, NameSpec}, Pid) ->
    proc_ns_server:register_name(NameServerRef, NameSpec, Pid).

unregister_name({NameServerRef, NameSpec}) ->
    proc_ns_server:unregister_name(NameServerRef, NameSpec).

whereis_name({NameServerRef, Name}) ->
    proc_ns_server:whereis_name(NameServerRef, Name).

registered_names(NameServerRef) ->
    proc_ns_server:registered_names(NameServerRef).

send({NameServerRef, Name}, Msg) ->
    proc_ns_server:send(NameServerRef, Name, Msg).
