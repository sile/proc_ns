-module(proc_ns).

-export([
         %% start_name_server
         %% stop_name_server
         %% list_name_servers

         register_name/2,
         unregister_name/1,
         whereis_name/1,
         registered_names/1,
         send/2
        ]).

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
