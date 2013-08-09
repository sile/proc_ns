-module(named_worker_manager).

-behaviour(gen_server).

-export([
         start_link/0,
         stop/0,

         start_worker/1,
         stop_worker/1,
         find_worker/1,
         list_workers/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(NAME_SERVER, named_worker_manager_ns).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

start_worker(WorkerId) ->
    supervisor:start_child(named_worker_sup, [WorkerId]).

stop_worker(WorkerId) ->
    case proc_ns:whereis_name({?NAME_SERVER, WorkerId}) of
        undefined -> ok;
        Pid       -> supervisor:terminate_child(named_worker_sup, Pid)
    end.

find_worker(WorkerId) ->
    proc_ns:whereis_name({?NAME_SERVER, WorkerId}).

list_workers() ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(named_worker_sup)].

init([]) ->
    process_flag(trap_exit, true),
    case proc_ns:start_name_server(?NAME_SERVER) of
        {error, Reason} -> {stop, Reason};
        {ok, _Pid}      -> {ok, []}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = proc_ns:stop_name_server(?NAME_SERVER).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


