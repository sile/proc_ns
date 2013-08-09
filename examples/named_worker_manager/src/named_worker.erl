-module(named_worker).

-behaviour(gen_server).

-export([
         start_link/1,
         stop/1,

         get_id/1
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
-define(PROC_NAME(Name), {via, proc_ns, {?NAME_SERVER, {single, Name}}}).

-record(state,
        {
          id :: term()
        }).

start_link(Id) ->
    gen_server:start_link(?PROC_NAME(Id), ?MODULE, [Id], []).

stop(ServerRef) ->
    gen_server:cast(ref_to_name(ServerRef), stop).

get_id(ServerRef) ->
    gen_server:call(ref_to_name(ServerRef), get_id).

ref_to_name(Ref) when is_pid(Ref) -> Ref;
ref_to_name(Ref)                  -> ?PROC_NAME(Ref).

init([Id]) ->
    {ok, #state{id = Id}}.

handle_call(get_id, _From, State) ->
    {reply, State#state.id, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
