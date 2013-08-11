-module(proc_ns_simple_entity_manager).

-behaviour(gen_server).

-export([
         start_link/1, start_link/2,
         start/1, start/2,
         stop/1,
         
         reserve_entity/3,
         reserve_multi_id_entity/3,
         release_entity/2,
         find_entity/2,
         which_entities/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          name_server :: pid(),
          supervisor  :: atom() | pid() % TODO
        }).

start_link(EntityManagerName, EntitySupervisorRef) ->
    gen_server:start_link({local, EntityManagerName}, ?MODULE, [EntitySupervisorRef], []).

start_link(EntitySupervisorRef) ->
    gen_server:start_link(?MODULE, [EntitySupervisorRef], []).

start(EntityManagerName, EntitySupervisorRef) ->
    gen_server:start({local, EntityManagerName}, ?MODULE, [EntitySupervisorRef], []).

start(EntitySupervisorRef) ->
    gen_server:start(?MODULE, [EntitySupervisorRef], []).

stop(EntityManagerRef) ->
    gen_server:cast(EntityManagerRef, stop).

reserve_entity(EntityManagerRef, EntityId, Args) ->
    gen_server:call(EntityManagerRef, {reserve_entity, {single, EntityId}, Args}).

reserve_multi_id_entity(EntityManagerRef, EntityIdList, Args) ->
    gen_server:call(EntityManagerRef, {reserve_entity, {multi, EntityIdList}, Args}).

release_entity(EntityManagerRef, EntityRef) when is_pid(EntityRef) ->
    gen_server:cast(EntityManagerRef, {release_entity, EntityRef});
release_entity(EntityManagerRef, EntityRef) ->
    case find_entity(EntityManagerRef, EntityRef) of
        {error, not_found} -> ok;
        {ok, Pid}          -> release_entity(EntityManagerRef, Pid)
    end.

find_entity(EntityManagerRef, EntityId) ->
    NameServer = gen_server:call(EntityManagerRef, get_name_server),
    case proc_ns:whereis_name({NameServer, EntityId}) of
        undefined -> {error, not_found};
        Pid       -> {ok, Pid}
    end.

which_entities(EntityManagerRef) ->
    Supervisor = gen_server:call(EntityManagerRef, get_supervisor),
    [Pid || {_, Pid, _, _} <- supervisor:which_children(Supervisor)].

init([SupervisorRef]) ->
    process_flag(trap_exit, true),
    case proc_ns:start_name_server() of
        {error, Reason} -> {stop, Reason};
        {ok, Pid}       ->
            case SupervisorRef of
                {ParentSupervisorPid, EntitySupervisorId} ->
                    gen_server:cast(self(), {resolve, ParentSupervisorPid, EntitySupervisorId}),
                    {ok, #?STATE{name_server = Pid, supervisor = undefined}};
                _ ->
                    {ok, #?STATE{name_server = Pid, supervisor = SupervisorRef}}
            end
    end.

handle_call({reserve_entity, EntityIdSpec, Args}, _From, State) ->
    #?STATE{name_server = NameServer, supervisor = Supervisor} = State,
    %% TODO: error-handling
    EntityId = {via, proc_ns, {NameServer, EntityIdSpec}},
    Result = 
        case supervisor:start_child(Supervisor, [EntityId | Args]) of
            {error, {already_started, Pid}} -> {error, {already_reserved, Pid}};
            Other                           -> Other
        end,
    {reply, Result, State};
handle_call(get_supervisor, _From, State) ->
    {reply, State#?STATE.supervisor, State};
handle_call(get_name_server, _From, State) ->
    {reply, State#?STATE.name_server, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({release_entity, EntityPid}, State) ->
    #?STATE{supervisor = Supervisor} = State,
    _ = supervisor:terminate_child(Supervisor, EntityPid),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({resolve, ParentSupervisorPid, EntitySupervisorId}, State) ->
    Children = supervisor:which_children(ParentSupervisorPid),
    case lists:keyfind(EntitySupervisorId, 1, Children) of
        false ->
            Reason = {entity_supervisor_no_exists, [{id , EntitySupervisorId}, {parent, ParentSupervisorPid}, {children, Children}]},
            {stop, {error, Reason}, State};
        {_, Pid, _, _} ->
            {noreply, State#?STATE{supervisor = Pid}}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok = proc_ns:stop_name_server(State#?STATE.name_server).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
