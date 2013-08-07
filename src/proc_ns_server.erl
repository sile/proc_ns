-module(proc_ns_server).

-behaviour(gen_server).

-export([
         start_link/1,

         register_name/3,
         unregister_name/2,
         whereis_name/2,
         registered_names/1,
         send/3
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
          server_name  :: atom(),
          name_to_pid  :: dict(),
          pid_to_names :: dict()
        }).

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [ServerName], []).

register_name(NameServerRef, NameSpec, Pid) when is_pid(Pid) ->
    case NameSpec of
        {single, _} -> ok;
        {multi, _}  -> ok;
        _           -> exit(badarg)
    end,
    gen_server:call(NameServerRef, {register_name, NameSpec, Pid}).

unregister_name(NameServerRef, NameSpec) ->
    case NameSpec of
        {single, _} -> ok;
        {multi, _}  -> ok;
        _           -> exit(badarg)
    end,
    gen_server:cast(NameServerRef, {unregister_name, NameSpec}).

whereis_name(NameServerRef, Name) ->
    gen_server:call(NameServerRef, {whereis_name, Name}).

registered_names(NameServerRef) ->
    gen_server:call(NameServerRef, registered_names).

send(NameServerRef, Name, Msg) ->
    case whereis_name(NameServerRef, Name) of
        undefined -> exit({badarg, {{NameServerRef, Name}, Msg}});
        Pid       -> Pid ! Msg
    end.

init([ServerName]) ->
    State = #?STATE{
                server_name  = ServerName,
                name_to_pid  = dict:new(),
                pid_to_names = dict:new()
            },
    {ok, State}.

handle_call({register_name, NameSpec, Pid}, _From, State) ->
    {Result, State2} = do_register_name(NameSpec, Pid, State),
    {reply, Result, State2};
handle_call({whereis_name, Name}, _From, State) ->
    Result = do_whereis_name(Name, State),
    {reply, Result, State};
handle_call(registered_names, _From, State) ->
    Result = do_registered_names(State),
    {reply, Result, State}.

handle_cast({unregister_name, NameSpec}, State) ->
    State2 = do_unregister_name(NameSpec, State),
    {noreply, State2}.

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, State) ->
    State2 = do_unregister_name_by_pid(Pid, State),
    {noreply, State2}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_register_name({single, Name}, Pid, State) ->
    do_register_names([Name], Pid, State);
do_register_name({multi, Names}, Pid, State) ->
    do_register_names(Names, Pid, State).

do_register_names(Names, Pid, State) ->
    Result =
        lists:foldl(fun (_Name, error) ->
                            error;
                        (Name, AccNameToPid) ->
                            case dict:find(Name, AccNameToPid) of
                                {ok, _} -> error;
                                error   -> dict:store(Name, Pid, AccNameToPid)
                            end
                    end,
                    State#?STATE.name_to_pid,
                    Names),
    case Result of
        error     -> {no, State};
        NameToPid ->
            PidToNames = dict:append_list(Pid, Names, State#?STATE.pid_to_names),
            {yes, State#?STATE{name_to_pid = NameToPid, pid_to_names = PidToNames}}
    end.

            
            
                    
