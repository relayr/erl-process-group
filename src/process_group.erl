%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2012
%% @version 1.0
%% @doc Process for managing of a process group.
%% @end
%%------------------------------------------------------------------------------
-module(process_group).
-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include("../include/process_group.hrl").

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
	create/1,
	delete/1,
	join/1,
	join/2,
	leave/1,
	leave/2,
	get_members/1,
	notify_members/2,
	which_groups/0,
	
	start_link/1
]).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------
-export([
		init/1,
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2,
		code_change/3
	]).

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------
-record(process_group_state, {
	name		:: atom(),
	table_id	:: dict()
}).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(SUPERVISOR,	process_group_supervisor).

%% =============================================================================
%% Exported functions
%% =============================================================================

%% @doc Start new instance of process_group gen_server.
-spec start_link(GroupName :: atom()) -> {ok, PID :: pid()} | {error, Reason :: any()}.
start_link(GroupName) ->
    gen_server:start_link({local, GroupName}, ?MODULE, [GroupName], []).

%% @doc Create new process group.
-spec create(GroupName :: atom()) -> ok | {error, Reason :: any()}.
create(GroupName) ->
	case supervisor:start_child(?SUPERVISOR,
								{GroupName,
								 {process_group, start_link, [GroupName]},
								 transient, brutal_kill, worker, [process_group]}) of
		{ok, _PID} ->
			ok;
		{error, {already_started, _PID}} ->
			ok;
		Error ->
			Error
	end.

%% @doc Delete process group.
-spec delete(GroupName :: atom()) -> ok.
delete(GroupName) ->
	ok = supervisor:terminate_child(?SUPERVISOR, GroupName),
	ok = supervisor:delete_child(?SUPERVISOR, GroupName).

%% @doc Join calling process to given group.
-spec join(GroupName :: atom()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
join(GroupName) ->
	join(GroupName, self()).

%% @doc Join process to given group.
-spec join(GroupName :: atom(), PID :: pid()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
join(GroupName, PID) ->
	try
		gen_server:call(GroupName, {join, PID})
	catch
		exit:{noproc, _} ->
			{error, {no_such_group, GroupName}}
	end.

%% @doc Leave calling process from given group.
-spec leave(GroupName :: atom()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
leave(GroupName) ->
	leave(GroupName, self()).

%% @doc Leave process from given group.
-spec leave(GroupName :: atom(), PID :: pid()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
leave(GroupName, PID) ->
	try
		gen_server:call(GroupName, {leave, PID})
	catch
		exit:{noproc, _} ->
			{error, {no_such_group, GroupName}}
	end.

%% @doc Get all members of a given group.
-spec get_members(GroupName :: atom()) -> Result :: [pid()] | {error, {no_such_group, GroupName :: atom()}}.
get_members(GroupName) ->
	try
		gen_server:call(GroupName, get_members)
	catch
		exit:{noproc, _} ->
			{error, {no_such_group, GroupName}}
	end.

%% @doc Send notification to all members of a given group.
-spec notify_members(GroupName :: atom(), Notification :: any()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
notify_members(GroupName, Notification) ->
	try
		gen_server:call(GroupName, {notify_members, Notification})
	catch
		exit:{noproc, _} ->
			{error, {no_such_group, GroupName}}
	end.

%% @doc Return all groups.
-spec which_groups() -> GroupNames :: [atom()].
which_groups() ->
	[GroupName || {GroupName, _PID, worker, [process_group]} <- supervisor:which_children(process_group_supervisor)].

%% =============================================================================
%% gen_server behaviour functions
%% =============================================================================
%%------------------------------------------------------------------------------
%% @spec init(Args) ->
%%			 {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%% where
%%		Args = any()
%%		State = state_record()
%%		Timeout = int() | infinity
%%		Reason = any()
%% @doc Initializing function of gen_server.
%% @end
%%------------------------------------------------------------------------------
init([GroupName]) ->
	TID = dict:new(),
	State = #process_group_state{name = GroupName, table_id = TID},
    {ok, State}.

%%------------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%      {reply, Reply, State} |
%%      {reply, Reply, State, Timeout} |
%%      {noreply, State} |
%%      {noreply, State, Timeout} |
%%      {stop, Reason, Reply, State} |
%%      {stop, Reason, State}
%% where
%%      Request = any()
%%      From = {pid(), Tag}
%%      State = sysmon_state_record()
%%      Reply = any()
%%      Timeout = int() | infinity
%%      Reason = any()
%% @doc Whenever a gen_server receives a request sent using gen_server:call/2,3 
%% or gen_server:multi_call/2,3,4, this function is called to handle the request. 
%% @end
%%------------------------------------------------------------------------------
handle_call({join, PID}, _From, State) ->
	#process_group_state{table_id = Dict} = State,
	Ref = erlang:monitor(process, PID),
	NewDict = dict:store(PID, Ref, Dict),
	NewState = State#process_group_state{table_id = NewDict},
	{reply, ok, NewState};
handle_call({leave, PID}, _From, State) ->
	#process_group_state{table_id = Dict} = State,
	NewState =
		case dict:find(PID, Dict) of
			error ->
				State;
			{ok, Ref} ->
				true = erlang:demonitor(Ref, [flush]),
				NewDict = dict:erase(PID, Dict),
				State#process_group_state{table_id = NewDict}
		end,
	{reply, ok, NewState};
handle_call(get_members, _From, State) ->
	#process_group_state{table_id = Dict} = State,
	PIDs = [PID || PID <- dict:fetch_keys(Dict)],
	{reply, PIDs, State};
handle_call({notify_members, Notification}, _From, State) ->
	#process_group_state{table_id = Dict} = State,
	_ = [PID ! Notification || PID <- dict:fetch_keys(Dict)],
	{reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, error, State}.

%%------------------------------------------------------------------------------
%% @spec handle_cast(Msg, State) ->
%%      {noreply, State} |
%%      {noreply, State, Timeout} |
%%      {stop, Reason, State}
%% where
%%      Msg = any()
%%      State = state_record()
%%      Timeout = int() | infinity
%%      Reason = any()
%% @doc Whenever a gen_server receives a request sent using gen_server:cast/2 
%% or gen_server:abcast/2,3, this function is called to handle the request. 
%% @end
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @spec handle_info(Info, State) -> 
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%% where
%%      Info = any()
%%      State = state_record()
%%      Timeout = int() | infinity
%%      Reason = any()
%% @doc This function is called by a gen_server when a timeout occurs or when it receives 
%% any other message than a synchronous or asynchronous request (or a system message). 
%% @end
%%------------------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, PID, _Reason}, State) ->
	#process_group_state{table_id = Dict} = State,
	NewDict = dict:erase(PID, Dict),
	NewState = State#process_group_state{table_id = NewDict},
	{noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = any()
%%      State = state_record()
%% @doc Function called when gen_server is going to be shutdown.
%% @end
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, State}
%% where
%%      OldVsn = Vsn | {down, Vsn}
%%      Vsn = any()
%%      State = state_record()
%%      Extra = any()
%% @doc Convert process state when code is changed
%% @end
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Local functions
%% =============================================================================
