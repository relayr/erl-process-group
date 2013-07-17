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
	count_members/1,
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
	table_id	:: ets:tid()
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
	_ = supervisor:terminate_child(?SUPERVISOR, GroupName),
	_ = supervisor:delete_child(?SUPERVISOR, GroupName),
	ok.

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

%% @doc Count members of a given group.
-spec count_members(GroupName :: atom()) -> Result :: non_neg_integer() | {error, {no_such_group, GroupName :: atom()}}.
count_members(GroupName) ->
	try
		gen_server:call(GroupName, count_members)
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

%% @private
init([GroupName]) ->
	TID = ets:new(GroupName, [protected, set, named_table]),
	State = #process_group_state{name = GroupName, table_id = TID},
    {ok, State}.

%% @private
handle_call({join, PID}, _From, State) ->
	#process_group_state{table_id = TID} = State,
	Ref = erlang:monitor(process, PID),
	true = ets:insert(TID, {PID, Ref}),
	{reply, ok, State};
handle_call({leave, PID}, _From, State) ->
	#process_group_state{table_id = TID} = State,
	case ets:lookup(TID, PID) of
		[] ->
			ok;
		[{PID, Ref}] ->
			true = erlang:demonitor(Ref, [flush]),
			true = ets:delete(TID, PID),
			ok
	end,
	{reply, ok, State};
handle_call(get_members, _From, State) ->
	#process_group_state{table_id = TID} = State,
	PIDs = [PID || {PID, _Ref} <- ets:tab2list(TID)],
	{reply, PIDs, State};
handle_call(count_members, _From, State) ->
	#process_group_state{table_id = TID} = State,
	Size = ets:info(TID, size),
	{reply, Size, State};
handle_call({notify_members, Notification}, _From, State) ->
	#process_group_state{table_id = TID} = State,
	_ = [PID ! Notification || {PID, _Ref} <- ets:tab2list(TID)],
	{reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, error, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, PID, _Reason}, State) ->
	#process_group_state{table_id = TID} = State,
	true = ets:delete(TID, PID),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Local functions
%% =============================================================================
