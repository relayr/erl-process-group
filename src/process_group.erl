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
    join/3,
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
    name        :: atom(),
    table_id    :: ets:tid()
}).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(SUPERVISOR,    process_group_supervisor).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type notify_fun() :: fun((Process :: pid(), ProcessState :: any()) -> ok).

-export_type([
    notify_fun/0
]).

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
-spec join(GroupName :: atom(), Process :: pid() | atom()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
join(GroupName, Process) ->
    join(GroupName, Process, undefined).

%% @doc Join process to given group.
-spec join(GroupName :: atom(), Process :: pid() | atom(), ProcessState :: any()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
join(GroupName, Process, ProcessState) ->
    try
        gen_server:call(GroupName, {join, Process, ProcessState})
    catch
        exit:{noproc, _} ->
            {error, {no_such_group, GroupName}}
    end.

%% @doc Leave calling process from given group.
-spec leave(GroupName :: atom()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
leave(GroupName) ->
    leave(GroupName, self()).

%% @doc Leave process from given group.
-spec leave(GroupName :: atom(), Process :: pid() | atom()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
leave(GroupName, Process) ->
    try
        gen_server:call(GroupName, {leave, Process})
    catch
        exit:{noproc, _} ->
            {error, {no_such_group, GroupName}}
    end.

%% @doc Get all members of a given group.
-spec get_members(GroupName :: atom()) -> Result :: [pid() | atom()] | {error, {no_such_group, GroupName :: atom()}}.
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

%% @doc Send notification to all members of a given group or execute notification function on each of them.
-spec notify_members(GroupName :: atom(), Notification :: notify_fun() | any()) -> Result :: ok | {error, {no_such_group, GroupName :: atom()}}.
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
    [GroupName || {GroupName, _PID, worker, [process_group]} <- supervisor:which_children(?SUPERVISOR)].

%% =============================================================================
%% gen_server behaviour functions
%% =============================================================================

%% @private
init([GroupName]) ->
    TID = ets:new(GroupName, [protected, set, named_table]),
    State = #process_group_state{name = GroupName, table_id = TID},
    {ok, State}.

%% @private
handle_call({join, Process, ProcessState}, _From, State) ->
    #process_group_state{table_id = TID} = State,
    Ref = erlang:monitor(process, Process),
    true = ets:insert(TID, {Process, Ref, ProcessState}),
    {reply, ok, State};
handle_call({leave, Process}, _From, State) ->
    #process_group_state{table_id = TID} = State,
    case ets:lookup(TID, Process) of
        [] ->
            ok;
        [{Process, Ref, _ProcessState}] ->
            true = erlang:demonitor(Ref, [flush]),
            true = ets:delete(TID, Process),
            ok
    end,
    {reply, ok, State};
handle_call(get_members, _From, State) ->
    #process_group_state{table_id = TID} = State,
    Processes = [Process || {Process, _Ref, _ProcessState} <- ets:tab2list(TID)],
    {reply, Processes, State};
handle_call(count_members, _From, State) ->
    #process_group_state{table_id = TID} = State,
    Size = ets:info(TID, size),
    {reply, Size, State};
handle_call({notify_members, Notification}, _From, State) when is_function(Notification) ->
    #process_group_state{table_id = TID} = State,
    _ = [catch Notification(Process, ProcessState) || {Process, _Ref, ProcessState} <- ets:tab2list(TID)],
    {reply, ok, State};
handle_call({notify_members, Notification}, _From, State) ->
    #process_group_state{table_id = TID} = State,
    _ = [Process ! Notification || {Process, _Ref, _ProcessState} <- ets:tab2list(TID)],
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', Ref, process, {Process, _Node}, Reason}, State) when is_atom(Process) ->
    handle_info({'DOWN', Ref, process, Process, Reason}, State);
handle_info({'DOWN', _Ref, process, Process, _Reason}, State) ->
    #process_group_state{table_id = TID} = State,
    true = ets:delete(TID, Process),
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
