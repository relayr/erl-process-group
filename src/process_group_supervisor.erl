%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2012
%% @version 1.0
%% @doc Supervisor for process_group processes.
%% @end
%%------------------------------------------------------------------------------
-module(process_group_supervisor).

-behaviour(supervisor).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
		 start_link/0,
		 init/1
		]).


%% =============================================================================
%% Exported functions
%% =============================================================================

%% @doc Start function for the application's main supervisor.
-spec start_link() -> {ok, PID :: pid()} | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/2,3, 
%%      this function is called by the new process to find out about restart strategy, 
%%      maximum restart frequency and child specifications.
%%      Args is the Args argument provided to the start function.
%%      RestartStrategy is the restart strategy and MaxR and MaxT defines the maximum 
%%      restart frequency of the supervisor. [ChildSpec] is a list of valid child 
%%      specifications defining which child processes the supervisor should start and 
%%      monitor. See the discussion about Supervision Principles above.
%%      Note that when the restart strategy is simple_one_for_one, the list of child 
%%      specifications must be a list with one child specification only. 
%%      No child process is then started during the initialization phase, 
%%      but all children are assumed to be started dynamically using supervisor:start_child/2.
%%      The function may also return ignore.
init([]) ->
    SupFlags = {one_for_all, 3, 10},
    {ok, {SupFlags, []}}.
