%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2012
%% @version 1.0
%% @doc Application module for process_group application.
%% @end
%%------------------------------------------------------------------------------
-module(process_group_app).

-behaviour(application).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% application behaviour exports
%%------------------------------------------------------------------------------
-export([
		start/2,
		stop/1
	]).

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------


%% =============================================================================
%% application behaviour functions
%% =============================================================================

-spec start(normal, Args :: list()) -> {ok, PID :: pid()} | {error, Reason :: any()}.
%% @private
%% @doc This is the entry module for your application. It contains the
%% 		start function and some other stuff. You identify this module
%% 		using the 'mod' attribute in the .app file.
%% 		The start function is called by the application controller.
%% 		It normally returns {ok,Pid}, i.e. the same as gen_server and
%% 		supervisor. Here, we simply call the start function in our supervisor.
%% 		One can also return {ok, Pid, State}, where State is reused in stop(State).
%% 		Type can be 'normal' or {takeover,FromNode}. If the 'start_phases'
%% 		attribute is present in the .app file, Type can also be {failover,FromNode}.
%% 		This is an odd compatibility thing.
start(normal, _Args) ->
	process_group_supervisor:start_link().

-spec stop(_State) -> ok.
%% @private
%% @doc Function used to stop application.
stop(_State) ->
    ok.


%% =============================================================================
%% Exported functions
%% =============================================================================


%% =============================================================================
%% Local functions
%% =============================================================================
