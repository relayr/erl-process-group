%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2012
%% @version 1.0
%% @doc process_group_tests
%% @end
%%------------------------------------------------------------------------------
-module(process_group_tests).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include("../include/process_group.hrl").
-include_lib("testutils/include/testing.hrl").

%% =============================================================================
%% Tests
%% =============================================================================
-ifdef(TEST).

-define(DEFAULT_GROUPS, [default_logger_group,
						 perfmon_logger_group]).

?TEST_FUN().
crud_test() ->
	Groups1 = process_group:which_groups() -- ?DEFAULT_GROUPS,
	?assertEqual([], Groups1),
	
	Result1 = process_group:create(g1),
	?assertEqual(ok, Result1),
	Groups2 = process_group:which_groups() -- ?DEFAULT_GROUPS,
	?assertEqual([g1], Groups2),
	
	Result2 = process_group:create(g1),
	?assertEqual(ok, Result2),
	Groups3 = process_group:which_groups() -- ?DEFAULT_GROUPS,
	?assertEqual([g1], Groups3),
	
	Result3 = process_group:create(g2),
	?assertEqual(ok, Result3),
	Groups4 = process_group:which_groups() -- ?DEFAULT_GROUPS,
	?assertEqual(2, length(Groups4)),
	?assert(lists:member(g1, Groups4)),
	?assert(lists:member(g2, Groups4)),
	
	Result4 = process_group:delete(g1),
	?assertEqual(ok, Result4),
	Groups5 = process_group:which_groups() -- ?DEFAULT_GROUPS,
	?assertEqual([g2], Groups5),
	
	Result5 = process_group:delete(g2),
	?assertEqual(ok, Result5),
	Groups6 = process_group:which_groups() -- ?DEFAULT_GROUPS,
	?assertEqual([], Groups6),
	
	Result6 = process_group:delete(g1),
	?assertEqual(ok, Result6).

?TEST_FUN().
members_test() ->
	MemberFun =
		fun(Fun, ReceivedMessages) ->
			receive
				{join, Group} ->
					ok = process_group:join(Group),
					Fun(Fun, ReceivedMessages);
				{leave, Group} ->
					ok = process_group:leave(Group),
					Fun(Fun, ReceivedMessages);
				{get_received_messages, PID} ->
					PID ! {received_messages, ReceivedMessages},
					Fun(Fun, ReceivedMessages);
				stop ->
					ok;
				Msg ->
					NewReceivedMessages = ReceivedMessages ++ [Msg],
					Fun(Fun, NewReceivedMessages)
			end
		end,
	Member1 = spawn_link(fun() -> MemberFun(MemberFun, []) end),
	Member2 = spawn_link(fun() -> MemberFun(MemberFun, []) end),
	
	ok = check_members(g1, {error, {no_such_group, g1}}),
	ok = check_members(g2, {error, {no_such_group, g2}}),
	
	Result1 = process_group:join(g1),
	?assertEqual({error, {no_such_group, g1}}, Result1),

	% create groups and join members
	ok = process_group:create(g1),
	ok = process_group:create(g2),
	
	Member1 ! {join, g1},
	Member2 ! {join, g1},
	Member2 ! {join, g2},

	% check if group membership is correct
	ok = check_members(g1, [Member1, Member2]),
	ok = check_members(g2, [Member2]),

	% no duplicate membership
	Member2 ! {join, g2},
	ok = check_members(g2, [Member2]),

	ok = process_group:notify_members(g1, notification1),
	ok = check_received_messages(Member1, [notification1]),
	ok = check_received_messages(Member2, [notification1]),

	ok = process_group:notify_members(g2, notification2),
	ok = check_received_messages(Member1, [notification1]),
	ok = check_received_messages(Member2, [notification1, notification2]),

	% one of the processes leaves the group
	Member2 ! {leave, g1},
	?LOOPER(fun() ->
				?assertNot(lists:member(Member2, process_group:get_members(g1)))
			end,
			[],
			10,
			50),
	ok = process_group:notify_members(g1, notification3),
	ok = check_received_messages(Member1, [notification1, notification3]),
	ok = check_received_messages(Member2, [notification1, notification2]),

	% delete one of the groups
	Result2 = process_group:delete(g2),
	?assertEqual(ok, Result2),
	Result3 = process_group:notify_members(g2, notification4),
	?assertEqual({error, {no_such_group, g2}}, Result3),
	ok = check_received_messages(Member1, [notification1, notification3]),
	ok = check_received_messages(Member2, [notification1, notification2]),

	Result4 = process_group:which_groups() -- ?DEFAULT_GROUPS,
	?assertEqual([g1], Result4),
	Result5 = process_group:notify_members(g1, notification5),
	?assertEqual(ok, Result5),
	ok = check_received_messages(Member1, [notification1, notification3, notification5]),
	ok = check_received_messages(Member2, [notification1, notification2]),

	Member1 ! stop,
	Member2 ! stop,
	?WAIT_FOR_PROCESS_STOPPED(Member1),
	?WAIT_FOR_PROCESS_STOPPED(Member2).

%% =============================================================================
%% Tests
%% =============================================================================

check_members(Group, ExpectedResult) when is_tuple(ExpectedResult) ->
	?LOOPER(
		fun() ->
			Result1 = process_group:get_members(Group),
            Result2 = process_group:count_members(Group),
            ?assertEqual(ExpectedResult, Result1),
			?assertEqual(ExpectedResult, Result2)
		end,
		[],
		10,
		50
	),
	ok;
check_members(Group, ExpectedResult) ->
	?LOOPER(
		fun() ->
            Result1 = process_group:get_members(Group),
            Result2 = process_group:count_members(Group),
			?assertEqual(lists:sort(ExpectedResult), lists:sort(Result1)),
            ?assertEqual(length(ExpectedResult), Result2)
		end,
		[],
		10,
		50
	),
	ok.

check_received_messages(Member, ExpectedMessages) ->
	Member ! {get_received_messages, self()},
	{received_messages, ReceivedMessages} = receive Response -> Response end,
	?assertEqual(ExpectedMessages, ReceivedMessages),
	ok.

-endif.