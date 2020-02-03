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
-include_lib("test_utils/include/testing.hrl").

%% =============================================================================
%% Tests
%% =============================================================================
-ifdef(TEST).

-define(DEFAULT_GROUPS, [default_logger_group]).

crud_test() ->
    setup(),
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

members_test() ->
    setup(),
    Member1 = start_member(),
    Member2 = start_member(),

    ok = check_members(g1, {error, {no_such_group, g1}}),
    ok = check_members(g2, {error, {no_such_group, g2}}),

    Result1 = process_group:join(g1),
    ?assertEqual({error, {no_such_group, g1}}, Result1),

    % create groups and join members
    ok = process_group:create(g1),
    ok = process_group:create(g2),

    ok = process_group:join(g1, Member1),
    ok = process_group:join(g1, Member2),
    ok = process_group:join(g2, Member2),

    % check if group membership is correct
    ok = check_members(g1, [Member1, Member2]),
    ok = check_members(g2, [Member2]),

    % no duplicate membership
    ok = process_group:join(g2, Member2),
    ok = check_members(g2, [Member2]),

    ok = process_group:notify_members(g1, notification1),
    ok = check_received_messages(Member1, [notification1]),
    ok = check_received_messages(Member2, [notification1]),

    ok = process_group:notify_members(g2, notification2),
    ok = check_received_messages(Member1, [notification1]),
    ok = check_received_messages(Member2, [notification1, notification2]),

    % one of the processes leaves the group
    ok = process_group:leave(g1, Member2),
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

    ok = stop_member(Member1),
    ok = stop_member(Member2).

members_notify_fun_test() ->
    setup(),
    Member1 = start_member(),
    Member2 = start_member(),
    Member3 = start_member(),
    Member4 = start_member(),

    ok = process_group:create(ng),

    ok = process_group:join(ng, Member1, #{value => 5}),
    ok = process_group:join(ng, Member2, #{reply => false}),
    ok = process_group:join(ng, Member3),   % notification function for this process will fail because of state badmatch
    ok = process_group:join(ng, Member4, #{value => 7}),

    ok = process_group:notify_members(ng, fun(PID, State) ->
            case maps:get(reply, State, true) of
                true ->
                    Value = maps:get(value, State),
                    PID ! {value, Value},
                    ok;
                _ ->
                    ok
            end
        end),
    ok = check_received_messages(Member1, [{value, 5}]),
    ok = check_received_messages(Member2, []),
    ok = check_received_messages(Member3, []),
    ok = check_received_messages(Member4, [{value, 7}]),

    ok = process_group:delete(ng),

    ok = stop_member(Member1),
    ok = stop_member(Member2),
    ok = stop_member(Member3),
    ok = stop_member(Member4).


%% =============================================================================
%% Tests
%% =============================================================================

start_member() ->
    MemberFun =
        fun(Fun, ReceivedMessages) ->
            receive
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
    spawn_link(fun() -> MemberFun(MemberFun, []) end).

stop_member(PID) ->
    PID ! stop,
    ?WAIT_FOR_PROCESS_STOPPED(PID).

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

setup() ->
    {ok, _} = application:ensure_all_started(process_group),
    ok.

-endif.
