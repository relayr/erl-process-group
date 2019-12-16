# process_group

[![Build Status](https://travis-ci.org/relayr/erl-process-group.svg?branch=master)](https://travis-ci.org/relayr/erl-process-group) [![Coverage Status](https://coveralls.io/repos/github/relayr/erl-process-group/badge.svg?branch=master)](https://coveralls.io/github/relayr/erl-process-group?branch=master)

Application for managing of distributed process groups with API compatible with [OTP 'pg2' module](http://erlang.org/doc/man/pg2.html).

Each process group is managed by a single gen_server that creates protected ETS table for faster access. This allows to remove bottleneck of 'pg2' of each call going through gen_server.

Process group gen_server is responsible for monitoring of attached processes and removing them from ETS if any of these processes went down.

## Usage

#### create/1
Create new named process group
```
1> process_group:create(my_group).
ok
```

#### join/1,2
Add new process to specified group
```
2> PID = self().
<0.170.0>
3> process_group:join(my_group, PID).
ok
```

#### get_members/1
Get members of process group
```
4> process_group:get_members(my_group).
[<0.170.0>]
```

#### notify_members/2
Send message to all processes in specific group
```
5> process_group:notify_members(my_group, {data, [test_msg]}).
ok
6> flush().
Shell got {data,[test_msg]}
ok
```

#### leave/1,2
Remove process from specific group
```
7> process_group:leave(my_group, PID).
ok
```

#### delete/1
Delete process group
```
8> process_group:delete(my_group).
ok
```
