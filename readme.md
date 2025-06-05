1. erl -sname mynode
2. then start the Erlang shell with: erl
3. c(server).
4. server:start().
4. erlang:get_cookie().
5. erlang:set_cookie(node(), sismdcookie).    // sismdcookie is the name of the cookie defined by me
6. client:send_msg(elsa, central, 'server@RemoteMachine', testmessage).
   client:add_remote('central@UKIMC06VF7N71M').

10. erl -sname central -setcookie sismdcookie
11. erl -sname client1 -setcookie sismdcookie
12. client:send_msg(client1, central, 'central@UKIMC06VF7N71M', {store, <<"temperature">>, <<"20">>}). <<>> é para transformar em binario 
13. client:start(client1, central, 'central@UKIMC06VF7N71M', 2000).
14. client:start(client1, central, 'central@UKIMC06VF7N71M', 120000). // 2minutes



1> kvs:store({location,isep},"Porto").
true
2> kvs:store(weather,raining).
true
3> kvs:lookup(raining).
undefined
4> kvs:lookup(weather).
{ok,raining}
5> kvs:lookup({location,isep}).
{ok,"Porto"}


no client:

process_flag(trap_exit, true),
Pid = spawn_link(fun() -> ... end),