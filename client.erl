-module(client).
-export([start/1, add_remote/1, send_msg/4]).



start(Client) -> register(Client, spawn_link(fun() -> loop() end)).

add_remote(RemoteMachine) -> net_adm:ping(RemoteMachine).

send_msg(Client, Server, RemoteMachine, Message)->
  Client ! {send, Server, RemoteMachine, Message}.

%stop_client(Client) ->
%  Client ! {stop_client}.

loop() ->
  receive
    {send, Server, RemoteMachine, Message} ->
      {Server, RemoteMachine} ! {self(), Message},
      receive
        {_,Reply} -> io:format("Received from server: ~p~n",[Reply])
      end,
      loop()
    %{stop_client} ->
    %  io:format("Cliente exiting...")
  end.

%(State) ->
%ceive
%{'EXIT', SomePid, Reason} -> %% do something with the error
%  io:format("Received exit signal from process ~p because: ~n",[SomePid, Reason]),
%  loop(State)
%

%keep_alive(Name, Fun) ->
%  register(Name, Pid = spawn(Fun)),
%  on_exit(Pid, fun() -> keep_alive(Name, Fun) end).