-module(client).
-export([start/4, stop/1]).


start(Client, Server, RemoteMachine, IntervalMs) ->
  Pid = spawn(fun() -> loop(Client, Server, RemoteMachine, IntervalMs, 0) end),
  register(Client, Pid).

stop(Client) ->
  Client ! stop.

loop(Client, Server, RemoteMachine, Interval, Index) ->
  receive
    stop ->
      io:format("~p stopping.~n", [Client])
  after Interval ->
    Keys = ["humidity", "temperature", "co2_level", "pressure"],
    Key = lists:nth((Index rem length(Keys)) + 1, Keys),
    Value = generate_value(Key),

    {Server, RemoteMachine} ! {self(), {store, Key, Value}},
    receive
      {_, Reply} ->
        io:format("Stored ~p: ~p (reply: ~p)~n", [Key, Value, Reply])
    end,

    loop(Client, Server, RemoteMachine, Interval, Index + 1)
  end.

generate_value("humidity") -> rand:uniform(100);
generate_value("temperature") -> rand:uniform(35) + 5;
generate_value("co2_level") -> rand:uniform(800) + 400;
generate_value("pressure") -> rand:uniform(20) + 980.