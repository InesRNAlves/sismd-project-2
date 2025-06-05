-module(client_2).
-export([start/3, stop/1]).

-record(state, {
  client,
  neighbors,
  interval_ms
}).

start(Client, Neighbors, MessageIntervalMs) ->
  Pid = spawn_link(fun() -> init(Client, Neighbors, MessageIntervalMs) end),
  register(Client, Pid).

stop(Client) ->
  Client ! stop.

init(Client, Neighbors, Interval) ->
  process_flag(trap_exit, true),
  rand:seed(exsplus, erlang:timestamp()), % or rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer(), node()}),
  State = #state{client = Client, neighbors = Neighbors, interval_ms = Interval},
  io:format("Client ~p started with neighbors: ~p~n", [Client, Neighbors]),
  loop(State).

loop(State = #state{client = Id, interval_ms = Interval, neighbors = Neighbors}) ->
  receive
    stop ->
      io:format("Client ~p stopping.~n", [Id]);
    {'DOWN', _Ref, process, _Pid, _Reason} ->
      [_Failed | Remaining] = Neighbors,
      loop(State#state{neighbors = Remaining})
  after Interval ->
    Key = random_key(),
    Value = generate_value(Key),
    try_neighbors(Neighbors, Key, Value, State),
    loop(State)
  end.

try_neighbors([], Key, _Value, State) ->
  io:format("Client ~p: No available neighbors for key ~p~n", [State#state.client, Key]);
try_neighbors([{Client, Node} | Rest], Key, Value, State) ->
  try_send(Client, Node, Key, Value, Rest, State).

try_send(Client, Node, Key, Value, Rest, State) ->
  Pid = rpc:call(Node, erlang, whereis, [Client]),
  try_monitor(Pid, Client, Node, Key, Value, Rest, State).

try_monitor(undefined, Client, Node, Key, Value, Rest, State) ->
  io:format("Client ~p: ~p@~p unreachable, trying next~n", [State#state.client, Client, Node]),
  try_neighbors(Rest, Key, Value, State);
try_monitor(Pid, Name, Node, Key, Value, Rest, State) ->
  Ref = monitor(process, Pid),
  Pid ! {self(), {store, Key, Value}},
  receive
    {_, Reply} ->
      demonitor(Ref, [flush]),
      io:format("Client ~p: Sent ~p = ~p to ~p@~p, reply: ~p~n",
        [State#state.client, Key, Value, Name, Node, Reply])
  after 1000 ->
    demonitor(Ref, [flush]),
    io:format("Client ~p: ~p@~p timeout, trying next~n", [State#state.client, Name, Node]),
    try_neighbors(Rest, Key, Value, State)
  end.

random_key() ->
  Keys = ["humidity", "temperature", "co2_level", "pressure"],
  lists:nth(rand:uniform(length(Keys)), Keys).

generate_value("humidity") -> rand:uniform(100);
generate_value("temperature") -> rand:uniform(35) + 5;
generate_value("co2_level") -> rand:uniform(800) + 400;
generate_value("pressure") -> rand:uniform(20) + 980.
