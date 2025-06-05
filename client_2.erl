-module(client_2).
-export([start/4, stop/1]).

-record(state, {
  id,
  neighbors,
  interval_ms
}).

start(Id, _Server, Neighbors, IntervalMs) ->
  Pid = spawn_link(fun() -> init(Id, Neighbors, IntervalMs) end),
  register(Id, Pid).

stop(Id) ->
  Id ! stop.

init(Id, Neighbors, Interval) ->
  process_flag(trap_exit, true),
  rand:seed(exsplus, erlang:timestamp()), % or rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer(), node()}),
  State = #state{id = Id, neighbors = Neighbors, interval_ms = Interval},
  io:format("Client ~p started with neighbors: ~p~n", [Id, Neighbors]),
  loop(State).

loop(State = #state{id = Id, interval_ms = Interval, neighbors = Neighbors}) ->
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
  io:format("Client ~p: No available neighbors for key ~p~n", [State#state.id, Key]);

try_neighbors([{Name, Node} | Rest], Key, Value, State) ->
  maybe_send(Name, Node, Key, Value, Rest, State).

maybe_send(Name, Node, Key, Value, Rest, State) ->
  Pid = rpc:call(Node, erlang, whereis, [Name]),
  maybe_monitor(Pid, Name, Node, Key, Value, Rest, State).

maybe_monitor(undefined, Name, Node, Key, Value, Rest, State) ->
  io:format("Client ~p: ~p@~p unreachable, trying next~n", [State#state.id, Name, Node]),
  try_neighbors(Rest, Key, Value, State);

maybe_monitor(Pid, Name, Node, Key, Value, Rest, State) ->
  Ref = monitor(process, Pid),
  Pid ! {self(), {store, Key, Value}},
  receive
    {_, Reply} ->
      demonitor(Ref, [flush]),
      io:format("Client ~p: Sent ~p = ~p to ~p@~p, reply: ~p~n",
        [State#state.id, Key, Value, Name, Node, Reply])
  after 1000 ->
    demonitor(Ref, [flush]),
    io:format("Client ~p: ~p@~p timeout, trying next~n", [State#state.id, Name, Node]),
    try_neighbors(Rest, Key, Value, State)
  end.

random_key() ->
  Keys = ["humidity", "temperature", "co2_level", "pressure"],
  lists:nth(rand:uniform(length(Keys)), Keys).

generate_value("humidity") -> rand:uniform(100);
generate_value("temperature") -> rand:uniform(35) + 5;
generate_value("co2_level") -> rand:uniform(800) + 400;
generate_value("pressure") -> rand:uniform(20) + 980.
