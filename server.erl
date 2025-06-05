-module(server).
-export([start/0, stop/0, store/2, lookup/1]).

start() -> register(central, spawn(fun() -> loops(#{}) end)).

stop() ->
  central ! stop_server.

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

rpc(Query) ->
  central ! {self(), Query},
  receive
    {central, Reply} -> Reply
  end.

loops(Store) ->
  receive
    stop_server ->
      io:format("Central Server: Shutting down~n"), ok;

    {From, {store, Key, Value}} ->
      % Append the value to the list for that key
      UpdatedStore = maps:update_with(Key, fun(OldList) -> [Value | OldList] end, [Value], Store),
      From ! {central, {ok, Value}},
      io:format("Central Server STORE: ~p -> ~p = ~p~n", [node(From), Key, Value]),
      loops(UpdatedStore);

    {From, {lookup, Key}} ->
      Value = Value = lists:reverse(maps:get(Key, Store, [])), % orders the values by the order in which they were received
      From ! {central, Value},
      io:format("Central Server LOOKUP: ~p requested ~p -> ~p~n", [node(From), Key, Value]),
      loops(Store)
  end.