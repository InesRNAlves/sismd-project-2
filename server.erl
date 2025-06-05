-module(server).
-export([start/0, store/2, lookup/1]).

% start/1
%start(Server) -> register(Server,spawn(fun() -> loop() end)).
%loop() ->
%  receive
%    {From, stop} ->
%      io:format("Received from ~p message to stop!~n",[From]),
%      From ! {self(),server_disconnect};
%    {From, Msg} ->
%      io:format("Received ~p: ~p~n",[From,Msg]),
%      io:format("Received ~p: ~p from ~p~n", [From, Msg, node(From)]),
%      io:format("Sending reply...~n"),
%      From ! {self(),happy_to_receive_your_message},
%      loop()
%  end.


%start() -> register(central, spawn(fun() -> loops() end)).
%store(Key, Value) -> rpc({store, Key, Value}).
%lookup(Key) -> rpc({lookup, Key}).
%
%stop() ->
%  rpc(stop).
%
%rpc(Q) ->
%  central ! {self(), Q},
%  receive
%    {central, Reply} -> Reply
%  end.
%
%loops() ->
%  receive
%    {From, {store, Key, Value}} ->
%      put(Key, {ok, Value}),
%      From ! {central, true, happy_to_receive_your_message},
%      io:format("STORE: Received ~p: ~p from ~p~n", [From, Key, Value, node(From)]),
%      loops();
%    {From, {lookup, Key}} ->
%      From ! {central, get(Key)},
%      io:format("LOOKUP: Received ~p: ~p from ~p~n", [From, Key, node(From)]),
%      loops();
%    {From, stop} ->
%      io:format("STOP: Received stop from ~p~n", [From]),
%      From ! {central, server_stopped},
%      ok  % Stops here, does NOT call loops()
%  end.


start() -> register(central, spawn(fun() -> loops() end)).
store(Key, Value) -> rpc({store, Key, Value}).
lookup(Key) -> rpc({lookup, Key}).
rpc(Q) ->
  central ! {self(), Q},
  receive
    {central, Reply} -> Reply
  end.
loops() ->
  receive
    {From, {store, Key, Value}} ->
      put(Key, {ok, Value}),
      From ! {central, true},
      io:format("STORE: Received ~p: ~p from ~p~n", [From, Key, Value, node(From)]),
      loops();
    {From, {lookup, Key}} ->
      From ! {central, get(Key)},
      io:format("LOOKUP: Received ~p: ~p from ~p~n", [From, Key, node(From)]),
      loops()
  end.