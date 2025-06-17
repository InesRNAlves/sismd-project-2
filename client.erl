-module(client).
-export([start/2, start/3, stop/1]).

-record(state, {
  client, % E.g.: client1
  neighbors, % E.g.: [{client1, client1@UKIMC06VF7N71M}, {central, central@UKIMC06VF7N71M}]
  message_interval_ms
}).

% Github repository: https://github.com/InesRNAlves/sismd-project-2

% Relay-only client (message_interval_ms = 0)
start(Client, Neighbors) ->
  start(Client, Neighbors, 0).

% Starts a new process with name 'Client'
start(Client, Neighbors, MessageIntervalMs) ->
  Pid = spawn(fun() -> init(Client, Neighbors, MessageIntervalMs) end),
  register(Client, Pid).

% Sends a stop message to the 'Client' process to shut it down
stop(Client) ->
  Client ! stop_process.

init(Client, Neighbors, MessageIntervalMs) ->
  State = #state{client = Client, neighbors = Neighbors, message_interval_ms = MessageIntervalMs},
  io:format("Client ~p started with neighbors: ~p~n", [Client, Neighbors]),
  loop(State).

% Loop for Relay-Only Clients (Interval = 0)
loop(State = #state{message_interval_ms = 0}) ->
  receive
    stop_process ->
      io:format("Client ~p stopping (relay).~n", [State#state.client]);
    {'DOWN', _Ref, process, _Pid, _Reason} ->
      [_Failed | Remaining] = State#state.neighbors,
      io:format("Client ~p: Neighbor down, remaining neighbors: ~p~n", [State#state.client, Remaining]),
      loop(State#state{neighbors = Remaining});
    {From, {store, Key, Value}} ->
      % Relay the data to a valid neighbor (e.g. central server)
      try_neighbors(State#state.neighbors, Key, Value, State),
      From ! {self(), message_relayed},
      io:format("Client ~p (relay): forwarded ~p = ~p to server~n", [State#state.client, Key, Value]),
      loop(State)
  after 1000 ->
    loop(State)
  end;

loop(State = #state{client = Client, message_interval_ms = MessageIntervalMs, neighbors = Neighbors}) ->
  receive
    stop_process ->
      io:format("Client ~p stopping.~n", [Client]); % Receives message to stop the process --> clean shutdown
    {'DOWN', _Ref, process, _Pid, _Reason} ->
      [_Failed | Remaining] = Neighbors,
      io:format("Client ~p: Neighbor down, remaining neighbors: ~p~n", [Client, Remaining]),
      loop(State#state{neighbors = Remaining});
    {From, {store, Key, Value}} ->
      % Relay the data to a valid neighbor (e.g. central server)
      try_neighbors(State#state.neighbors, Key, Value, State),
      From ! {self(), message_relayed},
      io:format("Client ~p (relay): forwarded ~p = ~p to server~n", [State#state.client, Key, Value]),
      loop(State)
  % After the defined interval, it: Picks a key like "temperature"
  % Generates a value (e.g., 27)
  % Tries to send to a neighbor
  % Recursively loops
  after MessageIntervalMs ->
    Key = random_key(),
    Value = generate_value(Key),
    try_neighbors(Neighbors, Key, Value, State),
    loop(State)
  end.

% Tries to send to the first available neighbor
try_neighbors([], Key, Value, State) ->
  io:format("Client '~p': No available neighbors to send ~p = ~p~n", [State#state.client, Key, Value]);
try_neighbors([{Client, Node} | Rest], Key, Value, State) ->
  try_send(Client, Node, Key, Value, Rest, State).

try_send(Client, Node, Key, Value, Rest, State) ->
  Pid = rpc:call(Node, erlang, whereis, [Client]),
  try_monitor(Pid, Client, Node, Key, Value, Rest, State).

% If the PID is undefined, tries the next neighbor
try_monitor(undefined, Client, Node, Key, Value, Rest, State) ->
  io:format("Client ~p: ~p@~p unreachable, trying next~n", [State#state.client, Client, Node]),
  try_neighbors(Rest, Key, Value, State);

% If the PID is found, sends the message
try_monitor(Pid, Name, Node, Key, Value, Rest, State) ->
  % Monitor the process to catch crashes
  Ref = monitor(process, Pid),
  Pid ! {self(), {store, Key, Value}},
  receive
    % Remove the monitor reference and flush any 'DOWN' messages from the mailbox,
    % in case the process crashed while waiting for the reply
    {_, Reply} ->
      demonitor(Ref, [flush]),
      io:format("Client ~p: Sent ~p = ~p to ~p@~p, reply: ~p~n",
        [State#state.client, Key, Value, Name, Node, Reply])

  % If the process does not respond within 1s, it is considered unreachable and tries the next neighbor
  after 1000 ->
    demonitor(Ref, [flush]),
    io:format("Client ~p: ~p@~p timeout, trying next~n", [State#state.client, Name, Node]),
    try_neighbors(Rest, Key, Value, State)
  end.

% Picks a random key from the list of keys
random_key() ->
  Keys = ["humidity", "temperature", "co2_level", "pressure"],
  lists:nth(rand:uniform(length(Keys)), Keys).

% Generates a random value based on the key
generate_value("humidity") -> rand:uniform(100);
generate_value("temperature") -> rand:uniform(35) + 5;
generate_value("co2_level") -> rand:uniform(800) + 400;
generate_value("pressure") -> rand:uniform(20) + 980.
