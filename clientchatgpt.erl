-module(clientchatgpt).
-export([start/1, send/2, stop/1, spawn_relay/1, loop/1]).

-record(state, {
  name,
  relays = [] %% List of {Pid, MonitorRef}
}).

%% Starts the main client
start(Client) ->
  register(Client, spawn(fun() -> loop(#state{name = Client}) end)).

%% Spawns a relay subprocess and adds it to the list
spawn_relay(Client) ->
  Client ! spawn_relay.

%% Sends a message like {Value, Type}
send(Client, Msg) ->
  Client ! {send, Msg}.

send_msg(Client, Server, RemoteMachine, Message)->
  Client ! {send, Server, RemoteMachine, Message}.

%% Stops the client
stop(Client) ->
  Client ! stop.

%% Main client loop
loop(State = #state{name = Name, relays = Relays}) ->
  receive
    spawn_relay ->
      Pid = spawn(fun relay_loop/0),
      Ref = monitor(process, Pid),
      io:format("~p: Relay criado: ~p~n", [Name, Pid]),
      loop(State#state{relays = [{Pid, Ref} | Relays]});

    {'DOWN', Ref, process, _Pid, _Reason} ->
      NewRelays = lists:filter(fun({_, R}) -> R =/= Ref end, Relays),
      io:format("~p: Relay morreu (ref ~p). Atualizando lista.~n", [Name, Ref]),
      loop(State#state{relays = NewRelays});

    {send, Msg} ->
      Result = try_send(Relays, Msg),
      handle_send_result(Result, State);

    stop ->
      io:format("~p: Cliente a encerrar~n", [Name])
  end.

%% Handle the result of try_send
handle_send_result({ok, UpdatedRelays}, State) ->
  loop(State#state{relays = UpdatedRelays});
handle_send_result({fail, []}, State = #state{name = Name}) ->
  io:format("~p: impossível enviar mensagem~n", [Name]),
  loop(State#state{relays = []});
handle_send_result({fail, Remaining}, State) ->
  loop(State#state{relays = Remaining}).

%% Try each relay until success or exhaustion
try_send([], _) ->
  {fail, []};
try_send([{Pid, Ref} | Rest], Msg) ->
  Pid ! {self(), Msg},
  receive
    {relay, ack} ->
      io:format("Mensagem entregue via ~p~n", [Pid]),
      {ok, [{Pid, Ref} | Rest]}
  after 1000 ->
    io:format("Relay ~p falhou~n", [Pid]),
    try_send(Rest, Msg)
  end.

relay_loop() ->
  receive
    {From, {Value, Type}} ->
      handle_send(From, Value, Type),
      relay_loop()
  end.

handle_send(From, Value, Type) ->
  Server = whereis(central),
  handle_server_lookup(Server, From, Value, Type).

handle_server_lookup(undefined, From, _Value, _Type) ->
  io:format("Relay: servidor central não encontrado~n"),
  From ! {relay, error};

handle_server_lookup(Server, From, Value, Type) ->
  Server ! {self(), {store, Type, Value}},
  receive
    {central, _, _} ->
      io:format("Relay: enviado {~p, ~p} ao servidor~n", [Type, Value]),
      From ! {relay, ack}
  after 1000 ->
    io:format("Relay: timeout ao contactar servidor~n"),
    From ! {relay, error}
  end.

