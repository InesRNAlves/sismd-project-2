-module(mobilecall).
-export([start/0,client/1]).


% Criação do servidor:
% mobilecall:start().
%
% Criação de clientes para testes:
%
% mobilecall:client(1).
% mobilecall:client(2).
% ...
% Cada um deles termina a chamada ao fim de 10 segundos.
% Para verificar que a falha é detectada, matar o processo diretamente usando o observer (dentro dos 10 segundos)
%

client(Id) ->
  io:format("Starting client ~w~n",[Id]),
  spawn(fun mobile_phone/0).

mobile_phone()->
  mobile ! {request,self(),allocate},
  receive
    {reply,{error, no_frequencies}} -> io:format("Cannot make call!~n");
    {reply,{ok,F}} ->
      io:format("Making call with frequency: ~w~n",[F]),
      timer:sleep(20000),
      io:format("Ok, ended call, releasing frequency ~w~n.",[F]),
      mobile ! {request,self(),{deallocate,F}},
      receive
        {reply,ok} -> io:format("Client closed...")
      end
  end.


start() ->
  register(mobile, spawn(fun init/0)).
init() ->
  process_flag(trap_exit, true),
  loop({[10,11,12,13,14,15],[]}).

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply,Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies=deallocate(Frequencies, Freq),
      Pid ! {reply,ok},
      loop(NewFrequencies);
    {'EXIT', _, normal} ->
      loop(Frequencies);
    {'EXIT', Pid, _Reason} ->
      Freq = find_pid(Pid,Frequencies),
      io:format("Client ~p crashed. Releasing frequency ~p~n", [Pid, Freq]),
      NewFrequencies = deallocate(Frequencies, Freq),
      loop(NewFrequencies)
  end.


allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequencies}};
allocate({[Freq|Frequencies], Allocated}, Pid) ->
  link(Pid),
  {{Frequencies,[{Freq,Pid}|Allocated]},{ok,Freq}}.

deallocate({Frequencies,Allocated},Freq)->
  {[Freq|Frequencies],delete_freq(Freq,Allocated)}.
delete_freq(_,[])->[];
delete_freq(Freq,[{Freq,_}|All])-> All;
delete_freq(Freq,[X|All])->[X|delete_freq(Freq,All)].

find_pid(Pid,{_,Allocated})-> find_pid2(Pid,Allocated).
find_pid2(Pid,[{Freq,Pid}|_])-> Freq;
find_pid2(Pid,[_|A])->find_pid2(Pid,A).