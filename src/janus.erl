-module(janus).

-behaviour(gen_server).

-export([start/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start(Server, Client, EventRef) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Server, Client, EventRef], []).


init([Server, Client, EventRef]) ->
    {ok, Socket} =
        gen_udp:open(
          0,
          [binary,
           {active, true},
           {ifaddr, {local, Client}}]),
    {ok,
     #{server => Server,
       socket => Socket,
       eventref => EventRef,
       next_transaction => 0,
       transactions => #{}}}.


handle_call(
  Request, From,
  State =
      #{socket := Socket,
        server := Server,
        next_transaction := Transaction,
        transactions := Transactions}) ->
    gen_udp:send(
      Socket,
      {local, Server},
      0,
      jsone:encode(
        Request#{
          <<"transaction">> => integer_to_binary(Transaction, 36)
         })),

    {noreply,
     State#{
       next_transaction => Transaction + 1,
       transactions => Transactions#{Transaction => From}
      }}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(
  {udp, Socket, {local, Server}, 0, Data},
  State = #{socket := Socket, server := Server}) ->
    Msg = jsone:decode(Data),
    handle_message(Msg, State);
handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_message(
  Msg = #{<<"janus">> := <<"event">>},
  State = #{eventref := EventRef}) ->
    gen_event:notify(EventRef, Msg),
    {noreply, State};
handle_message(
  Msg = #{<<"transaction">> := Transaction},
  State = #{transactions := Transactions}) ->
    Trans = binary_to_integer(Transaction, 36),
    case maps:find(Trans, Transactions) of
        error ->
            io:format("NOTRANS: ~p~n", [Msg]),
            {noreply, State};
        {ok, From} ->
            gen_server:reply(From, Msg),
            {noreply,
             State#{
               transactions => maps:remove(Trans, Transactions)
              }}
    end;
handle_message(Msg, State) ->
    io:format("UNKNOWN: ~p~n", [Msg]),
    {noreply, State}.
