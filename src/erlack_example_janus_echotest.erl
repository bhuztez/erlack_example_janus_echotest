-module(erlack_example_janus_echotest).

-export([start/0, start/1, handle/0, event_loop/0]).

start() ->
    start(8000).

start(Port) ->
    start(Port, <<"/tmp/janus.sock">>, <<"/tmp/janus-client.sock">>).

start(Port, Server, Client) ->
    {ok, _} = gen_event:start({local, janus_event}),
    {ok, _} = janus:start(Server, Client, janus_event),
    erlack_debug_server:start(Port, [], {?MODULE, handle, []}).

handle() ->
    ecgi:apply_handler(
      lists:foldr(
        fun erlack_middleware:wrap/2,
        fun () ->
                handle(get(<<"REQUEST_METHOD">>), get(<<"REQUEST_URI">>))
        end,
        [{erlack_reason_phrase, middleware,[]},
         {erlack_content_length, middleware,[]}])).


handle(<<"GET">>, <<"/events/", Path/binary>>) ->
    [Session, Handle] = binary:split(Path, <<"/">>),
    SessionID = binary_to_integer(Session),
    HandleID = binary_to_integer(Handle),

    gen_event:add_sup_handler(janus_event, echotest_event_handler, [self(), SessionID, HandleID]),

    {response,
     200,
     #{<<"Content-Type">> => <<"text/event-stream">>},
     {chunked, {?MODULE, event_loop, []}}};

handle(<<"GET">>, <<"/">>) ->
    handle_static("index.html", <<"text/html; charset=utf-8">>);
handle(<<"GET">>, <<"/webrtc.js">>) ->
    handle_static("webrtc.js", <<"application/javascript; charset=utf-8">>);

handle(<<"POST">>, <<"/">>) ->
    #{<<"janus">> := <<"success">>,
     <<"data">> := #{<<"id">> := SessionID}}
        = gen_server:call(
            janus,
            #{<<"janus">> => <<"create">>}),

    #{<<"janus">> := <<"success">>,
      <<"session_id">> := SessionID,
      <<"data">> := #{<<"id">> := HandleID}} =
        gen_server:call(
          janus,
          #{<<"janus">> => <<"attach">>,
            <<"session_id">> => SessionID,
            <<"plugin">> => <<"janus.plugin.echotest">>}
         ),
    {response, 200, #{}, jsone:encode(#{<<"session_id">> => SessionID, <<"handle_id">> => HandleID})};

handle(<<"POST">>, <<"/message">>) ->
    {ok, Body} = read_body(),
    Request = jsone:decode(Body),

    #{<<"janus">> := <<"ack">>} =
        gen_server:call(
          janus,
          Request#{<<"janus">> => <<"message">>}),
    {response, 200, #{}, <<"OK">>};

handle(<<"POST">>, <<"/trickle">>) ->
    {ok, Body} = read_body(),
    Request = jsone:decode(Body),
    #{<<"janus">> := <<"ack">>} =
        gen_server:call(janus, Request#{<<"janus">> => <<"trickle">>}),
    {response, 200, #{}, <<"OK">>};

handle(<<"POST">>, <<"/keepalive">>) ->
    {ok, Body} = read_body(),
    Request = jsone:decode(Body),
    #{<<"janus">> := <<"ack">>} =
        gen_server:call(janus, Request#{<<"janus">> => <<"keepalive">>}),
    {response, 200, #{}, <<"OK">>};
handle(_, _) ->
    {response, 500, #{}, <<"500 - Internal Server Error">>}.


handle_static(Filename, ContentType) ->
    case file:read_file(filename:join(code:priv_dir(?MODULE), Filename)) of
        {error, _} ->
            not_found();
        {ok, Bin} ->
            { response,
              200,
              #{<<"Content-Type">> => ContentType},
              Bin}
    end.

not_found() ->
    { response,
      404,
      #{<<"Content-Type">> => <<"text/plain">>},
      <<"404 - Not Found\n">>}.

event_loop() ->
    receive
        Event = #{<<"janus">> := <<"event">>} ->
            ecgi_sse:send(jsone:encode(Event))
    after 200 ->
            ok
    end,

    ?MODULE:event_loop().


read_body() ->
    case get(<<"HTTP_CONTENT_LENGTH">>) of
        undefined ->
            {error, length_required};
        Length ->
            ecgi:recv(binary_to_integer(Length))
    end.
