%% A sample mongrel2 server written in erlang
%% just compile this module and run in the erlang console
%%
%% em2_server:start_link()
%%
%% this server issues a request in an infinite loop and echoes back the received
%% responses.
%% this server should work with any handler configured to handle request from a
%% mongrel2 server configured accordingly to configuration file provided in this
%% directory

-module(em2_server).

-export([start_link/0,
         send_loop/2,
         receive_loop/1]).

-include("emongrel2.hrl").

-define(SEND_SPEC, "tcp://127.0.0.1:9997").
-define(RECV_SPEC, "tcp://127.0.0.1:9996").
-define(SERVER_IDENTITY, "erlang_server").
% some ready made json encoded headers
-define(HEADERS, <<"{\"PATH\":\"/\",\"x-forwarded-for\":\"127.0.0.1\",\"cache-control\":\"max-age=0\",\"accept-language\":\"en-US,en;q=0.8\",\"accept-encoding\":\"gzip,deflate,sdch\",\"connection\":\"keep-alive\",\"accept-charset\":\"ISO-8859-1,utf-8;q=0.7,*;q=0.3\",\"accept\":\"application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\",\"user-agent\":\"Mozilla/5.0 (X11; U; Linux i686; en-US) AppleWebKit/533.2 (KHTML, like Gecko) Chrome/5.0.342.7 Safari/533.2\",\"host\":\"127.0.0.1:8080\",\"METHOD\":\"GET\",\"VERSION\":\"HTTP/1.1\",\"URI\":\"/\",\"PATTERN\":\"/\"}">>).

start_link() ->
    % create a 0MQ context
    {ok, Context} = erlzmq:context(),
    % initialize the sender socket
    {ok, Sender} = em2_sockets:sender(Context,
                                      [{send_spec, ?SEND_SPEC},
                                       {send_ident, ?SERVER_IDENTITY}]),
    % initialize the receiver socket
    {ok, Receiver} = em2_sockets:receiver(Context,
                                          [{recv_spec, ?RECV_SPEC},
                                                                                                          {recv_ident, ?SERVER_IDENTITY}]),
    
    SenderPid = spawn_link(?MODULE, send_loop, [Sender, 0]),
    ReceiverPid = spawn_link(?MODULE, receive_loop, [Receiver]),
    {ok, [SenderPid, ReceiverPid]}.

send_loop(Sender, ConnectionId) ->
    NextConnectionId = ConnectionId + 1,
    Request = #em2_request{server_id = ?SERVER_IDENTITY,
                           connection_id = integer_to_list(NextConnectionId),
                           path = "/",
                           headers = ?HEADERS,
                           body = <<"">>},
    em2_request:send(Sender, em2_request:build(Request)),
    send_loop(Sender, NextConnectionId).

receive_loop(Receiver) ->
    {ok, Response} = em2_response:recv((Receiver)),
    error_logger:info_msg("~p ~n", [em2_response:parse(Response)]),
    receive_loop(Receiver).
