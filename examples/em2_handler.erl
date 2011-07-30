%% A sample mongrel2 handler written in erlang
%% just compile this module and run in the erlang console
%%
%% em2_handler:start_link()
%%
%% it simply echoes the request back as an answer.
%% In this example the acceptor and responder sockets
%% are handled fromt he same erlang process, in a real world implementation it's
%% better to handle each socket in its own process in order to take advantage
%% from the request/response loop decoupling
%%
%% This handler will respond to requests issued by a mongrel server started
%% using the configuration file provided in this directory.
%%

-module(em2_handler).

-export([start_link/0,
         loop/2]).

-include("emongrel2.hrl").

-define(SEND_SPEC, "tcp://127.0.0.1:9997").
-define(RECV_SPEC, "tcp://127.0.0.1:9996").
-define(HANDLER_IDENTITY, "erlang_handler").

start_link() ->
    % create a 0MQ context
    {ok, Context} = erlzmq:context(),
    % initialize the acceptor socket
    {ok, Acceptor} = em2_sockets:acceptor(Context,
                                          [{accp_spec, ?SEND_SPEC}]),
    % initialize the responder socket
    {ok, Responder} = em2_sockets:responder(Context,
                                            [{resp_spec, ?RECV_SPEC},
                                                                                                            {resp_ident, ?HANDLER_IDENTITY}]),
    spawn_link(?MODULE, loop, [Acceptor, Responder]).

loop(Acceptor, Responder) ->
    % receive the request
    {ok, Request} = em2_request:recv(Acceptor),
    % send the response back for the given response
    respond(em2_request:parse(Request), Responder),
    % loop back to receive the next request
    loop(Acceptor, Responder).

respond(#em2_request{server_id = ServerId,
                    connection_id = ConnectionId,
                    path = Path,
                    headers = Headers,
                    body = Body},
        Responder) ->
    ResponseBody = list_to_binary([Path, $;, Headers, $;, Body]),
    Response = em2_response:build(ServerId, [ConnectionId], http_response(ResponseBody)),
    em2_response:send(Responder, Response).

%% a utility function to build a minimal HTTP response
http_response(Body) ->
    ContentLength = integer_to_list(size(Body)),
    Msg = list_to_binary([<<"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length:">>,
                          ContentLength,
                          <<"\r\n\r\n">>,
                          Body]),
    % print the response
    error_logger:info_msg("~p ~n", [Msg]),
    Msg.
