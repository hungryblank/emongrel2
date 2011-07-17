-module(em2_sockets_tests).
-include_lib("eunit/include/eunit.hrl").

push_pull_test() ->
    {ok, Context} = erlzmq:context(),
    SendSpec = "tcp://127.0.0.1:9997",
    ServerIdent = "server_id",
    %% setup mongrel2 server sockets
    {ok, Sender} = em2_sockets:sender(Context,
                                      [{send_ident, ServerIdent},
                                       {send_spec, SendSpec}]),
    {ok, Acceptor} = em2_sockets:acceptor(Context,
                                          [{accp_spec, SendSpec}]),
    Request = <<"foo">>,
    timer:sleep(10),
    ?assertEqual(ok, em2_request:send(Sender, Request)),
    ?assertEqual({ok, Request}, em2_request:recv(Acceptor)).

pub_sub_test() ->
    {ok, Context} = erlzmq:context(),
    HandlerIdent = "handler_id",
    RecvSpec = "tcp://127.0.0.1:9996",
    ServerIdent = "server_id",
    {ok, Receiver} = em2_sockets:receiver(Context,
                                          [{recv_ident, ServerIdent},
                                           {recv_spec, RecvSpec}]),
    {ok, Responder} = em2_sockets:responder(Context,
                                            [{resp_spec, RecvSpec},
                                             {resp_ident, HandlerIdent}]),
    Response = list_to_binary([ServerIdent, $ , <<"foo">>]),
    ?assertEqual(ok, em2_response:send(Responder, Response)),
    ?assertEqual({ok, Response}, em2_response:recv(Receiver)).
