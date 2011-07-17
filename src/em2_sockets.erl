-module(em2_sockets).

-export([receiver/2,
         sender/2,
         acceptor/2,
         responder/2]).

%% @doc This is the socket that the mongrel2 server uses to
%% send requests to the handler
%%
%% Where possible parameters are named exactly like in the original mongrel2
%% configuration that can be found at the following url
%%
%% http://mongrel2.org/static/mongrel2-manual.html#x1-670005.3
%%
%% - Context: the erlzmq context on which the socket should be created
%%
%% Mandatory parameters
%%
%% - send_ident: (string) as per mongrel2 configuration
%%               example: "9539ED88-1B33-4D19-A9F9-283E5BF11AC7"
%% - send_spec: (string) as per mongrel2 configuration
%%              example: "tcp://127.0.0.1:9997"
%% @end
-spec sender(Context::erlzmq:erlzmq_context(), Args::list()) ->
    {ok, Socket::erlzmq:erlzmq_socket()} | erlzmq:erlzmq_error().
sender(Context, Args) ->
    Spec     = proplists:get_value(send_spec, Args),
    Identity = proplists:get_value(send_ident, Args),
    {ok, Socket} = erlzmq:socket(Context, push),
    case setsockopt(Socket, identity, Identity) of
        {ok, Socket} ->
            bind(Socket, Spec);
        Error ->
            Error
    end.

%% @doc This is the socket that the mongrel2 server uses to
%% receive responses from the handler
%%
%% Where possible parameters are named exactly like in the original mongrel2
%% configuration that can be found at the following url
%%
%% http://mongrel2.org/static/mongrel2-manual.html#x1-670005.3
%%
%% - Context: the erlzmq context on which the socket should be created
%%
%% Mandatory parameters
%%
%% - recv_ident: (string) as per mongrel2 configuration
%%               example: "9539ED88-1B33-4D19-A9F9-283E5BF11AC7"
%% - recv_spec: (string) as per mongrel2 configuration
%%              example: "tcp://127.0.0.1:9996"
%% @end
-spec receiver(Context::erlzmq:erlzmq_context(), Args::list()) ->
    {ok, Socket::erlzmq:erlzmq_socket()} | erlzmq:erlzmq_error().
receiver(Context, Args) ->
    Spec     = proplists:get_value(recv_spec, Args),
    Identity = proplists:get_value(recv_ident, Args),
    {ok, Socket} = erlzmq:socket(Context, sub),
    case setsockopt(Socket, subscribe, Identity) of
        {ok, Socket} ->
            bind(Socket, Spec);
        Error ->
            Error
    end.

%% @doc Socket used by the mongrel2 handler to receive requests from the server
%%
%% - Context: the erlzmq context on which the socket should be created
%%
%% Mandatory parameters
%%
%% - accp_spec: (string) must be the same value of send_spec of the sender
%%              socket on the server
%%              example: "tcp://127.0.0.1:9996"
%%
%% @end
-spec acceptor(Context::erlzmq:erlzmq_context(), Args::list()) ->
    {ok, Socket::erlzmq:erlzmq_socket()} | erlzmq:erlzmq_error().
acceptor(Context, Args) ->
    Spec     = proplists:get_value(accp_spec, Args),
    {ok, Socket} = erlzmq:socket(Context, pull),
    connect(Socket, Spec).

%% @doc Socket used by the mongrel2 handler to send responses to the server
%%
%% - Context: the erlzmq context on which the socket should be created
%%
%% Mandatory parameters
%%
%% - resp_spec: (string) must be the same value of send_spec of the sender
%%              socket on the server
%%              example: "tcp://127.0.0.1:9997"
%%
%% - resp_ident: (string) unique identifier for the handler
%%               example: "C1C277E0-921B-012E-B0ED-000C2968853B"
%%
%% @end
-spec responder(Context::erlzmq:erlzmq_context(), Args::list()) ->
    {ok, Socket::erlzmq:erlzmq_socket()} | erlzmq:erlzmq_error().
responder(Context, Args) ->
    Spec     = proplists:get_value(resp_spec, Args),
    Identity = proplists:get_value(resp_ident, Args),
    {ok, Socket} = erlzmq:socket(Context, pub),
    case setsockopt(Socket, identity, Identity) of
        {ok, Socket} ->
            connect(Socket, Spec);
        Error ->
            Error
    end.

%% @doc attempt erlzmq:setsockopt close socket on failure
setsockopt(Socket, Option, Value) ->
    ok_or_close(Socket,
                erlzmq:setsockopt(Socket, Option, Value)).

%% @doc attempt erlzmq:bind close socket on failure
bind(Socket, Spec) ->
    ok_or_close(Socket,
                erlzmq:bind(Socket, Spec)).

%% @doc attempt erlzmq:connect close socket on failure
connect(Socket, Spec) ->
    ok_or_close(Socket,
                erlzmq:connect(Socket, Spec)).

%% @doc close the given socket if result is not ok
ok_or_close(Socket, Result) ->
    case Result of
        ok    -> {ok, Socket};
        Error ->
            error_logger:info_msg("closing ~p ~n", [Socket]),
            erlzmq:close(Socket),
            Error
    end.
