-module(em2_sockets).

-export([receiver/1,
         sender/1]).

%% @doc This is the socket that the mongrel2 server uses to
%% send requests to the handler
%%
%% Where possible parameters are named exactly like in the original mongrel2
%% configuration that can be found at the following url
%%
%% http://mongrel2.org/static/mongrel2-manual.html#x1-670005.3
%%
%% - send_ident: (string) as per mongrel2 configuration
%% - send_spec: (string) as per mongrel2 configuration
%% - context: the erlzmq context on which the socket should be created
%% @end
-spec sender(Args::list()) ->
    {ok, Socket::erlzmq:erlzmq_socket()} | erlzmq:erlzmq_error().
sender(Args) ->
    Spec     = proplists:get_value(send_spec, Args),
    Identity = proplists:get_value(send_ident, Args),
    Context  = proplists:get_value(context, Args),
    {ok, Socket} = erlzmq:socket(Context, push),
    case setsockopt(Socket, identity, Identity) of
        ok ->
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
%% - recv_ident: (string) as per mongrel2 configuration
%% - recv_spec: (string) as per mongrel2 configuration
%% - context: the erlzmq context on which the socket should be created
%% @end
-spec receiver(Args::list()) ->
    {ok, Socket::erlzmq:erlzmq_socket()} | erlzmq:erlzmq_error().
receiver(Args) ->
    Spec     = proplists:get_value(recv_spec, Args),
    Identity = proplists:get_value(recv_ident, Args),
    Context  = proplists:get_value(context, Args),
    {ok, Socket} = erlzmq:socket(Context, sub),
    case setsockopt(Socket, subscribe, Identity) of
        ok ->
            bind(Socket, Spec);
        Error ->
            Error
    end.

%% @doc attempt erlzmq:setsockopt close socket on failure
setsockopt(Socket, Option, Value) ->
    ok_or_close(Socket,
                erlzmq:setsockopt(Socket, Option, Value)).

%% @doc attempt erlzmq:bind close socket on failure
bind(Socket, Spec) ->
    Result =  ok_or_close(Socket,
                  erlzmq:bind(Socket, Spec)),
    case Result of
        ok    -> {ok, Socket};
        Error -> Error
    end.

%% @doc close the given socket if result is not ok
ok_or_close(Socket, Result) ->
    case Result of
        ok    -> ok;
        Error ->
            erlzmq:close(Socket),
            Error
    end.
