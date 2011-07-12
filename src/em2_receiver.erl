-module(em2_receiver).

%%
%% API
%%

-export([socket/1,
         run/2]).

%% @doc initialize a socket for the receiver
-spec socket(Args::list()) -> {ok, Socket::erlzmq:erlzmq_socket()}.
socket(Args) ->
    Address = proplists:get_value(address, Args),
    Identity = proplists:get_value(identity, Args),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, sub),
    ok = erlzmq:setsockopt(Socket, subscribe, Identity),
    ok = erlzmq:bind(Socket, Address),
    {ok, Socket}.

%% @doc convenience function to start the loop
run(Socket, Pid) ->
    loop(Socket, Pid).

%% @doc run a receiver handler using the provided socket casting messages to
%% the provided gen_server identifier gen server needs to handle
%% cast {em2_reply, Message}
loop(Socket, Pid) ->
    {ok, Message} = erlzmq:recv(Socket),
    gen_server:cast(Pid, {em2_reply, Message}),
    loop(Socket, Pid).
