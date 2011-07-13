-module(em2_receiver).

%%
%% API
%%

-export([loop/1]).


%% @doc run a receiver handler using the provided socket casting messages to
%% the provided gen_server identifier gen server needs to handle
%% cast {em2_reply, Message}
loop(Socket) ->
    {ok, Message} = erlzmq:recv(Socket),
    error_logger:info_msg("~p ~n", [Message]),
    loop(Socket).
