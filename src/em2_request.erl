-module(em2_request).

-export([build/5,
         send/2]).

%% @doc build a request
build(ServerId, ConnectionId, Path, Headers, Body) ->
    list_to_binary([ServerId,
                    $ ,
                    ConnectionId,
                    $ ,
                    Path,
                    $ ,
                    netstring(Headers),
                    netstring(Body)]).

%% @doc build a request from the sender socket
-spec send(Socket  :: erlzmq:erlzmq_socket(),
           Request :: binary()) ->
    ok | erlzmq:erlzmq_error().
send(Socket, Request) ->
    erlzmq:send(Socket, Request, [noblock]).


%% @doc build a tstring given its content
-spec netstring(Content::binary()|list()) -> iolist().
netstring(Content) when is_list(Content) ->
    [integer_to_list(length(Content)), $:, Content, $,];

netstring(Content) when is_binary(Content) ->
    [integer_to_list(size(Content)), $:, Content, $,].
