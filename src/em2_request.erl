-module(em2_request).

-export([build/5,
         send/2,
         recv/1]).

%% @doc build a request
-spec build(ServerId :: string()|binary(),
            ConnectionId :: string()|binary(),
            Path :: string()|binary(),
            Headers :: string()|binary(),
            Body :: string()|binary()) ->
    Request :: binary().
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

%% @doc receive a request from the acceptor socket
-spec recv(Socket  :: erlzmq:erlzmq_socket()) ->
    Request::binary() | erlzmq:erlzmq_error().
recv(Socket) ->
    erlzmq:recv(Socket).

%% @doc build a netstring given its content
-spec netstring(Content::binary()|list()) -> iolist().
netstring(Content) when is_list(Content) ->
    [integer_to_list(length(Content)), $:, Content, $,];

netstring(Content) when is_binary(Content) ->
    [integer_to_list(size(Content)), $:, Content, $,].
