-module(em2_request).

-include("emongrel2.hrl").

-export([build/5,
         build/1,
         send/2,
         parse/1,
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
                    netstring:encode(Headers),
                    netstring:encode(Body)]).

%% @doc build a request from a record
-spec build(#em2_request{}) -> Request :: binary().
build(#em2_request{server_id = ServerId,
                   connection_id = ConnectionId,
                   path = Path,
                   headers = Headers,
                   body = Body}) ->
build(ServerId, ConnectionId, Path, Headers, Body).

-spec parse(Data::binary()) ->
    #em2_request{}.
parse(Data) ->
    [NetString, Path, ConnectionId, ServerId] = split(Data, 3, []),
    {Header, Rest} = netstring:decode(NetString),
    {Body, <<"">>} = netstring:decode(Rest),
    #em2_request{server_id = ServerId,
                 connection_id = ConnectionId,
                 path = Path,
                 headers = Header,
                 body = Body}.

%% @doc build a request from the sender socket
-spec send(Socket::erlzmq:erlzmq_socket(),
           Request::binary()) ->
    ok | erlzmq:erlzmq_error().
send(Socket, Request) ->
    erlzmq:send(Socket, Request, [noblock]).

%% @doc receive a request from the acceptor socket
-spec recv(Socket::erlzmq:erlzmq_socket()) ->
    Request::binary() | erlzmq:erlzmq_error().
recv(Socket) ->
    erlzmq:recv(Socket).

split(Data, 0, Acc) ->
    [Data | Acc];
split(Data, Num, Acc) ->
    [H, T] = binary:split(Data, <<" ">>),
    split(T, Num - 1, [H | Acc]).
