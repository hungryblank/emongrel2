-module(em2_response).

-export([parse/1,
         send/2,
         build/3,
         recv/1]).

%% @doc parse a response
-spec parse(Response::binary()) -> {Identifier::binary(),
                                    ConnectionIds::list(binary()),
                                    Body::binary()}.
parse(Response) ->
    [ServerId, Rest] = binary:split(Response, <<$ >>),
    {ConnectionIds, <<$ , Body/binary>>} = netstring:decode(Rest),
    {ServerId,
     binary:split(ConnectionIds, <<$ >>, [global]),
     Body}.

%% @doc build a response from its components
-spec build(ServerId::binary(),
            ConnectionIds::list(binary()),
            Body::binary()) ->
    binary().
build(ServerId, ConnectionIds, Body) ->
    list_to_binary([ServerId,
                    $ ,
                    netstring:encode(join(ConnectionIds)),
                    $ ,
                    Body]).

%% @doc send a response from responder socket
-spec send(Socket::erlzmq:erlzmq_socket(), Response::binary()) ->
    ok | erlzmq:erlzmq_error().
send(Socket, Response) ->
    erlzmq:send(Socket, Response).

%% @doc receive a response from receiver socket
-spec recv(Socket::erlzmq:erlzmq_socket()) ->
    {ok, Response::binary()} | erlzmq:erlzmq_error().
recv(Socket) ->
    erlzmq:recv(Socket).

join([H]) ->
    H;
join([H | T]) ->
    list_to_binary([H | [[$ , X] || X <- T]]).
