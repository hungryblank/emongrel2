-module(em2_response).

-export([parse/1,
         send/2,
         recv/1]).

%% @doc parse a response
-spec parse(Response::binary()) -> {Identifier::binary(),
                                    ConnectionId::binary(),
                                    Body::binary()}.
parse(Response) ->
    [Identifier, Rest] = binary:split(Response, <<$ >>),
    [Size, More] = binary:split(Rest, <<$: >>),
    SizeI = list_to_integer(binary_to_list(Size)),
    <<ConnectionId:SizeI/binary, $,, $ , Body/binary>> = More,
    {Identifier, ConnectionId, Body}.

%% @doc receive a response from receiver socket
-spec send(Socket::erlzmq:erlzmq_socket(), Response::binary()) ->
    ok | erlzmq:erlzmq_error().
send(Socket, Response) ->
    erlzmq:send(Socket, Response).

%% @doc receive a response from receiver socket
-spec recv(Socket::erlzmq:erlzmq_socket()) ->
    {ok, Response::binary()} | erlzmq:erlzmq_error().
recv(Socket) ->
    erlzmq:recv(Socket).
