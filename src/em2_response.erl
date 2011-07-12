-module(em2_response).

-export([parse/1]).

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

