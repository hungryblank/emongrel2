-module(em2_request_tests).
-include_lib("eunit/include/eunit.hrl").

build_test() ->
    ServerId = "server_id",
    ConnectionId = "connection_id",
    Path = <<"path">>,
    Headers = <<"headers">>,
    Body = <<"body">>,
    Request = em2_request:build(ServerId,
                                 ConnectionId,
                                 Path,
                                 Headers,
                                 Body),
    ?assertEqual(<<"server_id connection_id path 7:headers,4:body,">>,
                 Request).
