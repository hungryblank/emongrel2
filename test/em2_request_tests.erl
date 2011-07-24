-module(em2_request_tests).

-include("emongrel2.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(REQUEST, <<"server_id connection_id path 7:headers,4:body,">>).
-define(SERVER_ID, <<"server_id">>).
-define(CONNECTION_ID, <<"connection_id">>).
-define(HEADERS, <<"headers">>).
-define(PATH, <<"path">>).
-define(BODY, <<"body">>).

build_test() ->
    ServerId = ?SERVER_ID,
    ConnectionId = ?CONNECTION_ID,
    Path = ?PATH,
    Headers = ?HEADERS,
    Body = ?BODY,
    Request = em2_request:build(ServerId,
                                ConnectionId,
                                Path,
                                Headers,
                                Body),
    ?assertEqual(?REQUEST, Request).

parse_test_() ->
    Request = em2_request:parse(?REQUEST),
    [?_assertEqual(?SERVER_ID, Request#em2_request.server_id),
     ?_assertEqual(?CONNECTION_ID, Request#em2_request.connection_id),
     ?_assertEqual(?PATH, Request#em2_request.path),
     ?_assertEqual(?HEADERS, Request#em2_request.headers),
     ?_assertEqual(?BODY, Request#em2_request.body)].
