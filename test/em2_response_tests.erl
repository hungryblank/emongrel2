-module(em2_response_tests).
-include_lib("eunit/include/eunit.hrl").

-define(RESPONSE, <<"server_id 13:connection_id, body">>).
-define(SERVER_ID, <<"server_id">>).
-define(CONNECTION_ID, <<"connection_id">>).
-define(BODY, <<"body">>).

parse_test() ->
    Response = ?RESPONSE,
    ?assertEqual({?SERVER_ID, [?CONNECTION_ID], ?BODY},
                 em2_response:parse(Response)).

build_test() ->
    Response = em2_response:build(?SERVER_ID, [?CONNECTION_ID], ?BODY),
    ?assertEqual(?RESPONSE, Response).

multiple_connections_test() ->
    Response = em2_response:build(?SERVER_ID, [?CONNECTION_ID, <<"second_connection">>], ?BODY),
    Expected = list_to_binary([?SERVER_ID,
                                 $ ,
                                 "31:connection_id second_connection",
                                 $,,
                                 $ ,
                                 ?BODY]),
    ?assertEqual(Expected, Response).
