-module(em2_response_tests).
-include_lib("eunit/include/eunit.hrl").

build_test() ->
    Response = <<"server_id 13:connection_id, body">>,
    ?assertEqual({<<"server_id">>, <<"connection_id">>, <<"body">>},
                 em2_response:parse(Response)).
