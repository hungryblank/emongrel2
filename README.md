Emongrel2
=========

Howto
---------

    {ok, Receiver} = em2_gen_receive:start_link([{address, "tcp://127.0.0.1:9996"}, {identity, "9539ED88-1B33-4D19-A9F9-283E5BF11AC7"}]).
    {ok, Sender} = em2_sender:start_link([{address, "tcp://127.0.0.1:9997"}, {identity, "9539ED88-1B33-4D19-A9F9-283E5BF11AC7"}]).
    gen_server:call(Sender,
                    {request, list_to_binary(em2_request:build(<<"9539ED88-1B33-4D19-A9F9-283E5BF11AC7">>, <<"123">>, <<"/">>, <<"{\"PATH\":\"/foo\",\"x-forwarded-for\":\"127.0.0.1\",\"accept-language\":\"en-US,en;q=0.8\",\"accept-encoding\":\"gzip,deflate,sdch\",\"connection\":\"keep-alive\",\"accept-charset\":\"ISO-8859-1,utf-8;q=0.7,*;q=0.3\",\"accept\":\"application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\",\"user-agent\":\"Mozilla/5.0 (X11; U; Linux i686; en-US) AppleWebKit/533.2 (KHTML, like Gecko) Chrome/5.0.342.7 Safari/533.2\",\"host\":\"localhost:8080\",\"METHOD\":\"GET\",\"VERSION\":\"HTTP/1.1\",\"URI\":\"/foo\",\"PATTERN\":\"/\"}">>, <<"body">>))}).

Copyright
---------

Copyright (c) 2011 Paolo Negri - hungryblank. See LICENSE for details.

