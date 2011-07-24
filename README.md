Emongrel2
=========

Erlang toolkit to work with or emulate mongrel2

Rationale
---------

Mongrel2 http://mongrel2.org is an innovative web server that make use of
zeromq http://zeromq.org sockets to enable communication between the webserver
and the application server.
This library provides all what is needed to build mongrel2 handlers using
erlang.
The library also provides all what is need in order to build a web server
compatible with mongrel2.

Documentation
---------

emongrel2 provides 3 erlang modules

- em2_request
  build/parse send/receive mongrel2 requests
- em2_response
  build/parse send/receive mongrel2 responses

- em2_sockets functions to safely start the zeromq sockets used by mongrel2
  handler and server

TODO
---------

- Provide example for builing erlang handlers
- Provide examples for builing erlang mongrel2 server and illustrate how to
  connect it with python/ruby handlers available on github

Copyright
---------

Copyright (c) 2011 Paolo Negri - hungryblank. See LICENSE for details.

