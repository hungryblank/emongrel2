Emongrel2
=========

Erlang toolkit to work with or emulate mongrel2

Rationale
---------

Mongrel2 http://mongrel2.org is an innovative web server that make use of
zeromq http://zeromq.org sockets to enable communication between the webserver
and the application server.

This library provides helpers that simplify the creation of mongrel2 handlers
written in erlang.

This library also provides helpers that simplify the creation of mongrel2
compatible servers.

Documentation
---------

emongrel2 provides 3 erlang modules

- em2_request
  build/parse send/receive mongrel2 requests
- em2_response
  build/parse send/receive mongrel2 responses

- em2_sockets functions to safely start the zeromq sockets used by mongrel2
  handler and server

Examples
---------

Examples of usage of this library are provided in the example directory

TODO
---------

- Provide support for tnetstring protocol

Copyright
---------

Copyright (c) 2011 Paolo Negri - hungryblank. See LICENSE for details.

