-module(em2_request).

-export([build/5]).

%% @doc build a request
build(ServerId, ConnectionId, Path, Headers, Body) ->
    [ServerId,
     $ ,
     ConnectionId,
     $ ,
     Path,
     $ ,
     netstring(Headers),
     netstring(Body)].

%% @doc build a tstring given its content
-spec netstring(Content::binary()|list()) -> iolist().
netstring(Content) when is_list(Content) ->
    [integer_to_list(length(Content)), $:, Content, $,];

netstring(Content) when is_binary(Content) ->
    [integer_to_list(size(Content)), $:, Content, $,].
