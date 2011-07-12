-module(em2_sender).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    Address = proplists:get_value(address, Args),
    Identity = proplists:get_value(identity, Args),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, push),
    ok = erlzmq:setsockopt(Socket, identity, Identity),
    ok = erlzmq:bind(Socket, Address),
    {ok, {Socket, Context}}.

%% send a request to the push socket on which handlers are listening
handle_call({request, Request}, _From, {Socket, Context}) ->
    case erlzmq:send(Socket, Request, [noblock]) of
        ok -> {reply, ok, {Socket, Context}};
        Error -> {reply, {error, Error}, {Socket, Context}}
    end;

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {Socket, Context}) ->
    erlzmq:close(Socket),
    erlzmq:term(Context),
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

