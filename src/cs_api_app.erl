%%%-------------------------------------------------------------------
%% @doc cs_api public API
%% @end
%%%-------------------------------------------------------------------

-module(cs_api_app).

-behaviour(application).

-export([start/2, start/0, stop/1]).

start(_StartType, _StartArgs) ->
    start().

start() ->
    cs_api_sup:start_link().

stop(_State) ->
    ok.
