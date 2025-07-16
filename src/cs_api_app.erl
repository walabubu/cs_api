%%%-------------------------------------------------------------------
%% @doc cs_api public API
%% @end
%%%-------------------------------------------------------------------

-module(cs_api_app).

-behaviour(application).

-export([start/2, start/0, stop/1]).

-define(DEF_PORT, 8000).

start(_StartType, _StartArgs) ->
    start().

start() ->
    {ok, _Pid} = inets:start(httpd, [
        {port, get_port()},
        {server_root,"/tmp"},
        {document_root,"/tmp"},
        {bind_address, {127, 0, 0, 1}},
        {erl_script_alias, {"/cs", [cs_api_http]}}
    ]),
    cs_api_sup:start_link().

stop(_State) ->
    ok = inets:stop(httpd, {{127, 0, 0, 1}, get_port()}),
    ok.

get_port() ->
    application:get_env(cs_api, port, ?DEF_PORT).
