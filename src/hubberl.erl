%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Startup for hubberl application.

-module(hubberl).
-author('Leandro Silva <leandrodoze@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the hubberl server.
start() ->
    hubberl_deps:ensure(),
    ensure_started(crypto),
    application:start(hubberl).

%% @spec stop() -> ok
%% @doc Stop the hubberl server.
stop() ->
    Res = application:stop(hubberl),
    application:stop(crypto),
    Res.
