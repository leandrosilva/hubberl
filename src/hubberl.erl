%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(hubberl).
-author('author <author@example.com>').
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
