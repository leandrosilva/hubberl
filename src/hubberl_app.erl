%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Callbacks for the hubberl application.

-module(hubberl_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for hubberl.
start(_Type, _StartArgs) ->
    hubberl_deps:ensure(),
    hubberl_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for hubberl.
stop(_State) ->
    ok.
