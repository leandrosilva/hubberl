%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Web server for hubberl.

-module(hubberl_web).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
           ?MODULE:loop(Req, DocRoot)
         end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Request, DocRoot) ->
  Path = Request:get(path),
  case Path of
    "/admin" ++ _ ->
      hubberl_web_admin:handle_request(Request, DocRoot);
      
    "/client" ++ _ ->
      hubberl_web_client:handle_request(Request, DocRoot);
      
    _ ->
      "/" ++ ShortPath = Path,
      Request:serve_file(ShortPath, DocRoot)
  end.

%% Internal API

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
