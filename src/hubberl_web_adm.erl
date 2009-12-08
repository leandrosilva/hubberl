%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Request handler for hubberl's HTTP API for administration interface.

-module(hubberl_web_adm).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([handler_request/2]).

%% External API

handler_request(Request, DocRoot) ->
	Path = Request:get(path),
	Method = Request:get(method),
	
	log_request(Method, Path, DocRoot),

	handler_request(Method, Path, Request, DocRoot).

%% Internal API

handler_request('GET', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request('POST', "/adm/destinations", Request, )
	Data = Request:parse_post(),

	Json = proplists:get_value("json", Data),
	Struct = mochijson2:decode(Json),

	io:format("~n>>> Struct: ~p~n", [Struct]),

	Uri = binary_to_list(struct:get_value(<<"uri">>, Struct)),

	Result = notes:Action(Struct),

	%%io:format("~nResult : ~p~n", [Result]),

	DataOut = mochijson2:encode(Result),

	Request:ok({"application/json", [], [DataOut]});

handler_request('POST', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request('PUT', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request('DELETE', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request(_Method, _Path, Request, _DocRoot) ->
  Request:respond({501, [], []}).

log_request(Method, Path, DocRoot) ->
	io:format("{adm_request,~n"),
	io:format("   Method = ~s,~n",  [Method]),
	io:format("   Path = ~s,~n",    [Path]),
	io:format("   DocRoot = ~s}~n", [DocRoot]).
