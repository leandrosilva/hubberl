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

handler_request('POST', "/adm/destinations", Request, _DocRoot) ->
	Destination = struct:from_json(Request, "destination"),
	io:format("~n--- Destination: ~p~n", [Destination]),

	NewDestination = destinations:create(Destination),
	io:format("~n--- NewDestination: ~p~n", [NewDestination]),

	JsonOutput = struct:to_json(NewDestination),
	io:format("~n--- JsonOutput : ~p~n", [JsonOutput]),

	Request:ok({"application/json", [], [JsonOutput]});

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
