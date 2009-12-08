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
	
	hubberl_log:log_request(Method, Path, DocRoot),

	handler_request(Method, Path, Request, DocRoot).

%% Internal API

handler_request('GET', "/adm/destinations", Request, _DocRoot) ->
  Destinations = destinations:read_all(),
	Output = struct:to_json(Destinations),

	Request:ok({"application/json", [], [Output]});

handler_request('GET', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request('POST', "/adm/destinations", Request, _DocRoot) ->
	Data = Request:parse_post(),
	Input = struct:from_json("destination", Data),

	NewDestination = destinations:create(Input),
	Output = struct:to_json(NewDestination),

	Request:ok({"application/json", [], [Output]});

handler_request('POST', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request('PUT', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request('DELETE', _Path, Request, _DocRoot) ->
  Request:not_found();

handler_request(_Method, _Path, Request, _DocRoot) ->
  Request:respond({501, [], []}).
