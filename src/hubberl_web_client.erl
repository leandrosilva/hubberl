%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Request handler for hubberl's HTTP API for client interface.

-module(hubberl_web_client).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([handle_request/2]).

%% External API

handle_request(Request, DocRoot) ->
	Path = Request:get(path),
	Method = Request:get(method),
	
	hubberl_log:log_request(Method, Path, DocRoot),

	handle_request(Method, Path, Request, DocRoot).

%% Internal API

handle_request('GET', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request('POST', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request('PUT', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request('DELETE', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request(_Method, _Path, Request, _DocRoot) ->
  Request:respond({501, [], []}).
