%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Request handler for hubberl's HTTP API for administration interface.

-module(hubberl_web_admin).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([handle_request/4]).

%% External API

handle_request('GET', "/admin/destinations", Request, _DocRoot) ->
  Destinations = destinations:read_all(),
  Output = struct:to_json(Destinations),

  Request:ok({"application/json", [], [Output]});

handle_request('GET', "/admin/destinations/" ++ Name, Request, _DocRoot) ->
  Input = struct:new("name", Name),

  Destination = destinations:read(Input),
  Output = struct:to_json(Destination),

  Request:ok({"application/json", [], [Output]});

handle_request('GET', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request('POST', "/admin/destinations", Request, _DocRoot) ->
  Data = Request:parse_post(),
  Input = struct:from_json("destination", Data),
  
  NewDestination = destinations:create(Input),
  Output = struct:to_json(NewDestination),

  Request:ok({"application/json", [], [Output]});

handle_request('POST', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request('PUT', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request('DELETE', "/admin/destinations/" ++ Name, Request, _DocRoot) ->
  Input = struct:new("name", Name),

  Destination = destinations:delete(Input),
  Output = struct:to_json(Destination),

  Request:ok({"application/json", [], [Output]});

handle_request('DELETE', _Path, Request, _DocRoot) ->
  Request:not_found();

handle_request(_Method, _Path, Request, _DocRoot) ->
  Request:respond({501, [], []}).
