%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Request handler for hubberl's HTTP API for administration interface.

-module(hubberl_web_admin).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([handle_request/4]).
-import(http_helper, [http_response/2, http_response/3, http_response/4]).

%% External API

handle_request('POST', "/admin/destinations", Request, _DocRoot) ->
  Body = Request:recv_body(),
  Input = struct:from_json(Body),
  
  case destinations:create(Input) of
    already_exists ->
      http_response(Request, 200, "Object Already Exists");
    created ->
      Name = struct:get_value(<<"name">>, Input),

      http_response(Request, 201, [{"Location", "/admin/destinations/" ++ Name}], [])
  end;

handle_request('POST', _Path, Request, _DocRoot) ->
  http_response(Request, 404);

handle_request('GET', "/admin/destinations", Request, _DocRoot) ->
  Destinations = destinations:read_all(),
  Output = struct:to_json(Destinations),

  http_response(Request, 200, [{"Content-Type", "application/json"}], Output);

handle_request('GET', "/admin/destinations/" ++ Name, Request, _DocRoot) ->
  Input = struct:new("name", Name),

  case destinations:read(Input) of
    not_found ->
      http_response(Request, 404);
    Destination ->
      Output = struct:to_json(Destination),
      http_response(Request, 200, [{"Content-Type", "application/json"}], Output)
  end;

handle_request('GET', _Path, Request, _DocRoot) ->
  http_response(Request, 404);

handle_request('DELETE', "/admin/destinations/" ++ Name, Request, _DocRoot) ->
  Input = struct:new("name", Name),

  case destinations:delete(Input) of
    not_found ->
      http_response(Request, 404);
    deleted ->
      http_response(Request, 200)
  end;

handle_request('DELETE', _Path, Request, _DocRoot) ->
  http_response(Request, 404);

handle_request(_Method, _Path, Request, _DocRoot) ->
  http_response(Request, 501).
