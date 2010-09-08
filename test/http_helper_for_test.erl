%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc helper module for http.

-module(http_helper_for_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([http_post/2, http_get/1, http_put/2, http_delete/1]).

%% External API

http_post(Uri, Body) ->
  http_request(post, Uri, Body).
  
http_get(Uri) ->
  http_request(get, Uri).

http_put(Uri, Body) ->
  http_request(put, Uri, Body).

http_delete(Uri) ->
  http_request(delete, Uri).
  
%% Internal API

http_request(HttpMethod, Uri) ->
  httpc:request(HttpMethod, {Uri, []}, [], []).
  
http_request(HttpMethod, Uri, Body) ->
  Headers = [],
  ContentType = "application/json",
  HttpOptions = [],
  Options = [{body_format, string}],

  httpc:request(HttpMethod, {Uri, Headers, ContentType, Body}, HttpOptions, Options).