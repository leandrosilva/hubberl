%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc test module for hubberl.

-module(hubberl_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include("records.hrl").

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing destinations module
%%
describe_hubberl_test_() ->
  {"hubberl",
    [
      {"before tests",
        {setup, fun before_all/0, []}},
        
      {"when in admin mode",
        [
          {"should accept POST on /admin/destinations to create a destination",
            fun should_accept_post_to_create_a_destination/0},
          {"should accept GET on /admin/destinations to list all destinations",
            fun should_accept_get_to_list_all_destinations/0},
          {"should accept GET on /admin/destinations/{name} to retrieve a destination",
            fun should_accept_get_to_retrieve_a_destination/0},
          {"should return 404 on /admin/destinations/{name} when GET a destination that doesn't exist",
            fun should_return_404_when_get_a_destination_that_doesnt_exist/0},
          {"should not accept PUT on /admin/destinations",
            fun should_not_accept_put/0},
          {"should accept DELETE on /admin/destinations/{name} to remove a destination",
            fun should_accept_delete_to_remove_a_destination/0},
          {"should return 404 on /admin/destinations/{name} when DELETE a destination that doesn't exist",
            fun should_return_404_when_delete_a_destination_that_doesnt_exist/0}
        ]},
              
      {"after tests",
        {setup, fun after_all/0, []}}
    ]}.

%%
%% Setup
%%

-define(RESOURCE_URI, "http://localhost:8000/admin/destinations").

before_all() ->
  hubberl_db:reset(),
  application:start(inets),
  ok.

after_all() ->
  application:stop(inets),
  ok.

%%
%% Scenary: when in admin mode
%%
%% hubberl_web_adm module
%%

should_accept_post_to_create_a_destination() ->
  HttpMethod = post,
  Headers = [],
  ContentType = "application/x-www-form-urlencoded",
  Body = "destination={\"name\":\"payments\", \"type\":\"queue\", \"description\":\"payments queue\"}",
  HttpOptions = [],
  Options = [{body_format, string}],

  HttpResponse = http:request(HttpMethod, {?RESOURCE_URI, Headers, ContentType, Body}, HttpOptions, Options),
  
  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [_, _, _, {"content-type", "application/json"}],
                     "{\"status\":\"created\",\"name\":\"payments\",\"type\":\"queue\",\"description\":\"payments queue\"}"}},
               HttpResponse).

%
% Example:
%
% {ok,{{"HTTP/1.1",200,"OK"},
%      [{"date",
%        "Thu, 02 Sep 2010 04:29:04 GMT"},
%       {"server",
%        "MochiWeb/1.0 (Any of you quaids got a smint?)"},
%       {"content-length","2"},
%       {"content-type",[...]}],
%      "[]"}}
%

should_accept_get_to_list_all_destinations() ->
  AllDestinations = http:request(?RESOURCE_URI),

  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [_, _, _, {"content-type", "application/json"}],
                     "[{\"name\":\"payments\",\"type\":\"queue\",\"description\":\"payments queue\"}]"}},
               AllDestinations).

should_accept_get_to_retrieve_a_destination() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).

should_return_404_when_get_a_destination_that_doesnt_exist() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).

should_not_accept_put() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).

should_accept_delete_to_remove_a_destination() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).

should_return_404_when_delete_a_destination_that_doesnt_exist() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).
  