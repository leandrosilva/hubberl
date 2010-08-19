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
          {"should accept GET on /admin/destinations to list all destinations",
            fun should_accept_get_to_list_all_destinations/0},
          {"should accept GET on /admin/destinations/{name} to retrieve a destination",
            fun should_accept_get_to_retrieve_a_destination/0},
          {"should accept POST on /admin/destinations to create a destination",
            fun should_accept_post_to_create_a_destination/0},
          {"should not accept PUT on /admin/destinations",
            fun should_not_accept_put/0},
          {"should accept DELETE on /admin/destinations/{name} to remove a destination",
            fun should_accept_delete_to_remove_a_destination/0}
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

should_accept_get_to_list_all_destinations() ->
  AllDestinations = http:request(?RESOURCE_URI),
  ?assertMatch(yet_not_implemented, AllDestinations).

should_accept_get_to_retrieve_a_destination() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).

should_accept_post_to_create_a_destination() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).

should_not_accept_put() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).

should_accept_delete_to_remove_a_destination() ->
  ?assertMatch(yet_not_implemented, yet_not_implemented).
