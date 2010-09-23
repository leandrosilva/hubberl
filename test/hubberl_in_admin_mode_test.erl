%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Functional test module for hubberl when in admin mode.
%%      The reference module is hubberl_web_admin. It's where the request is handled.

-module(hubberl_in_admin_mode_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-import(http_helper, [http_post/2, http_get/1, http_put/2, http_delete/1]).

-include("records.hrl").

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing destinations module
%%
describe_hubberl_test_() ->
  {"hubberl",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"when in admin mode for destinations",
        [
          {"should accept POST on /admin/destinations to create a destination",
            fun should_accept_post_to_create_a_destination/0},
              
          {"should return 200 on /admin/destinations if try to POST a exitent destination",
            fun should_return_200_if_try_to_post_a_existent_destination/0},
              
          {"should return 404 on /admin/destinations if try to POST a invalid resource",
            fun should_return_404_if_try_to_post_a_invalid_resource/0},
              
          {"should accept GET on /admin/destinations to list all destinations",
            fun should_accept_get_to_list_all_destinations/0},
              
          {"should accept GET on /admin/destinations/{name} to retrieve a destination",
            fun should_accept_get_to_retrieve_a_destination/0},
              
          {"should return 404 on /admin/destinations/{name} if try to GET a invalid destination",
            fun should_return_404_if_try_to_get_a_invalid_destination/0},
              
          {"should not accept PUT on /admin/destinations",
            fun should_not_accept_put/0},

          {"should manage its subscriptions",
            [
              {"should accept POST on /admin/subscriptions to create a subscription",
                fun should_accept_post_to_create_a_subscription/0}
            ]},

          {"should accept DELETE on /admin/destinations/{name} to remove a destination",
            fun should_accept_delete_to_remove_a_destination/0},
              
          {"should return 404 on /admin/destinations/{name} if try to DELETE a invalid destination",
            fun should_return_404_if_try_to_delete_a_invalid_destination/0}
        ]},

      {"after all tests",
        {setup, fun after_all/0, []}}
    ]}.

%%
%% Setup
%%

-define(DESTINATIONS_URI, "http://localhost:8000/admin/destinations").

before_all() ->
  hubberl_db:reset(),
  application:start(inets),
  ok.

after_all() ->
  application:stop(inets),
  ok.

%%
%% Scenary: when in admin mode for destinations
%%
%% hubberl_web_adm module
%%

should_accept_post_to_create_a_destination() ->
  HttpResponse = http_post(?DESTINATIONS_URI, "{\"name\":\"payments\", \"description\":\"payments queue\"}"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 201, "Created"}, [_, {"location", "/admin/destinations/payments"}, _, _], []}}, HttpResponse),
  ?assertMatch(yes, hubberl_db:table_exists(payments_destination)).

should_return_200_if_try_to_post_a_existent_destination() ->
  HttpResponse = http_post(?DESTINATIONS_URI, "{\"name\":\"payments\", \"description\":\"payments queue\"}"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"}, [_, _, _], "Object Already Exists"}}, HttpResponse).

should_return_404_if_try_to_post_a_invalid_resource() ->
  HttpResponse = http_post(?DESTINATIONS_URI ++ "/_invalid", ""),
  
  ?assertMatch({ok, {{"HTTP/1.1", 404, "Object Not Found"}, _, _}}, HttpResponse).

should_accept_get_to_list_all_destinations() ->
  HttpResponse = http_get(?DESTINATIONS_URI),

  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [_, _, _, {"content-type", "application/json"}],
                     "[{\"name\":\"payments\",\"description\":\"payments queue\"}]"}},
               HttpResponse).

should_accept_get_to_retrieve_a_destination() ->
  HttpResponse = http_get(?DESTINATIONS_URI ++ "/payments"),

  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [_, _, _, {"content-type", "application/json"}],
                     "{\"name\":\"payments\",\"description\":\"payments queue\"}"}},
               HttpResponse).

should_return_404_if_try_to_get_a_invalid_destination() ->
  HttpResponse = http_get(?DESTINATIONS_URI ++ "/_invalid"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 404, "Object Not Found"}, _, _}}, HttpResponse).

should_not_accept_put() ->
  HttpResponse = http_put(?DESTINATIONS_URI, "{\"name\":\"payments\", \"description\":\"updating payments queue description\"}"),

  ?assertMatch({ok, {{"HTTP/1.1", 501, "Not Implemented"}, _, _}}, HttpResponse).
  
should_accept_post_to_create_a_subscription() ->
  % HttpMethod = post,
  % Uri = ?DESTINATIONS_URI ++ "/payments/subscriptions",
  % Headers = [],
  % ContentType = "application/json",
  % Body = "{\"destination_name\":\"payments\", \"subscriber_uri\":\"http://payments_consumer\"}",
  % HttpOptions = [],
  % Options = [{body_format, string}],
  % 
  % HttpResponse = httpc:request(HttpMethod, {Uri, Headers, ContentType, Body}, HttpOptions, Options),
  % 
  % ?assertMatch({ok, {{"HTTP/1.1", 201, "Created"}, [_, {"location", "/admin/destinations/payments"}, _, _], []}}, HttpResponse),

  ?assertMatch(yet_not_implemented, yet_not_implemented).
  
should_accept_delete_to_remove_a_destination() ->
  HttpResponse = http_delete(?DESTINATIONS_URI ++ "/payments"),

  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"}, [_, _, _], []}}, HttpResponse),
  ?assertMatch(no, hubberl_db:table_exists(payments_destination)).
  
should_return_404_if_try_to_delete_a_invalid_destination() ->
  HttpResponse = http_delete(?DESTINATIONS_URI ++ "/_invalid"),

  ?assertMatch({ok, {{"HTTP/1.1", 404, "Object Not Found"}, _, _}}, HttpResponse).
  