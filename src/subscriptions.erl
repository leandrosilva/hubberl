%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Friendly API for hubberl's business operation on subscriptions for destinations.
%%      Its API was projected  to works fine (and facilitate, of course) when dealing
%%      with mochiwebjson2 struct.

-module(subscriptions).
-author('Leandro Silva <leandrodoze@gmail.com>').

-compile(export_all).

-include("records.hrl").

create(S) ->
  DestinationName = struct:get_value(<<"destination_name">>, S),
  SubscriberUri  = struct:get_value(<<"subscriber_uri">>, S),
  
  {atomic, ok} = hubberl_db:write({subscription, DestinationName, SubscriberUri}),
  
  _Created = struct:set_value(<<"status">>, created, S).

read(S) ->
  DestinationName = struct:get_value(<<"destination_name">>, S),

  case hubberl_db:read({subscription, DestinationName}) of
    [R] ->
      {struct,
        [
          {<<"destination_name">>, R#subscription.destination_name},
          {<<"subscriber_uri">>, R#subscription.subscriber_uri}
        ]};
    [] ->
      _NotFound = struct:set_value(<<"status">>, not_found, S)
  end.
  
read_all() -> 
  Subscriptions = hubberl_db:read_all(subscription),
  
  F = fun(R) ->
        {struct,
          [
            {<<"destination_name">>, R#subscription.destination_name},
            {<<"subscriber_uri">>, R#subscription.subscriber_uri}
          ]}
  end,
  
  lists:map(F, Subscriptions).

update(S) ->
  DestinationName = struct:get_value(<<"destination_name">>, S),
  SubscriberUri  = struct:get_value(<<"subscriber_uri">>, S),

  {atomic, ok} = hubberl_db:write({subscription, DestinationName, SubscriberUri}),

  _Updated = struct:set_value(<<"status">>, updated, S).

delete(S) ->
  DestinationName = struct:get_value(<<"destination_name">>, S),

  {atomic, ok} = hubberl_db:delete({subscription, DestinationName}),

  _Deleted = struct:set_value(<<"status">>, deleted, S).