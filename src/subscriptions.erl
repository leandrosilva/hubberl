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
	Id             = hubberl_db:new_id(destination),
	DestinationUri = struct:get_value(<<"destination_uri">>, S),
	SubscriberUri  = struct:get_value(<<"subscriber_uri">>, S),
	
	{atomic, ok} = hubberl_db:write({subscription, Id, DestinationUri, SubscriberUri}),
	
	New = struct:set_value(<<"id">>, Id),
	
	_Created = struct:set_value(<<"status">>, created, New).

read(S) ->
	Id = struct:get_value(<<"id">>, S),

	case hubberl_db:read({subscription, Id}) of
		[R] ->
			{struct,
				[
					{<<"id">>, R#subscription.id},
				 	{<<"destination_uri">>, R#subscription.destination_uri},
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
						{<<"id">>, R#subscription.id},
					 	{<<"destination_uri">>, R#subscription.destination_uri},
					 	{<<"subscriber_uri">>, R#subscription.subscriber_uri}
					]}
	end,
	
	lists:map(F, Subscriptions).

update(S) ->
	Id             = struct:get_value(<<"id">>, S),
	DestinationUri = struct:get_value(<<"destination_uri">>, S),
	SubscriberUri  = struct:get_value(<<"subscriber_uri">>, S),

	{atomic, ok} = hubberl_db:write({subscription, Id, DestinationUri, SubscriberUri}),

	_Updated = struct:set_value(<<"status">>, updated, S).

delete(S) ->
	Id = struct:get_value(<<"id">>, S),

	{atomic, ok} = hubberl_db:delete({subscription, Id}),

	_Deleted = struct:set_value(<<"status">>, deleted, S).