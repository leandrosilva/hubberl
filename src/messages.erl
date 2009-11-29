%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Friendly API for hubberl's business operation on messages for destinations.
%%      Its API was projected  to works fine (and facilitate, of course) when dealing
%%      with mochiwebjson2 struct.

-module(messages).
-author('Leandro Silva <leandrodoze@gmail.com>').

-compile(export_all).

-include("records.hrl").

create(S) ->
	Id = hubberl_db:new_id(message),
	DestinationUri = struct:get_value(<<"destination_uri">>, S),
	PublisherUri = struct:get_value(<<"publisher_uri">>, S),
	Content = struct:get_value(<<"content">>, S),
	
	{atomic, ok} = hubberl_db:write({message, Id, DestinationUri, PublisherUri, Content, not_yet}),
	
	New = struct:set_value(<<"id">>, Id),
	
	_Created = struct:set_value(<<"status">>, created, New).

read(S) ->
	Id = struct:get_value(<<"id">>, S),

	case hubberl_db:read({message, Id}) of
		[R] ->
			{struct,
				[
					{<<"id">>, R#message.id},
				 	{<<"destination_uri">>, R#message.destination_uri},
				 	{<<"publisher_uri">>, R#message.publisher_uri},
				 	{<<"content">>, R#message.content},
				 	{<<"delivered">>, R#message.delivered}
				]};
		[] ->
			_NotFound = struct:set_value(<<"status">>, not_found, S)
	end.
	
read_all() -> 
	Messages = hubberl_db:read_all(message),
	
	F = fun(R) ->
				{struct,
					[
						{<<"id">>, R#message.id},
					 	{<<"destination_uri">>, R#message.destination_uri},
					 	{<<"publisher_uri">>, R#message.publisher_uri},
					 	{<<"content">>, R#message.content},
					 	{<<"delivered">>, R#message.delivered}
					]}
	end,
	
	lists:map(F, Messages).

update(S) ->
	Id = struct:get_value(<<"id">>, S),
	DestinationUri = struct:get_value(<<"destination_uri">>, S),
	PublisherUri = struct:get_value(<<"publisher_uri">>, S),
	Content = struct:get_value(<<"content">>, S),
	
	{atomic, ok} = hubberl_db:write({message, Id, DestinationUri, PublisherUri, Content, not_yet}),

	_Updated = struct:set_value(<<"status">>, updated, S).

delete(S) ->
	Id = struct:get_value(<<"id">>, S),

	{atomic, ok} = hubberl_db:delete({message, Id}),

	_Deleted = struct:set_value(<<"status">>, deleted, S).