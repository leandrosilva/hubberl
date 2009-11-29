%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Friendly API for hubberl's business operation on destinations (queue or topic).
%%      Its input/output works fine with struct.

-module(destinations).
-author('Leandro Silva <leandrodoze@gmail.com>').

-compile(export_all).

-include("records.hrl").

create(S) ->
	Uri = struct:get_value(<<"uri">>, S),
	Type = struct:get_value(<<"type">>, S),
	Description = struct:get_value(<<"description">>, S),
	
	{atomic, ok} = hubberl_db:write({destination, Uri, Type, Description}),
	
	_Created = struct:set_value(<<"status">>, created, S).

read(S) ->
	Uri = struct:get_value(<<"uri">>, S),

	case hubberl_db:read({destination, Uri}) of
		[R] ->
			{struct,
				[
					{<<"uri">>, R#destination.uri},
				 	{<<"type">>, R#destination.type},
				 	{<<"description">>, R#destination.description}
				]};
		[] ->
			_NotFound = struct:set_value(<<"status">>, not_found, S)
	end.
	
read_all(_S) -> 
	Destinations = hubberl_db:read_all(destination),
	
	F = fun(R) ->
				{struct,
					[
						{<<"uri">>, R#destination.uri},
						{<<"type">>, R#destination.type},
						{<<"description">>, R#destination.description}
					]}
	end,
	
	lists:map(F, Destinations).

update(S) ->
	Uri = struct:get_value(<<"uri">>, S),
	Type = struct:get_value(<<"type">>, S),
	Description = struct:get_value(<<"description">>, S),

	{atomic, ok} = hubberl_db:write({destination, Uri, Type, Description}),

	_Updated = struct:set_value(<<"status">>, updated, S).

delete(S) ->
	Uri = struct:get_value(<<"uri">>, S),

	{atomic, ok} = hubberl_db:delete({destination, Uri}),

	_Deleted = struct:set_value(<<"status">>, deleted, S).