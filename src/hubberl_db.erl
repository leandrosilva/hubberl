%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Friendly API for hubberl's Mnesia database.

-module(hubberl_db).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([new_id/1, write/1, read/1, read_all/1, delete/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

%% External API

new_id(Key) ->
	mnesia:dirty_update_counter({counter, Key}, 1).

write(Rec) ->
	F = fun() ->
			mnesia:write(Rec)
	end,
	mnesia:transaction(F).

read(Rid) ->
	F = fun() ->
			mnesia:read(Rid)
	end,
	transaction(F).

read_all(Table) ->
	Q = qlc:q([X || X <- mnesia:table(Table)]),
	find(Q). 

delete(Rid) ->
	F = fun() ->
			mnesia:delete(Rid)
	end,
	mnesia:transaction(F).

%% Internal API

reset() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),

	% table for ID generation
	mnesia:create_table(counter, [{disc_copies, [node()]}, {attributes, record_info(fields, counter)}]),

	% business tables
	mnesia:create_table(queue, [{disc_copies, [node()]}, {attributes, record_info(fields, queue)}]),
	mnesia:create_table(topic, [{disc_copies, [node()]}, {attributes, record_info(fields, topic)}]).

transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, _Reason} ->
			[]
	end.	

find(Q) ->
	F = fun() ->
			qlc:e(Q)
	end,
	transaction(F).
