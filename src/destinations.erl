%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Friendly API for hubberl's business operation on destinations (queue or topic).
%%      Its API was projected  to works fine (and facilitate, of course) when dealing
%%      with mochiwebjson2 struct.

-module(destinations).
-author('Leandro Silva <leandrodoze@gmail.com>').

-compile(export_all).

-include("records.hrl").

%% External API

create(S) ->
  case exists(S) of
    yes ->
      already_exists;
    no ->
      Name        = struct:get_value(<<"name">>, S),
      Type        = struct:get_value(<<"type">>, S),
      Description = struct:get_value(<<"description">>, S),

      {atomic, ok} = hubberl_db:write({destination, Name, Type, Description}),
      
      TableName = list_to_atom(binary_to_list(Name) ++ "_" ++ binary_to_list(Type)),
      
      {atomic, ok} = hubberl_db:create_table(TableName, {type, ordered_set}, {attributes, record_info(fields, message)}),

      created
  end.
  
exists(S) ->
  case read(S) of
    not_found ->
      no;
    _ ->
      yes
  end.

read(S) ->
  Name = struct:get_value(<<"name">>, S),

  case hubberl_db:read({destination, Name}) of
    [R] ->
      {struct,
        [
          {<<"name">>, R#destination.name},
          {<<"type">>, R#destination.type},
          {<<"description">>, R#destination.description}
        ]};
    [] ->
      not_found
  end.
  
read_all() -> 
  Destinations = hubberl_db:read_all(destination),
  
  F = fun(R) ->
        {struct,
          [
            {<<"name">>, R#destination.name},
            {<<"type">>, R#destination.type},
            {<<"description">>, R#destination.description}
          ]}
  end,
  
  lists:map(F, Destinations).

update(S) ->
  Name        = struct:get_value(<<"name">>, S),
  Type        = struct:get_value(<<"type">>, S),
  Description = struct:get_value(<<"description">>, S),

  case exists(S) of
    yes ->
      {atomic, ok} = hubberl_db:write({destination, Name, Type, Description}),

      updated;
    no ->
      not_found
  end.
  
delete(S) ->
  Name = struct:get_value(<<"name">>, S),

  case exists(S) of
    yes ->
      {atomic, ok} = hubberl_db:delete({destination, Name}),

      deleted;
    no ->
      not_found
  end.
