%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Friendly API for hubberl's business operation on destinations (queue or topic).
%%      Its API was projected  to works fine (and facilitate, of course) when dealing
%%      with mochiwebjson2 struct.

-module(destinations).
-author('Leandro Silva <leandrodoze@gmail.com>').

-compile(export_all).

-include("records.hrl").

create(S) ->
  case exists(S) of
    yes ->
      _AlreadyExists = struct:set_value(<<"status">>, already_exists, S);
    no ->
      Name        = struct:get_value(<<"name">>, S),
      Type        = struct:get_value(<<"type">>, S),
      Description = struct:get_value(<<"description">>, S),

      {atomic, ok} = hubberl_db:write({destination, Name, Type, Description}),

      _Created = struct:set_value(<<"status">>, created, S)
  end.
  
exists(S) ->
  Read   = read(S),
  Status = struct:get_value(<<"status">>, Read),
  
  case Status of
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
      _NotFound = struct:set_value(<<"status">>, not_found, S)
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

  {atomic, ok} = hubberl_db:write({destination, Name, Type, Description}),

  _Updated = struct:set_value(<<"status">>, updated, S).

delete(S) ->
  Name = struct:get_value(<<"name">>, S),

  {atomic, ok} = hubberl_db:delete({destination, Name}),

  _Deleted = struct:set_value(<<"status">>, deleted, S).