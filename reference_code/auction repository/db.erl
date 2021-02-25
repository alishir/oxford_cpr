%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: db.erl
%%% @author trainers@erlang-solutions.com
%%% @copyright 1999-2011 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(db).
%% Client Function Exports
-export([new/0, write/3, delete/2, read/2, match/2, destroy/1]).
-include("db.hrl").
-export_type([db/0]).
-type db() :: ETSTableRef::term().

%% @doc Create a new database. Make it protected, unnamed with key position
%% pointing to the record key.
-spec new() -> db().
new() -> ets:new(db, [{keypos, #data.key}]).

%% @doc Insert a new element in the database
-spec write(Key::term(), Val::term(), db()) -> db().
write(Key, Element, Db) ->
 ets:insert(Db, #data{key = Key, value = Element}),
 Db.

%% @doc Remove an element from the database
-spec delete(Key::term(), db()) -> db().
delete(Key, Db) ->
 ets:delete(Db, Key),
 Db.

%% @doc Retrieve the first element in the database with a matching key
-spec read(Key::term(), db()) -> {ok, term()} | {error, instance}.
read(Key, Db) ->
 case ets:lookup(Db, Key) of
 [#data{value = Element}] -> {ok, Element};
 [] -> {error, instance}
 end.

%% @doc Return all the keys whose values match the given element.
-spec match(Val::term(), db()) -> [term()].
match(Element, Db) ->
 lists:flatten(ets:match(Db, #data{key = '$1', value = Element})).
 
%% @doc Deletes the database.
-spec destroy(db()) -> ok.
destroy(Db) ->
 ets:delete(Db),
 ok.