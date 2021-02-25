%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: db.erl
%%% @author trainers@erlang-solutions.com
%%% @copyright : 1999-2019 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(db).
-export([new/0, write/3, delete/2, read/2, match/2, destroy/1]).
-export_type([db/0]).
-type db() :: map().
%% @doc Create a new database
-spec new() -> db().
new() ->
 #{}.
%% @doc Insert a new element in the database
-spec write(Key::term(), Val::term(), db()) -> db().
write(Key, Element, Db) ->
 maps:put(Key, Element, Db).
%% @doc Remove an element from the database
-spec delete(Key::term(), db()) -> db().
delete(Key, Db) ->
 maps:remove(Key, Db).
%% @doc Retrieve the first element in the database with a matching key
-spec read(Key::term(), db()) -> {ok, term()} | {error, instance}.
read(Key, Db) ->
 case maps:find(Key, Db) of
 {ok, Element} -> {ok, Element};
 error -> {error, instance}
 end.
%% @doc Return all the keys whose values match the given element.
-spec match(Val::term(), db()) -> [term()].
match(Element, Db) ->
 [ Key || {Key, El} <- maps:to_list(Db),
 El =:= Element].
%% @doc Deletes the database.
-spec destroy(db()) -> ok.
destroy(_Db) ->
 ok.
