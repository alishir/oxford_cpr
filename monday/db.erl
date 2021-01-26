-module(db).
-export([new/0, 
         destroy/1, 
         write/3, 
         delete/2, 
         read/2, 
         match/2
]).
%-compile(export_all).

% new() -> DbRef
new() -> [].

% destroy(DbRef) -> ok
destroy(_) -> ok.

% write(Key, Element, DbRef) -> NewDbRef
write(Key, Element, DbRef) -> 
  [{Key, Element} | delete(Key, DbRef)].
% could also recurse through list to add to end

% read(Key, DbRef) -> {ok, Element} | {error, instance}
read(_, []) -> 
  {error, instance};
read(Key, [{Key, Element} | _]) -> 
  {ok, Element};
read(Key, [_ | DbRefTail]) -> 
  read(Key, DbRefTail).

% match(Element, DbRef) -> [Key1, ..., KeyN]
match(Element, DbRef) ->
  match_acc(Element, DbRef, []).
match_acc(_, [], Matched) ->
  Matched;
match_acc(Element, [{Key, Element} | DbRefTail], Matched) ->
  match_acc(Element, DbRefTail, [Key | Matched]);
match_acc(Element, [_ | DbRefTail], Matched) ->
  match_acc(Element, DbRefTail, Matched).

% delete(Key, DbRef) -> NewDbRef
delete(Key, DbRef) ->
  delete_acc(Key, DbRef, []).

delete_acc(_, [], DbRefPassed) ->
  DbRefPassed;
delete_acc(Key, [{Key, _} | DbRefTail], DbRefPassed) ->
  delete_acc(Key, DbRefTail, DbRefPassed);
delete_acc(Key, [KeyElement | DbRefTail], DbRefPassed) ->
  delete_acc(Key, DbRefTail, [KeyElement | DbRefPassed]).
