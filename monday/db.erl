-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

% new() -> DbRef
new() -> [].

% destroy(DbRef) -> ok
destroy(_) -> ok.

% write(Key, Element, DbRef) -> NewDbRef
write(Key, Element, DbRef) -> 
  DbRefDeleted = delete(Key, DbRef),
  [{Key, Element} | DbRefDeleted].


% read(Key, DbRef) -> {ok, Element} | {error, instance}
read(Key, [{Key, Element} | _]) -> 
  {ok, Element};
read(Key, [_ | DbRefTail]) -> 
  read(Key, DbRefTail);
read(_, []) -> 
  {error, instance}.

% match(Element, DbRef) -> [Key1, ..., KeyN]
match(Element, DbRef) ->
  match_acc(Element, DbRef, []).

match_acc(Element, [{Key, Element} | DbRefTail], Matched) ->
  match_acc(Element, DbRefTail, [Key | Matched]);
match_acc(Element, [_ | DbRefTail], Matched) ->
  match_acc(Element, DbRefTail, Matched);
match_acc(_, [], Matched) ->
  Matched.

% delete(Key, DbRef) -> NewDbRef
delete(Key, DbRef) ->
  delete_acc(Key, DbRef, []).

delete_acc(Key, [{Key, _} | DbRefTail], DbRefPassed) ->
  delete_acc(Key, DbRefTail, DbRefPassed);
delete_acc(Key, [KeyElement | DbRefTail], DbRefPassed) ->
  delete_acc(Key, DbRefTail, [KeyElement | DbRefPassed]);
delete_acc(_, [], DbRefPassed) ->
  DbRefPassed.
