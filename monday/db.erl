-module(db).
% -export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

% destroy(DbRef) ->

write(Key, Element, DbRef) -> 
  [{Key, Element} | DbRef].
% delete(Key, DbRef) ->

% read(Key, DbRef) ->

% match(Element, DbRef) ->

