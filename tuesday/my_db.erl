-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([server_init/0, server_start/0]).
-import(db, [new/0, destroy/1, write/3, delete/2, read/2, match/2]).

%%% Client
start() ->
  server_start(),
  ok.

stop() ->
  server_stop(),
  ok.

write(Key, Element) ->
  server ! {write, Key, Element},
  ok.

delete(Key) ->
  server ! {delete, Key},
  ok.

read(Key) ->
  server ! {read, Key, self()},
  receive
    {read_result, Response} -> Response
  end.

match(Element) ->
  server ! {match, Element, self()},
  receive
    {match_result, Response} -> Response
  end.

%%% Server
% key choices
% state = just db
% receive = db method & args
% init args = na
server_start() ->
  register(server, spawn(?MODULE, server_init, [])),
  ok.

server_init() -> 
  server_loop(db:new()).

server_loop(Db) ->
  receive
    stop -> ok;
    {write, Key, Element} -> 
      server_loop(db:write(Key, Element, Db));
    {delete, Key} ->
      server_loop(db:delete(Key, Db));
    {read, Key, Client} -> 
      Client ! {read_result, db:read(Key, Db)}, % Response = {error, instance} or {ok, Element}.
      server_loop(Db);
    {match, Element, Client} ->
      Client ! {match_result, db:match(Element, Db)},
      server_loop(Db)
    % meta programming approach
    % {Operation, From, Pattern} 
    %   when Operation == read orelse Operation == match ->
    %   Result = db:Operation(Pattern, State),
    %   From ! {Operation, Result},
    %   loop(State)
  end.

server_stop() ->
  server ! stop,
  ok.