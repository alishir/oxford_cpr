-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([server_init/0]).
-import(db, [new/0, destroy/1, write/3, delete/2, read/2, match/2]).

%%% Client
start() ->
  server_start(),
  ok.

stop() ->
  server_stop(),
  ok.

write(Key, Element) ->
  server ! {write, {Key, Element}},
  ok.

delete(Key) ->
  server ! {delete, Key},
  ok.

read(Key) ->
  server ! {read, Key, self()},
  receive
    Reply -> Reply
  end.

match(Element) ->
  server ! {match, Element, self()},
  receive
    Reply -> Reply
  end.

%%% Server
% key choices
% state = just db
% receive = db method & args
% init args = na
server_start() ->
  code:add_path("C:/Users/lao8n/OneDrive/Documents/oxford_cpr/monday"),
  register(server, spawn(?MODULE, server_init, [])),
  ok.

server_init() -> 
  Db = db:new(),
  server_loop(Db).

server_loop(Db) ->
  receive
    stop -> ok;
    {write, {Key, Element}} -> 
      server_loop(db:write(Key, Element, Db));
    {delete, Key} ->
      server_loop(db:delete(Key, Db));
    {read, Key, Client} -> 
      Response = db:read(Key, Db), % server blocked whilst waiting
      Client ! Response, % Response = {error, instance} or {ok, Element}.
      server_loop(Db);
    {match, Element, Client} ->
      Response = db:match(Element, Db), % server blocked whilst waiting
      Client ! Response,
      server_loop(Db)
  end.

server_stop() ->
  server ! stop,
  ok.