-module(ring).
-compile(export_all)

% 2 strategies
% 1. central process that sets up the ring and initiates message sending
% 2. each process spawns the next process in the ring, but need a way to connect the last to the first


% each process
% 1. setups own loop
% 2. creates the next process & its loop
% 3. sends a message to the next process
% loop() ->
%   receive

% first process
% start(M, N, Message) ->
%   FirstPid = spawn(echo, loop, []),
%   FirstPid ! Message;
%   start(M, N, 1, FirstPid).

% % ith process
% start(M, N, ProcessI, Message, FirstPid) ->
%   spawn(echo, loop, []),
%   start(M, N, ProcessI + 1, Message, FirstPid).
% start(M, N, N, Message) ->