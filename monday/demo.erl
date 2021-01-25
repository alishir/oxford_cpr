-module(demo).
-export([double/1]).

% This is a comment.
% Everything after '%' is ignored.

double(X) ->
  times(X, 2).

times(X, N) ->
  X * N.