-module(temp).
-export([f2c/1, c2f/1, convert/1]).

f2c(Fahrenheit) ->
  (5 * (Fahrenheit - 32))/9.

c2f(Celsius) ->
  (9 * Celsius) / 5 + 32.

convert({c, Temp}) ->
  {f, c2f(Temp)};
convert({f, Temp}) ->
  {c, f2c(Temp)}.