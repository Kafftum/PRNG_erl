-module(prng_math).

-export([next/1, output/1]).

a() -> 16807.

c() -> 0.

m() -> 2147483647.

%% ( (a * State) + c ) mod m
next(State) ->
  ( (a() * State) + c() ) rem m().

%% State / m
output(State) ->
  State / m().