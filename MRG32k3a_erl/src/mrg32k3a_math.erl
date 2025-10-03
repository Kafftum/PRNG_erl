-module(mrg32k3a_math).

-export([m1/0, m2/0]).
-export([stafford_mix/1, next/1]).

%% Constants
-define(MASK64, 16#FFFFFFFFFFFFFFFF).

m1() -> 4294967087.

m2() -> 4294944443.

norm() -> 2.328306549295727688e-10.

%% Mathematical components

%% Takes a seed and deterministically expands it
%% Input: Seed: Int
%% Output: MixedSeed: Int
stafford_mix(Seed) -> 
  S1 = (Seed bxor (Seed bsr 30)),
  S1m = (S1 * 16#BF58476D1CE4E5B9) band ?MASK64,

  S2 = (S1m bxor (S1m bsr 27)),
  S2m = (S2 * 16#94D049BB133111EB) band ?MASK64,

  MixedSeed = S2m bxor (S2m bsr 31),
  MixedSeed.

%% Returns a tuple containing two tuples: the left one contains the next state, and the right one
%%                                        contains the complete new state after the shift.
%%
%% Input: {S10: Int, S11: Int, S12: Int, S20: Int, S21: Int, S22: Int}
%% Legend: S10 = X(n-3), S11 = X(n-2), S12 = X(n-1), S20 = Y(n-3), S21 = Y(n-2), S22 = Y(n-1)
%% Output: { Output, {newState (Shifted input tuple)} }
next(_State = {S10, S11, S12, S20, S21, S22}) -> 
  X0 = ((1403580 * S11) - (810728 * S10)) rem m1(),
  X = if X0 < 0 -> X0 + m1(); true -> X0 end,

  Y0 = ((527612 * S22) - (1370589 * S20)) rem m2(),
  Y = if Y0 < 0 -> Y0 + m2(); true -> Y0 end,

  io:format("X: ~p~nY: ~p~n", [X, Y]),

  Z = X - Y,
  Z1 = if Z =< 0 -> Z + m1(); true -> Z end,
  
  Output = Z1 * norm(),

  io:format("Output: ~p~n", [Output]),

  { Output, {S11, S12, X, S21, S22, Y} }.