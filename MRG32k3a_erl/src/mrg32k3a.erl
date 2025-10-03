-module(mrg32k3a).
-behaviour(gen_server).

%% Exporting GenServer callbacks
-export([start_link/0, init/1, stop/0, handle_call/3]).

%% Exporting module-specific callbacks
-export([new/1]).

-define(GAMMA, 16#9E3779B97F4A7C15).

%% State: {S10, S11, S12, S20, S21, S22}

new(Max) when is_float(Max) -> 
  gen_server:call(mrg32k3a_genserver, {new_float, Max});

new(Max) -> 
  gen_server:call(mrg32k3a_genserver, {new_int, Max}).

stop() -> 
  gen_server:call(mrg32k3a_genserver, stop).

start_link() -> 
  gen_server:start_link(?MODULE, [], []).

%% Legend: ESXX = ExpandedSeedXX
%% Legend: S = Seed
%% XX = Lagged seed position
init(_Args) -> 
  register(mrg32k3a_genserver, self()),

  %% Set seed (Current time in microseconds)
  Seed = os:system_time(microsecond),

  %% Set up for deterministic expansion function (stafford mix)
  S10 = Seed + 16#9e3779b97f4a7c15,
  S11 = S10 + ?GAMMA,
  S12 = S11 + ?GAMMA,

  S20 = S12 + ?GAMMA,
  S21 = S20 + ?GAMMA,
  S22 = S21 + ?GAMMA,

  %% Expand seeds
  ES10 = mrg32k3a_math:stafford_mix(S10) rem mrg32k3a_math:m1(),
  ES11 = mrg32k3a_math:stafford_mix(S11) rem mrg32k3a_math:m1(),
  ES12 = mrg32k3a_math:stafford_mix(S12) rem mrg32k3a_math:m1(),

  ES20 = mrg32k3a_math:stafford_mix(S20) rem mrg32k3a_math:m2(),
  ES21 = mrg32k3a_math:stafford_mix(S21) rem mrg32k3a_math:m2(),
  ES22 = mrg32k3a_math:stafford_mix(S22) rem mrg32k3a_math:m2(),

  ExpandedSeeds = {ES10, ES11, ES12, ES20, ES21, ES22},
  {ok, ExpandedSeeds}.

handle_call({new_float, Max}, _From, State) ->
  {Output, NewState} = mrg32k3a_math:next(State),  
  {reply, Output * Max, NewState};

handle_call({new_int, Max}, _From, State) ->
  {Output, NewState} = mrg32k3a_math:next(State),
  {reply, round(Output * Max), NewState};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

terminate(normal, _State) ->
  ok.