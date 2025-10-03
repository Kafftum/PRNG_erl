-module(prng).
-behaviour(gen_server).

-export([start_link/0, init/1, stop/0, terminate/2, handle_call/3]).
-export([new/1]).

new(Max) ->
  gen_server:call(prng_genserver, {new, Max}).

stop() ->
  gen_server:call(prng_genserver, stop).

start_link() -> 
  gen_server:start_link(?MODULE, [], []).

init(_) ->
  register(prng_genserver, self()),
  {ok, os:system_time(microsecond)}.

handle_call({new, Max}, _From, State) -> 
  NewState = prng_math:next(State),
  Output = prng_math:output(NewState),
  {reply, Output * Max, NewState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

terminate(normal, _State) ->
  ok.