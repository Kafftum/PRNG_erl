# LCG Erlang Implementation

An implementation of Lewis', Goodman's, and Miller's linear congruential generator in Erlang making use of ``gen_server`` behaviour.

## File Structure:

This library is comprised of two folders: ``doc`` and ``src``:

- ``doc``: Contains the documentation for this library comprised of markdown files.

- ``src``: Contains the source code for the library. In this case the source code consists of two ``.erl`` files each containing a module necessary for the functionality of the library.
  
  - ``prng_math``: This module contains the mathematical functionality of the generator.

    - ``a() -> Const``, ``c() -> Const``, ``m() -> Const``: Functions which return constant values required for by the generator's formula.

    - ``next(State) -> State``: This function takes a ``State`` and returns a new ``State`` after passing the previous ``State`` through the LCG's equation.

    - ``output(State) -> UDV`` This function takes a ``State`` and returns a Uniformely Distributed Value (``UDV``) between ``(0, 1)``.

  - ``prng``: This module contains the initialization and generator functionalities of the generator (structured as a ``gen_server`` callback module).

    - ``new(Max) -> RandomNumber``: This function takes in an integer ``Max`` and returns a random value between ``(0, Max)``.

    - ``init(_Args) -> {ok, system_time_in_microseconds}``: This function initializes the RNG's ``gen_server`` process and returns a tuple containing ``ok`` and the current ``os:system_time`` in microseconds.

    - ``stop() -> ok``: This function stops the RNG's ``gen_server`` process and returns ``ok``.

    - ``start_link() -> PID``: This function starts the RNG's ``gen_server`` processes link and returns it's ``PID``.