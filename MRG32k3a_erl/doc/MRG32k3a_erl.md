# MRG32k3a Erlang Implementation

An implementation of the MRG32k3a multiple recursive generator in Erlang making use of ``gen_server`` behaviour.

## File Structure:

This library is comprised of two folders: ``doc`` and ``src``:

- ``doc``: Contains the documentation for this library comprised of markdown files.

- ``src``: Contains the source code for the library. In this case the source code consists of two ``.erl`` files each containing a module necessary for the functionality of the library.
  
  - ``mrg32k3a_math``: This module contains the mathematical functionality of the generator.

    - ``m1() -> Const``, ``m2() -> Const``, ``norm() -> Const``: Are functions that return constant values required by the generator's formula.

    - ``stafford_mix(Seed) -> MixedSeed``: This function takes a seed and deterministically expands it returning a ``MixedSeed``.

    - ``next(State) -> {Output, NewState}``: This function takes a ``State`` tuple containing the relevant state lags and returns a tuple containing the normalized ``Output`` and a new ``State`` tuple.

  - ``mrg32k3a``: This module contains the initialization and generator functionalities of the generator (structured as a ``gen_server`` callback module).

    - ``new(Max) -> Float | Int``: This function takes in a ``Max`` (float or int), and depending on the type of ``Max`` returns a random float or int between ``(0, Max)``.

    - ``init(_Args) -> {ok, ExpandedSeeds}``: This function sets the seed, sets it up for the deterministic expansion function, and expands the 6 necessary seeds for MRG32k3a. It returns a tuple containing ``ok`` as well as a tuple containing the expanded seeds (``ExpandedSeeds``).

    - ``stop() -> ok``: This function stops the generator's ``gen_server`` process, returning ``ok``.

    - ``start_link() -> PID``: This function starts the RNG's ``gen_server`` processes link and returns it's ``PID``.