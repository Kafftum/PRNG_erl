# Two Pseudo-Random Number Generators implemented in Erlang

This repo is comprised of two subdirectories, each of which contains an implementation of a pseudo-random number generator. Both are written in Erlang.

- [LCG_erl](./LCG_erl/doc/LCG_erl.md): Contains an implementation of Lewis', Goodman's, and Miller's linear congruential generator in Erlang making use of ``gen_server`` behaviour.

- [MRG32k3a_erl](./MRG32k3a_erl/doc/MRG32k3a_erl.md): Contains an implementation of the MRG32k3a multiple recursive generator in Erlang making use of ``gen_server`` behaviour.

## Running the code:
Running or utilizing the code is quite simple. Each module exposes a ``start_link()`` function which starts the ``gen_server`` process of the generator. Running it in the Erlang REPL will start the ``gen_server`` process after which, calling the ``new(Max)`` function (which is also exposed) will result in a new "random" number as well as an internal update to the ``gen_server``'s ``State``.