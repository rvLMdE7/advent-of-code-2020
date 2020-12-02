# Advent Of Code 2020

Doing the Advent of Code 2020 challenges in Haskell.

I have each day set up under one umbrella 'project', which should work with
either `stack` or `cabal`. So, in order to play around with any particular day,
you can do the following:

- to open a repl, either `$ cabal repl` or `$ stack repl`, and then you can
  load up a given day with `:load DayXX` or `import DayXX`
- to run a given day, do either `$ cabal run dayXX` or `$ stack run dayXX`

(We have the stack resolver set as a nightly snapshot, rather than a LTS
snapshot, because at time of writing there wasn't yet an LTS release supporting
GHC-8.10.)
