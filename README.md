# Advent Of Code 2020

Doing the Advent of Code 2020 challenges in Haskell.

I have each day set up as a separate 'project', and I have things set up so
they should work with either `stack` or `cabal`. So, in order to play around
with any particular day, you can do the following:

- change dir: `$ cd day-xx`
- open repl: `$ cabal repl` or `$ stack repl`
- run task: `$ cabal run` or `$ stack run`

(We have the stack resolver set as a nightly snapshot, rather than a LTS
snapshot, because at time of writing there wasn't yet an LTS release supporting
GHC-8.10.)
