# Advent Of Code 2020

Doing the Advent of Code 2020 challenges in Haskell.

## Package Setup

The way this repository is set up hits a nice sweet spot:

- It's super simple - each day is just a single file for the code, and then a
  single file for the input. That's it. There is no special machinery for
  parsing command line arguments to work out what day to run or anything like
  that.
- All the days are under one big cabal project, so you can do `cabal repl`
  (or `stack repl`) to load up the code for all days at once into a REPL to
  play around with, and easily swap between them. Or you could even load up
  multiple days simultaneously if you want.
- IDE support works well, as all the code is considered "library" code, and
  it's all under one cabal package.
- Nonetheless (using a little bit of backpack trickery), we provide an
  executable for each day with minimal effort. So you can do
  `cabal run day-XX` (or `stack run day-XX`) to run the compiled executable
  for each day.

(Note: we have the stack resolver set as a nightly snapshot, rather than a LTS
snapshot, because at time of writing there wasn't yet an LTS release supporting
GHC-8.10.)

## Approach To The Challenges

In terms of approaching each day, I'm not worrying too much about optimizing
things or making something robust in terms of error handling and such. Instead,
I'm giving some consideration to picking the right data structures and
algorithms, and then aiming to write straightforward, clear, declarative code
that does the job.
