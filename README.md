# Effects

An exploration of the various approaches to modeling side effects in a purely functional programming language.

[![Build Status](https://travis-ci.org/stepchowfun/effects.svg?branch=master)](https://travis-ci.org/stepchowfun/effects)

## Techniques demonstrated

- A bespoke monad.
- A standard monad transformer stack.
- A free monad with an interpreter into the aforementioned monad transformer stack.

## The challenge

Write a program that initializes an accumulator and random seed both with `0` and then runs the following procedure 10 times:

- Log the value of the accumulator.
- Pick an integer uniformly randomly from the half-open interval `[0, 10)`.
- Mutate the accumulator by adding the random integer to it.

## Instructions

Make sure you have [Make](https://www.gnu.org/software/make/) and [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then you can use this command to run the demo:

```
make run
```
