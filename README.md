# Effects

A brief exploration of the various approaches to modeling side effects in a purely functional programming language.

[![Build status](https://github.com/stepchowfun/effects/workflows/Continuous%20integration/badge.svg?branch=master)](https://github.com/stepchowfun/effects/actions?query=branch%3Amaster)

## The challenge

Write a program that initializes an accumulator and random seed both with `0` and then runs the following procedure 10 times:

- Log the value of the accumulator.
- Pick an integer uniformly randomly from the half-open interval `[0, 10)`.
- Mutate the accumulator by adding the random integer to it.

Thus, 3 computational effects are exhibited: logging, randomness, and mutable state.

## Techniques demonstrated

This repository contains 4 implementations of the program described above, each demonstrating a specific technique:

- A [bespoke monad](https://github.com/stepchowfun/effects/blob/master/src/BespokeMonad.hs)
- A standard [monad transformer stack](https://github.com/stepchowfun/effects/blob/master/src/MonadTransformers.hs)
- A [free monad](https://github.com/stepchowfun/effects/blob/master/src/FreeMonad.hs)
- The [`Eff` monad](https://github.com/stepchowfun/effects/blob/master/src/ExtensibleEffects.hs) from the "extensible effects" framework

## Instructions

Make sure you have [Toast](https://github.com/stepchowfun/toast) installed. Then you can run `toast run` to build and run the demo.
