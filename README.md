# Advent of Code 2019

## About

These challenges are being implemented with [dotty](http://dotty.epfl.ch) using functional programming libraries such as [ZIO](https://zio.dev) and [spire](https://typelevel.org/spire/) to lift IO operations and work with numerics.

### Usage

`sbt console` to open the dotty repl. `import aoc.{_,given}` to get access to the `sync()` method, which you can call on any ZIO value that has type `Nothing` in its error channel to execute it and yield a result. Each challenge is implemented as a ZIO value following a naming scheme of `dayN_M` where `N` is the day and `M` is the challenge number.
