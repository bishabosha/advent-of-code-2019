# Advent of Code 2019

## About

These challenges are being implemented with [Scala 3](http://scala-lang.org) using functional programming libraries such
as [ZIO](https://zio.dev) and [spire](https://typelevel.org/spire/) to lift IO operations and work with numerics.

### Usage

`sbt console` to open the Scala 3 REPL. `import aoc.exports.given` to get access to the `sync()` method,
which you can call on any `ZIO` value that has type `Nothing` in its error channel to execute it and yield a result.

Each challenge is implemented as a ZIO value following a naming scheme of `aoc.DayN.dayN_M` where `N` is the day and
`M` is the challenge number.

The following is an example of evaluating Day 3, challenge 2.

```scala
$ sbt console
Welcome to Scala 3.1.0 (1.8.0_282, Java OpenJDK 64-Bit Server VM GraalVM CE 21.0.0).
Type in expressions for evaluation. Or try :help.

scala> import aoc.exports.given

scala> aoc.Day3.day3_2.sync()
val res4: Either[String, Int] = Right(<answer>)
```
