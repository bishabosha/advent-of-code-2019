package aoc

import zio._

import java.lang.Math._

object Day14 with

  val Quantity = raw"(\d+?) (\w+)".r
  val Reaction = raw"(\d+? \w+?(?:, \d+? \w+?)*) => (\d+ \w+)".r

  type Quantity = (String, Long)
  type Formula  = (List[Quantity], Quantity)
  type Formulas = Map[String, (List[Quantity], Long)]
  type Quants   = Map[String, Long]

  def oreFor(chemical: String, requirement: Long)(formulas: Formulas): UIO[Long] =
    dependencies(chemical, requirement, formulas, Map.empty) map (_.getOrElse("ORE", -1L))

  def givenOre(chemical: String, ore: Long)(formulas: Formulas): UIO[Long] =

    def inner(formulas: Formulas, quantities: Quants, total: Long, requirement: Long): UIO[Long] =
      for
        quantities1 <- dependenciesTake(chemical, requirement, formulas, quantities)
        ore = quantities1("ORE")
        total <- (
          if ore == quantities("ORE") then UIO.succeed(total)
          else if ore > 0 then UIO.effectSuspendTotal(inner(formulas, quantities1, total + requirement, requirement))
          else UIO.effectSuspendTotal(inner(formulas, quantities, total, requirement/2)) // backtrack with smaller amount
        )
      yield total

    inner(formulas, Map("ORE" -> ore), 0, ore/2)

  end givenOre


  def dependenciesTake(chemical: String, requirement: Long, formulas: Formulas, quantities: Quants): UIO[Quants] =
    dependencies(_-_, dependenciesTake)(chemical,requirement,formulas,quantities)

  def dependencies(chemical: String, requirement: Long, formulas: Formulas, quantities: Quants): UIO[Quants] =
    dependencies(_+_, dependencies)(chemical,requirement,formulas,quantities)

  inline def dependencies(
    updateOre: => (Long, Long) => Long,
    suspend: => (String, Long, Formulas, Quants) => UIO[Quants])(
    chemical: String,
    requirement: Long,
    formulas: Formulas,
    quantities: Quants
  ): UIO[Quants] =
    val existing = quantities.getOrElse(chemical, 0L)
    if requirement <= existing then
      UIO.succeed(quantities.updated(chemical, existing - requirement))
    else
      val reqs -> production = formulas(chemical)
      val multiplier = quant(production, requirement - existing)
      val quantities1 = quantities.updated(chemical, (existing + production * multiplier) - requirement)
      reqs match
      case ("ORE", ore) :: Nil =>
        UIO.succeed(quantities1.updated("ORE", updateOre(quantities1.getOrElse("ORE", 0L), ore * multiplier)))
      case reqs =>
        UIO.foldLeft(reqs)(quantities1)((acc, req) =>
          UIO.effectSuspendTotal(suspend(req._1, req._2 * multiplier, formulas, acc)) map (acc ++ _))

  def quant(unit: Long, requirement: Long) =
    ceil(requirement/unit.toDouble).toLong

  def load(formulas: List[Formula]): Formulas =
    formulas.groupBy(_._2._1).map(_.bimap(identity, _.head.bimap(identity, _._2)))

  def parseReactions(s: List[String]): IO[IllegalArgumentException, List[Formula]] =
    IO.foreachPar(s)(parseReaction)

  def parseReaction(s: String): IO[IllegalArgumentException, Formula] = s match
    case Reaction(lhs, Quantity(n, c)) =>
      IO.foldLeft(lhs.split(", "))(List.empty[Quantity]) { (acc, q) => q match
        case Quantity(n,c) => IO.succeed((c, n.toLong) :: acc)
        case _             => IO.fail(IllegalArgumentException(s"not a quantity: $q"))
      } map (_.reverse -> (c -> n.toLong))
    case _ => IO.fail(IllegalArgumentException(s"not a reaction: $s"))

  def numberOfOreFor(chemical: String, amount: Long) =
    (sourceFile >>= parseReactions) map load >>= (oreFor(chemical, amount)(_))
  def numberOfGivenOre(chemical: String, ore: Long) =
    (sourceFile >>= parseReactions) map load >>= (givenOre(chemical, ore)(_))

  val day14_1 = challenge("day14")(numberOfOreFor("FUEL", 1))
  val day14_2 = challenge("day14")(numberOfGivenOre("FUEL", 1000000000000L))

end Day14
