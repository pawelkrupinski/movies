package tools.impactfixture

// SameFileEdgesSpec's fixture, PopulationMonotonicitySpec's shape: an abstract check, the concrete parts that run it in
// its own file, and its object, the one class of the file that uses Relation. Zinc records Walk's use of Relation (the
// class and its object are one class name to it) and nothing between the classes of this file.

abstract class Walk(part: Int) { def run: Boolean = Walk.check(part) }

final class WalkPart1 extends Walk(1)
final class WalkPart2 extends Walk(2)

/** Uses no other class of the file. */
final class Unrelated { def answer: Int = 42 }

object Walk {
  final case class Row(part: Int)
  def check(part: Int): Boolean = Relation.same(Row(part).toString, "Row(1)")
}
