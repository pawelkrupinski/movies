package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** A2 at the solver: the partition is a function of the node and constraint SETS, no component
 *  holds a cannot-linked pair, and the ambiguity rule leaves a node reaching two cannot-linked
 *  components alone — each against the first-wins mutant. */
class ConstraintSolverSpec extends AnyFlatSpec with Matchers {

  import ConstraintSolver.{Constraint, Presentation}

  private def graph(seed: Long): (Seq[Int], Seq[Constraint[Int]]) = {
    val rnd   = new Random(seed)
    val nodes = 0 until (3 + rnd.nextInt(10))
    val cs = for {
      a <- nodes; b <- nodes if a < b && rnd.nextDouble() < 0.45
    } yield if (rnd.nextDouble() < 0.12) Constraint(a, b, must = false, 0, "cannot")
            else Constraint(a, b, must = true, 1 + rnd.nextInt(3), "must")
    (nodes, cs)
  }

  private def presented(seed: Long, nodes: Seq[Int], cs: Seq[Constraint[Int]]) = {
    val rnd = new Random(seed)
    (rnd.shuffle(nodes), rnd.shuffle(cs).map(c => if (rnd.nextBoolean()) c.copy(a = c.b, b = c.a) else c))
  }

  private def firstOrderViolation(presentation: Presentation): Option[Long] = (1L to 3000L).find { seed =>
    val (nodes, cs) = graph(seed)
    val reference = ConstraintSolver.solveAs(nodes, cs, presentation).toSet
    (1L to 20L).exists { p =>
      val (n2, c2) = presented(seed * 100 + p, nodes, cs)
      ConstraintSolver.solveAs(n2, c2, presentation).toSet != reference
    }
  }

  "The solver" should "partition 3,000 random constraint graphs identically under 20 presentations each" in {
    firstOrderViolation(Presentation.Canonical) shouldBe None
  }

  it should "be caught applying must-links first-wins (mutation)" in {
    firstOrderViolation(Presentation.AsGiven) should not be empty
  }

  it should "never put a cannot-linked pair in one component" in {
    (1L to 3000L).foreach { seed =>
      val (nodes, cs) = graph(seed)
      val componentOf = ConstraintSolver.solve(nodes, cs).zipWithIndex.flatMap { case (c, i) => c.map(_ -> i) }.toMap
      cs.filterNot(_.must).foreach(c => componentOf(c.a) should not be componentOf(c.b))
    }
  }

  it should "leave a node whose must-links reach two cannot-linked components alone (A ~ B ~ C, A ≁ C)" in {
    val cs = Seq(Constraint("A", "B", must = true, 2, "same-title"), Constraint("B", "C", must = true, 2, "same-title"),
      Constraint("A", "C", must = false, 0, "different-films"))
    ConstraintSolver.solve(Seq("A", "B", "C"), cs) shouldBe Seq(Seq("A"), Seq("B"), Seq("C"))
    ConstraintSolver.solve(Seq("C", "B", "A"), cs.reverse) shouldBe Seq(Seq("A"), Seq("B"), Seq("C"))
    // …which first-wins decides by order: {A,B}{C} one way, {A}{B,C} the other.
    ConstraintSolver.solveAs(Seq("A", "B", "C"), cs, Presentation.AsGiven) should not be
      ConstraintSolver.solveAs(Seq("C", "B", "A"), cs.reverse, Presentation.AsGiven)
  }
}
