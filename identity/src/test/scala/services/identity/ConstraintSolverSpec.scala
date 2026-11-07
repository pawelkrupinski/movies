package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** A2 at the solver: the partition is a function of the node and constraint SETS, no component
 *  holds a cannot-linked pair or two films, and the ambiguity rule leaves a node reaching two cannot-linked
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

  // Torrington "Dracula" (drift slice plough-arts-centre-torrington-dracula-625b65d9): the bare title's same-title links
  // reach the 1931 and the 1958 films, so it is ambiguous; its weaker same-search-form link to the 1958 one alone used
  // to settle it, and the take went 11868 -> 138 -> 11868 as the listings arrived.
  it should "leave a node a stronger tier found ambiguous alone in the weaker tiers too (B ~2 X, B ~2 Y, B ~3 Y)" in {
    val cs = Seq(Constraint("B", "X", must = true, 2, "same-title"), Constraint("B", "Y", must = true, 2, "same-title"),
      Constraint("B", "Y", must = true, 3, "same-search-form"))
    val films = Map("X" -> 138, "Y" -> 11868)
    ConstraintSolver.solve(Seq("B", "X", "Y"), cs, films) shouldBe Seq(Seq("B"), Seq("X"), Seq("Y"))
    // Without the 1931 node the weaker link joins it, as before.
    ConstraintSolver.solve(Seq("B", "Y"), cs.filterNot(_.b == "X"), films) shouldBe Seq(Seq("B", "Y"))
  }

  it should "leave a node cannot-linked to one component it reaches alone, never hand it to the other (U ~ M, U ~ O, U ≁ M)" in {
    // UK "Sense and Sensibility [U]" [132′] (U), apart from the 2026 film's spellings (M) and must-linked to them and to
    // the "Baby & Me: …" screenings of the 1995 film (O): read as no choice, the link it cannot take left it the 1995
    // film's, and its siblings' bare spellings, reaching it there, lost the 2026 film (recording 38363428661)
    val cs = Seq(Constraint("M", "U", must = true, 3, "same-search-form"), Constraint("O", "U", must = true, 3, "same-search-form"),
      Constraint("M", "U", must = false, 0, "denies-film"))
    ConstraintSolver.solve(Seq("M", "O", "U"), cs, Map("M" -> 1503762, "O" -> 4584)) shouldBe Seq(Seq("M"), Seq("O"), Seq("U"))
  }

  it should "never put two films in one component, though no cannot-link joins their nodes" in {
    // The PL festival chain: A holds one film, D another, and must-links run A ~ B, B ~ C, C ~ D
    // across tiers with no cannot-link between A and D — they share no key.
    val cs = Seq(Constraint("B", "C", must = true, 3, "same-search-form"), Constraint("A", "B", must = true, 4, "title-segment"),
      Constraint("C", "D", must = true, 4, "title-segment"))
    ConstraintSolver.solve(Seq("A", "B", "C", "D"), cs) shouldBe Seq(Seq("A", "B", "C", "D"))
    val films = Map("A" -> 1, "D" -> 2)
    val components = ConstraintSolver.solve(Seq("A", "B", "C", "D"), cs, films)
    components.foreach(c => c.flatMap(films.get).distinct.size should be <= 1)
    // Tier 4 joins A's film first, in the canonical order; D's then stays apart.
    components shouldBe Seq(Seq("A", "B", "C"), Seq("D"))
  }

  it should "never put two films in one component, whatever the graph" in {
    (1L to 3000L).foreach { seed =>
      val (nodes, cs) = graph(seed)
      val films = nodes.zipWithIndex.collect { case (n, i) if i % 3 == 0 => n -> (i % 2) }.toMap
      ConstraintSolver.solve(nodes, cs, films).foreach(c => c.flatMap(films.get).distinct.size should be <= 1)
    }
  }

  // Rule 3's ambiguity test, asked of every node a tier joins: a bare title must-linked to every spelling of a family
  // reaches every one of their roots, so a test over every PAIR of roots made the solver cubic in a family of kept spellings
  "The ambiguity test" should "answer as the pairwise test does, whatever the roots, cannot-links and films" in {
    import scala.collection.mutable
    (1L to 3000L).foreach { seed =>
      val rnd    = new Random(seed)
      val ids    = 0 until (1 + rnd.nextInt(12))
      val roots  = ids.filter(_ => rnd.nextDouble() < 0.6).toSet
      val cannot = mutable.HashMap.from(ids.map(i => i -> mutable.Set.from(ids.filter(j => j != i && rnd.nextDouble() < 0.1))))
      val film   = mutable.HashMap.from(ids.filter(_ => rnd.nextDouble() < 0.3).map(i => i -> rnd.nextInt(3)))
      val pairwise = roots.exists(r => roots.exists(ConstraintSolver.forbids(cannot, film, r, _)))
      withClue(s"seed $seed: ")(ConstraintSolver.anyForbidden(roots, cannot, film) shouldBe pairwise)
    }
  }

  it should "read each root's cannot-links once, not once per pair of roots" in {
    import scala.collection.mutable
    // a hub reaching 2,000 roots, none of them apart: the pairwise test read the cannot-links 4,000,000 times
    var reads = 0
    // counted by a map that tells its reads: the solver's own map type, whose subclassing is deprecated, not forbidden
    @scala.annotation.nowarn("cat=deprecation")
    val cannot = new mutable.HashMap[Int, mutable.Set[Int]]() {
      override def getOrElse[V1 >: mutable.Set[Int]](key: Int, default: => V1): V1 = { reads += 1; super.getOrElse(key, default) }
    }
    (0 until 2000).foreach(i => cannot(i) = mutable.Set(i + 5000))
    ConstraintSolver.anyForbidden((0 until 2000).toSet, cannot, mutable.HashMap.empty) shouldBe false
    reads should be <= 2000
  }
}
