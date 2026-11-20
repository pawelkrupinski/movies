package tools

import org.scalatest.Assertions

/**
 * A REFACTOR'S EQUIVALENCE PHASE: a frozen copy of the old implementation and the new one read every input of a recorded
 * corpus -- captures, drift slices, fixtures -- each reading rendered as lines (its outputs and its trace strings), and
 * every input's lines must be byte-identical. What lane A1's AcceptanceEquivalenceSpec (93e70fee7) and the billing
 * parsers' equivalence spec (0da0c8d74) each wrote by hand: the frozen copy and the spec using this go once the rewrite
 * has landed.
 *
 * [[assertEquivalent]] proves the comparison can fail before trusting it passes: the new side's reading of the first
 * input, deliberately broken (one line changed), and then the frozen side's, must each be caught as a mismatch. It
 * refuses a side compared with itself (the one function handed twice), a corpus smaller than its `minimum` (a fixture
 * that went missing) and one whose every reading is the same (a reading that says nothing).
 *
 * Compiled into common's and identity's tests too (build.sbt's sharedTestDoubles), beside testkit's own.
 */
object Equivalence {

  /** One input the two sides read differently: each side's lines. */
  final case class Mismatch[I](input: I, frozen: Seq[String], current: Seq[String]) {
    /** The input, then the lines only the frozen side read (`was`) and only the new side did (`now`). */
    def render(show: I => String): String =
      s"\n  ${show(input)}\n    was ${frozen.filterNot(current.contains).mkString("\n        ")}\n    now ${current.filterNot(frozen.contains).mkString("\n        ")}"
  }

  /** How many inputs were compared, how many distinct readings the new side gave, and the first mismatches kept. */
  final case class Verdict[I](compared: Int, distinct: Int, mismatches: Seq[Mismatch[I]], mismatched: Int)

  /** Both sides' readings of every input, compared line for line; the first `keep` mismatches kept. */
  def compare[I](inputs: Iterable[I], frozen: I => Seq[String], current: I => Seq[String], keep: Int = 5): Verdict[I] = {
    val kept = Seq.newBuilder[Mismatch[I]]
    val readings = scala.collection.mutable.Set.empty[Seq[String]]
    var compared = 0
    var mismatched = 0
    inputs.foreach { input =>
      val (was, now) = (frozen(input), current(input))
      compared += 1
      readings += now
      if (was != now) { if (mismatched < keep) kept += Mismatch(input, was, now); mismatched += 1 }
    }
    Verdict(compared, readings.size, kept.result(), mismatched)
  }

  /** `reading` with its reading of `input` broken: the first line changed, or a line added where it read none. */
  def broken[I](reading: I => Seq[String], input: I): I => Seq[String] = { i =>
    val lines = reading(i)
    if (i != input) lines else lines.headOption.fold(Seq("<deliberately broken>"))(first => (first + " <deliberately broken>") +: lines.tail)
  }

  /** The equivalence phase's one assertion: `frozen` and `current` read every one of `inputs` (at least `minimum` of
   *  them) byte-identically, after the self-checks above. `show`: how a mismatch names its input. The verdict, for the
   *  spec's `info`. */
  def assertEquivalent[I](inputs: Iterable[I], frozen: I => Seq[String], current: I => Seq[String], minimum: Int,
                          show: I => String = (i: I) => String.valueOf(i)): Verdict[I] = {
    val corpus = inputs.toSeq
    if (frozen eq current) Assertions.fail("the frozen side and the new one are the same function: it compares a side with itself")
    if (corpus.size < minimum) Assertions.fail(s"the corpus holds ${corpus.size} inputs, fewer than the $minimum it must: a fixture went missing?")
    corpus.headOption.foreach { first =>
      Seq("new" -> compare(Seq(first), frozen, broken(current, first)), "frozen" -> compare(Seq(first), broken(frozen, first), current))
        .collect { case (side, v) if v.mismatched != 1 => side }
        .foreach(side => Assertions.fail(s"the comparison missed the $side side's deliberately broken reading of ${show(first)}: it asserts nothing"))
    }
    val verdict = compare(corpus, frozen, current)
    if (corpus.size > 1 && verdict.distinct < 2) Assertions.fail(s"every one of the ${corpus.size} inputs reads the same: the reading says nothing")
    if (verdict.mismatched > 0)
      Assertions.fail(s"${verdict.mismatched} of ${verdict.compared} inputs read differently:" + verdict.mismatches.map(_.render(show)).mkString)
    verdict
  }
}
