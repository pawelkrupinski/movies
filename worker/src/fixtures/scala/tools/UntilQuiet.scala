package tools

/** Repeat a harness phase until a round does no work — what production gets from its clock —
 *  but only `maxRounds` times. A producer that keeps re-asking for work already done (a due
 *  gate bypassed, a stamp that never lands) never goes quiet, and an unbounded loop turned that
 *  bug into a hung run that CI kills without a word about which phase or why. */
object UntilQuiet {

  /** Rounds the rating phase may take before it is declared non-converging. A healthy replay
   *  settles in one or two (a round's follow-ups — a resolved imdbId unlocking its rating — are
   *  the only reason for another); the corpora have never needed more than a handful. */
  val MaxRatingRounds = 25

  /** Run `round` (given its 1-based number, answering how much work it did) until it answers 0.
   *  Returns the number of rounds run. Throws once `maxRounds` rounds have all done work. */
  def apply(phase: String, maxRounds: Int)(round: Int => Int): Int = {
    var n    = 0
    var work = 1
    while (work > 0) {
      if (n == maxRounds)
        throw new IllegalStateException(s"$phase did not go quiet within $maxRounds rounds (the last did $work " +
          "unit(s) of work) — a producer is re-asking for work already done, so the loop would never end")
      n += 1
      work = round(n)
    }
    n
  }
}
