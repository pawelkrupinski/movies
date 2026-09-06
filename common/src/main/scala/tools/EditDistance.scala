package tools

/**
 * Levenshtein distance — insertions, deletions and substitutions — in the two forms
 * the corpus asks for: the distance itself, and the cheaper "is it within N", which
 * abandons the walk as soon as every path has spent the budget.
 *
 * One implementation. The title-side comparison (`TitleCorroboration`) and the
 * person-side one (`SamePerson`) each carried their own, and the person-side one had
 * already replaced a greedy walk that mis-scored a substitution following a deletion
 * ("Sokourov" against "Sokurow" — exactly the shape two transliterations take).
 */
object EditDistance {

  /** The full distance between `a` and `b`. */
  def between(a: String, b: String): Int =
    if (a.isEmpty) b.length
    else if (b.isEmpty) a.length
    else {
      var previous = (0 to b.length).toArray
      var row      = new Array[Int](b.length + 1)
      var i = 1
      while (i <= a.length) {
        row(0) = i
        var j = 1
        while (j <= b.length) {
          val substitution = previous(j - 1) + (if (a.charAt(i - 1) == b.charAt(j - 1)) 0 else 1)
          row(j) = math.min(math.min(row(j - 1) + 1, previous(j) + 1), substitution)
          j += 1
        }
        val swap = previous; previous = row; row = swap
        i += 1
      }
      previous(b.length)
    }

  /** True when `a` and `b` are at most `max` edits apart. Allocation-light and
   *  bounded: once the best cell of a row exceeds `max`, every distance from there
   *  on does too, so the walk stops. */
  def within(a: String, b: String, max: Int): Boolean = {
    if (math.abs(a.length - b.length) > max) return false
    if (a.isEmpty || b.isEmpty) return math.max(a.length, b.length) <= max
    var previous = (0 to b.length).toArray
    var row      = new Array[Int](b.length + 1)
    var i = 1
    while (i <= a.length) {
      row(0) = i
      var best = row(0)
      var j = 1
      while (j <= b.length) {
        val substitution = previous(j - 1) + (if (a.charAt(i - 1) == b.charAt(j - 1)) 0 else 1)
        row(j) = math.min(math.min(row(j - 1) + 1, previous(j) + 1), substitution)
        best   = math.min(best, row(j))
        j += 1
      }
      if (best > max) return false
      val swap = previous; previous = row; row = swap
      i += 1
    }
    previous(b.length) <= max
  }
}
