package controllers

object TitleNormalizer {
  // "Mortal Kombat 2" and "Mortal Kombat II" should collapse.
  private val ArabicToRoman = Map(
    "1" -> "I", "2" -> "II", "3" -> "III", "4" -> "IV", "5" -> "V",
    "6" -> "VI", "7" -> "VII", "8" -> "VIII", "9" -> "IX", "10" -> "X",
    "11" -> "XI", "12" -> "XII", "13" -> "XIII", "14" -> "XIV", "15" -> "XV",
    "16" -> "XVI", "17" -> "XVII", "18" -> "XVIII", "19" -> "XIX", "20" -> "XX"
  )

  // Always-applied transformation: standalone Arabic numerals → Roman.
  def normalize(title: String): String =
    title.split(" ").map(word => ArabicToRoman.getOrElse(word, word)).mkString(" ")

  // Conditional cleanups, applied only when another title in `allTitles` would
  // reduce to the same canonical form. Keeps lone "Gwiezdne Wojny: A New Hope"
  // or "Pizza & Pasta" intact when there's no merge partner.
  private def canonical(t: String): String =
    t.stripPrefix("Gwiezdne Wojny: ").replace(" & ", " i ")

  // Group key for merging. Falls back to the plain Roman-numeral form when no
  // sibling title reduces to the same canonical.
  def mergeKey(title: String, allTitles: Iterable[String]): String =
    mergeKeyLookup(allTitles)(title)

  // Faster batch entry point: when caller has many titles to key, pre-compute
  // the canonical→count index once (O(N)) and then look up each title in O(1).
  // Caller iterates with `index(title)`. Equivalent semantics to `mergeKey`.
  def mergeKeyLookup(allTitles: Iterable[String]): String => String = {
    val romanized = allTitles.iterator.map(normalize).toSet
    // Counts how many distinct romanized titles share each canonical form.
    val canonicalCounts: Map[String, Int] =
      romanized.iterator.map(canonical).toSeq.groupBy(identity).view.mapValues(_.size).toMap
    title => {
      val r = normalize(title)
      val c = canonical(r)
      if (c == r) r.toLowerCase
      // A canonical with count > 1 means at least one *other* romanized title
      // reduces to the same canonical, so we have a real merge — use it.
      else if (canonicalCounts.getOrElse(c, 0) > 1) c.toLowerCase
      else r.toLowerCase
    }
  }

  // Among a group of titles that merge to one schedule, pick the display form.
  // When a merge happens (two or more distinct titles), always show the
  // canonical form — " & " replaced with " i " and the "Gwiezdne Wojny: "
  // prefix stripped — even when the canonical form isn't literally present in
  // the input. That way "Mandalorian i Grogu" wins over both
  // "Mandalorian & Grogu" and "Gwiezdne Wojny: Mandalorian i Grogu", regardless
  // of which spellings the cinemas happened to ship.
  // A single-title group is returned untouched to avoid mutating standalone
  // titles like "Gwiezdne Wojny: A New Hope" or "Pizza & Pasta" that did not
  // actually trigger a merge.
  def preferredDisplay(titles: Iterable[String]): Option[String] = {
    val seq = titles.iterator.toSeq.distinct
    if (seq.size <= 1) seq.headOption
    else Some(canonical(seq.head))
  }
}
