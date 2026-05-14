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

  // ── Cinema-decoration stripping ────────────────────────────────────────────
  //
  // Single set of patterns reused by:
  //   - mergeKey / preferredDisplay  — so "Top Gun 40th Anniversary" and a
  //     plain "Top Gun" listing collapse into one card.
  //   - EnrichmentService.searchTitle — so the TMDB/OMDb query is "Top Gun",
  //     not "Top Gun 40th Anniversary" (the latter doesn't match anything).
  //
  // Patterns target decoration that cinemas apply to a base film title —
  // anniversary rereleases, remasters, cycle screenings, bilingual postfixes.
  // Each is anchored so that a sequel ("Top Gun: Maverick", "Mortal Kombat II"),
  // a real-titled film ("Rocznica"), or a synopsis-style line can't be hit.
  private val CyklPrefix        = """^Cykl\s+[„"][^„""]*[„""]?\s+[-–—]\s+""".r
  private val SlashSuffix       = """\s+/\s+.+$""".r
  // Require the suffix to start with a letter so that mathematical titles
  // like "Orwell: 2 + 2 = 5" aren't truncated to "Orwell: 2".
  private val PlusSuffix        = """\s+\+\s+\p{L}.+$""".r
  private val AnniversarySuffix = """(?i)\s*[-–—|.]?\s*\d+(?:st|nd|rd|th)?\.?\s*(?:anniversary|rocznica)\s*$""".r
  private val RestoredSuffix    = """(?i)\s*[-–—|.]?\s*\d+\s*k\s+(?:restored|remaster(?:ed)?)\s*$""".r
  private val WersjaSuffix      = """(?i)\s*[-–—.]\s+wersja\s+\p{L}+\s*$""".r

  /** Strip cinema decoration (anniversary, restored, Cykl prefix, bilingual
   *  postfix, remaster suffix). Display titles intentionally keep these — only
   *  used for grouping and external-API lookups.
   */
  def searchTitle(display: String): String = {
    val a = CyklPrefix.replaceFirstIn(display, "")
    val b = SlashSuffix.replaceFirstIn(a, "")
    val c = PlusSuffix.replaceFirstIn(b, "")
    val d = AnniversarySuffix.replaceFirstIn(c, "")
    val e = RestoredSuffix.replaceFirstIn(d, "")
    WersjaSuffix.replaceFirstIn(e, "").trim
  }

  // Conditional cleanups for merging — applied only when another title in
  // `allTitles` reduces to the same canonical form. Builds on `searchTitle` so
  // anniversary/wersja variants merge with their base film, then adds the
  // cross-cinema spelling unifications (Gwiezdne Wojny prefix, " & " → " i ")
  // that aren't needed for enrichment lookups.
  private def canonical(t: String): String =
    searchTitle(t).stripPrefix("Gwiezdne Wojny: ").replace(" & ", " i ")

  // Group key for merging. Falls back to the plain Roman-numeral form when no
  // sibling title reduces to the same canonical.
  def mergeKey(title: String, allTitles: Iterable[String]): String =
    mergeKeyLookup(allTitles)(title)

  // Faster batch entry point: when caller has many titles to key, pre-compute
  // the canonical→count index once (O(N)) and then look up each title in O(1).
  // Caller iterates with `index(title)`. Equivalent semantics to `mergeKey`.
  //
  // Counts are keyed by *lower-cased* canonical so cross-cinema casing diffs
  // (e.g. Rialto's sentence-case "Top gun | 40 rocznica" alongside Helios's
  // "Top Gun 40th Anniversary") don't prevent a merge.
  def mergeKeyLookup(allTitles: Iterable[String]): String => String = {
    val romanized = allTitles.iterator.map(normalize).toSet
    val canonicalCounts: Map[String, Int] =
      romanized.iterator.map(t => canonical(t).toLowerCase).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap
    title => {
      val r       = normalize(title)
      val c       = canonical(r)
      val cLower  = c.toLowerCase
      val rLower  = r.toLowerCase
      if (cLower == rLower) rLower
      // A canonical with count > 1 means at least one *other* romanized title
      // reduces to the same canonical, so we have a real merge — use it.
      else if (canonicalCounts.getOrElse(cLower, 0) > 1) cLower
      else rLower
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
