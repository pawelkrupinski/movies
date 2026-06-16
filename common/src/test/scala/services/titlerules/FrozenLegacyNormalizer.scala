package services.titlerules

/**
 * A reference copy of the `TitleNormalizer` global normalisation chains. Both
 * the curated `TitleRuleMigrationSpec` and the prod-driven
 * `ProdTitlesNormalizationSpec` assert the default rule set reproduces this
 * byte-for-byte. It mirrors the ENGINE's tier composition, so it tracks
 * deliberate tier changes: `canonical` here drops `searchTitle` because the
 * structural decoration strip no longer feeds identity (it's external-lookup
 * only — see `TitleRuleSet`). To change normalisation BEHAVIOUR, edit the seed
 * rules in `TitleRuleDefaults`; to change the tier COMPOSITION, edit both the
 * engine (`TitleRuleSet`) and this reference in lockstep.
 */
object FrozenLegacyNormalizer {
  private val CyklPrefix        = """^Cykl\s+[„"][^„""]*[„""]?\s+[-–—]\s+""".r
  private val SlashSuffix       = """\s+/\s+.+$""".r
  private val AnniversarySuffix = """(?i)\s*[-–—|.]?\s*\d+(?:st|nd|rd|th)?\.?\s*(?:anniversary|rocznica)\s*$""".r
  private val RestoredSuffix    = """(?i)\s*[-–—|.]?\s*\d+\s*k\s+(?:restored|remaster(?:ed)?)\s*$""".r
  private val WersjaSuffix      = """(?i)\s*[-–—.]\s+wersja\s+\p{L}+\s*$""".r
  private val PlusSuffix        = """\s+\+\s+\p{L}[^)]*$""".r
  private val WithEventSuffix   = """(?i)\s*[-–—]?\s*z\s+(?:autorską\s+narracją|prelekcj[ąae]|wprowadzeniem|udziałem)\S*.*$""".r
  private val ProgrammePrefix   =
    ("""(?i)^(?:Kino\s+bez\s+barier|""" +
     """Pokaz\s+sensorycznie\s+przyjazny|""" +
     """Filmow[ey]\s+Poran(?:ki|ek)(?:\s+[^:]+)?|""" +
     """Zimowe\s+Poranki(?:\s+[^:]+)?|""" +
     """Poranek\s+dla\s+dzieci|""" +
     """Filmowy\s+Klub\s+Seniora|""" +
     """Dyskusyjny\s+Klub\s+Filmowy|""" +
     """Filmowe\s+spotkania\s+z\s+psychoanaliz[ąa]|""" +
     """Cinema\s+Italia\s+Oggi|""" +
     """Plenerowe\s+Pa[łl]acowe):\s+""").r
  private val AccessibilityTag  = """(?i)\s*\(\s*AD\b[^)]*\)?\s*$""".r

  def searchTitle(display: String): String = {
    val a = CyklPrefix.replaceFirstIn(display, "")
    val b = SlashSuffix.replaceFirstIn(a, "")
    val d = AnniversarySuffix.replaceFirstIn(b, "")
    val e = RestoredSuffix.replaceFirstIn(d, "")
    WersjaSuffix.replaceFirstIn(e, "").trim
  }
  def apiQuery(display: String): String = {
    val stripped  = ProgrammePrefix.replaceFirstIn(display, "")
    val tagless   = AccessibilityTag.replaceFirstIn(stripped, "")
    val eventless = PlusSuffix.replaceFirstIn(tagless, "")
    val narrationless = WithEventSuffix.replaceFirstIn(eventless, "")
    searchTitle(narrationless)
  }
  def programmePrefix(title: String): Option[String] =
    ProgrammePrefix.findPrefixMatchOf(title).map(_.matched)

  private val ArabicToRoman = Map(
    "1" -> "I", "2" -> "II", "3" -> "III", "4" -> "IV", "5" -> "V",
    "6" -> "VI", "7" -> "VII", "8" -> "VIII", "9" -> "IX", "10" -> "X",
    "11" -> "XI", "12" -> "XII", "13" -> "XIII", "14" -> "XIV", "15" -> "XV",
    "16" -> "XVI", "17" -> "XVII", "18" -> "XVIII", "19" -> "XIX", "20" -> "XX")
  def normalize(title: String): String =
    title.split(" ").map(w => ArabicToRoman.getOrElse(w, w)).mkString(" ")
  // Identity canonical: the cross-cinema spelling unifications over the trimmed
  // title — NO `searchTitle`/structural decoration strip (that tier is now
  // external-lookup-only, see TitleRuleSet). Decoration editions key by their
  // own form and stay separate rows.
  private def canonical(t: String): String =
    t.trim.stripPrefix("Gwiezdne Wojny: ").replace(" & ", " i ")
  def sanitize(title: String): String =
    tools.TextNormalization.deburr(canonical(normalize(title)))
      .toLowerCase
      .replaceAll("[^\\p{L}\\p{N}]+", "")
}
