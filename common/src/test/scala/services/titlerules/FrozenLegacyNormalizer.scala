package services.titlerules

/**
 * A reference copy of the `TitleNormalizer` global normalisation chains.
 * `ProdTitlesNormalizationSpec` asserts the default rule set reproduces this
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
  // Mirror `TitleRuleDefaults`' meeting rules (kept in lockstep so this
  // reference reproduces the engine on the prod corpus):
  //   `search-spotkanie-banner-prefix` (order 11) — a 'Spotkania …:' banner the
  //      film follows ("SPOTKANIA FILOZOFICZNE: Wędrówka na północ").
  //   `search-meeting-suffix` (order 15) — a 'spotkanie' annotation introduced
  //      by any separator (+ | – —), with a lookahead that keeps a real film
  //      whose title merely contains 'spotkania' intact.
  private val SpotkanieBannerPrefix = """(?i)^(?:Filmowe\s+)?spotkani\p{L}*[^:]*:\s*""".r
  private val MeetingSuffix =
    ("""\s*[-–—|+]\s*(?:[^|]*?\s)?(?i:spotkani)\p{L}*""" +
     """(?=\s+(?i:z|ze|po|przed)\b|\s*[|+]|\s+\p{Lu}|\s*$).*$""").r
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
    val stripped  = ProgrammePrefix.replaceFirstIn(display, "")        // order 10
    val bannerless = SpotkanieBannerPrefix.replaceFirstIn(stripped, "") // order 11
    val tagless   = AccessibilityTag.replaceFirstIn(bannerless, "")    // order 12
    val eventless = PlusSuffix.replaceFirstIn(tagless, "")             // order 14
    val meetingless = MeetingSuffix.replaceFirstIn(eventless, "")      // order 15
    val narrationless = WithEventSuffix.replaceFirstIn(meetingless, "") // order 16
    searchTitle(narrationless)
  }
  def programmePrefix(title: String): Option[String] =
    ProgrammePrefix.findPrefixMatchOf(title).map(_.matched)

  private val RomanToArabic = Map(
    "II" -> "2", "III" -> "3", "IV" -> "4", "VI" -> "6", "VII" -> "7",
    "VIII" -> "8", "IX" -> "9", "XI" -> "11", "XII" -> "12", "XIII" -> "13",
    "XIV" -> "14", "XV" -> "15", "XVI" -> "16", "XVII" -> "17", "XVIII" -> "18",
    "XIX" -> "19", "XX" -> "20")
  def normalize(title: String): String =
    title.split(" ").map(w => RomanToArabic.getOrElse(w.toUpperCase(java.util.Locale.ROOT), w)).mkString(" ")
  // Identity canonical: the cross-cinema spelling unifications over the trimmed
  // title — NO `searchTitle`/structural decoration strip (that tier is now
  // external-lookup-only, see TitleRuleSet). Decoration editions key by their
  // own form and stay separate rows.
  private def canonical(t: String): String =
    t.trim.stripPrefix("Gwiezdne Wojny: ").replace(" & ", " i ")
  def sanitize(title: String): String =
    tools.TextNormalization.deburr(normalize(canonical(title)))
      .toLowerCase
      .replaceAll("[^\\p{L}\\p{N}]+", "")
}
