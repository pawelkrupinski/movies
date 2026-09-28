package integration

import services.identity.{Evidence, IdentityMeasures}

/**
 * The benchmark's ABSOLUTE referee: one listing's film, old or new, judged ALONE from the
 * listing's own published facts — never by comparing the two sides, and never an input to the
 * resolver. The comparison then reads an old match judged `Wrong` as unresolved (unresolved beats
 * wrong) and counts a new one as the resolver's error, which no change may increase.
 *
 * Wrong: two independent denials, or a performing-arts HOUSE the film's record contradicts ("Met
 * Opera 2026-27: Macbeth" is not the 2025 film, "MetOpera: Carmen (2009)" not the Opéra Comique's).
 * Right: a fact agrees and none denies. Otherwise unknown. The facts are the resolver's own
 * comparators (`IdentityMeasures.ownAgreement`: name order and scripts handled), with the referee's
 * own caution: a year denies only more than two apart (a festival's or re-release's year), an
 * original title only agrees (venues put an English title there), and a runtime 30 minutes off or
 * a season starting after the film denies, and so do two titles numbering their editions apart
 * ("League of Legends Worlds 26" and "… Worlds25").
 */
object IdentityReferee {

  enum Verdict { case Right, Wrong, Unknown }

  /** Houses a cinema broadcast names; the referee's lexicon. */
  private val Houses: Map[String, scala.util.matching.Regex] = Map(
    "met"       -> """\bmet\b|metopera|metropolitan opera""".r,
    "royal"     -> """royal ballet|royal opera|\brbo\b|\broh\b""".r,
    "nt"        -> """national theatre|\bnt live\b""".r,
    "bolshoi"   -> """bolshoi""".r,
    "comique"   -> """opera comique""".r,
    "paris"     -> """opera (national )?de paris|paris opera""".r,
    "glynde"    -> """glyndebourne""".r,
    "scala"     -> """la scala""".r,
    "rsc"       -> """royal shakespeare|\brsc\b""".r,
    "australia" -> """opera australia""".r,
    "wien"      -> """wiener staatsoper|vienna state opera""".r)
  private val WrittenYear = """(?<!\d)(?:18|19|20)\d{2}(?!\d)""".r
  /** The edition numbers a title writes: every run of one to three digits, glued to a word or not
   *  ("Worlds25", "Worlds 26"); four digits are a year. */
  private val EditionNumber = """(?<!\d)\d{1,3}(?!\d)""".r
  private def editionNumbers(title: String): Set[Int] = EditionNumber.findAllIn(title).map(_.toInt).toSet
  private def fold(s: String): String =
    java.text.Normalizer.normalize(s.toLowerCase(java.util.Locale.ROOT), java.text.Normalizer.Form.NFD).replaceAll("\\p{M}", "")
  private def housesOf(s: String): Set[String] = Houses.collect { case (h, p) if p.findFirstIn(fold(s)).isDefined => h }.toSet

  def judge(e: Evidence, f: IdentityMeasures.Film): (Verdict, Seq[String]) = {
    val m = IdentityMeasures.listingFilm(e.measured, f, None, 0, 0)
    val (agree, deny) = IdentityMeasures.ownAgreement(m)
    val yearFar = m.get("year.distance").exists { case IdentityMeasures.Number(d) => d > 2; case _ => false }
    val runtimeFar = e.runtime.zip(f.runtime.filter(_ > 0)).exists { case (a, b) => math.abs(a - b) >= 30 }
    val seasonAfter = e.measured.seasonYear.zip(f.year).exists { case (s, y) => s - y > 1 }
    // A year the ORIGINAL title writes ("The Royal Ballet: The Nutcracker (2024)") is a published year too.
    val originalYears = e.originalTitle.toSeq.flatMap(WrittenYear.findAllIn(_).map(_.toInt))
    val originalYearFar = e.year.isEmpty && originalYears.nonEmpty && f.year.exists(y => originalYears.forall(w => math.abs(w - y) > 2))
    val (lh, fh) = (housesOf(e.rawTitle), housesOf((Seq(f.title) ++ f.originalTitle).mkString(" ")))
    val house = lh.nonEmpty && fh.nonEmpty && (lh intersect fh).isEmpty
    // Both titles number an edition, and no number agrees: "Worlds 26" is not "Worlds25".
    val (ln, fn) = (editionNumbers(e.rawTitle), (Seq(f.title) ++ f.originalTitle).flatMap(editionNumbers).toSet)
    val titleNumber = ln.nonEmpty && fn.nonEmpty && (ln intersect fn).isEmpty
    val denials = Seq("year" -> yearFar, "director" -> deny("director"), "runtime" -> runtimeFar, "season" -> seasonAfter,
      "originalTitleYear" -> originalYearFar, "house" -> house, "titleNumber" -> titleNumber)
      .collect { case (name, true) => name }
    if (house || denials.sizeIs >= 2) (Verdict.Wrong, denials)
    else if (agree.nonEmpty && denials.isEmpty) (Verdict.Right, Nil)
    else (Verdict.Unknown, denials)
  }
}
