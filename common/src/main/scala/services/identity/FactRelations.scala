package services.identity

import services.identity.IdentityMeasures.{Category, directorRelation}
import services.resolution.YearWindow

/**
 * The ONE place a rule compares two sides' published facts outside the calibrated measures: a listing's or a family
 * record's year and directors against a film's — the agreement's votes and contradictions, the broadcast join's fit,
 * a correction's evidence, an edition's year. The calibrated measures ([[IdentityMeasures.listingFilm]]) weigh the same
 * facts as numbers; these are the yes/no reads the guards, takes and corrections make of them.
 *
 * Years are NEAR within [[YearWindow.PublishedAdjacency]] (one year: a festival year beside a release year), and
 * APART only when both are stated and are not. Directors are compared by [[IdentityMeasures.directorRelation]]: an
 * empty side is never the same person nor another one.
 */
object FactRelations {
  private val SamePerson      = Category("same_person")
  private val Different       = Category("different")
  private val DifferentScript = Category("different_script")

  /** Are two stated years within [[YearWindow.PublishedAdjacency]] of each other? */
  def yearsNear(a: Int, b: Int): Boolean = math.abs(a - b) <= YearWindow.PublishedAdjacency

  /** A year difference (a measure's `titleYear.delta`) within [[YearWindow.PublishedAdjacency]]. */
  def nearDelta(delta: Double): Boolean = math.abs(delta) <= YearWindow.PublishedAdjacency

  /** Both years stated and near: positive agreement. */
  def yearsAgree(a: Option[Int], b: Option[Int]): Boolean = a.zip(b).exists { case (x, y) => yearsNear(x, y) }

  /** Both years stated and not near: positive evidence of another film. Never true on silence. */
  def yearsApart(a: Option[Int], b: Option[Int]): Boolean = a.zip(b).exists { case (x, y) => !yearsNear(x, y) }

  /** Do the two credits name one person? */
  def samePerson(a: Seq[String], b: Seq[String]): Boolean = directorRelation(a, b) == SamePerson

  /** Are the two credits other people, in the same script? */
  def otherPerson(a: Seq[String], b: Seq[String]): Boolean = directorRelation(a, b) == Different

  /** Are the two credits other PEOPLE — other names, in the same script or in two (transliterated,
   *  [[IdentityMeasures.directorRelation]]'s `different_script`), sharing no name's stem ([[namePrefixes]]: "Marc Donskoi"
   *  is "Mark Donskoy", "Simona Risi" is "Simona Lina Risi", "Andrei Tarkovsky" is "Андрей Тарковский")? ONE reading for
   *  every rule that rules a film out by its director: the agreement's listing contradiction and a correction's. */
  def otherPeople(a: Seq[String], b: Seq[String]): Boolean = {
    val relation = directorRelation(a, b)
    (relation == Different || relation == DifferentScript) && namePrefixes(a).intersect(namePrefixes(b)).isEmpty
  }

  /** Each name's words of four letters or more, in Latin letters ([[IdentityMeasures.latinized]]), folded to ASCII and cut
   *  to their first four. */
  def namePrefixes(names: Seq[String]): Set[String] =
    names.flatMap(name => tools.TextNormalization.deburr(IdentityMeasures.latinized(name)).toLowerCase(java.util.Locale.ROOT).split("[^a-z]+"))
      .filter(_.length >= 4).map(_.take(4)).toSet
}
