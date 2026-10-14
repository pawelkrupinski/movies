package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.IdentityJudgements
import tools.UnmatchedClusters.Label

/**
 * The identity judgements agree with themselves (one listing judged both ways is a duplicate key the judgements lint,
 * `scripts/ci/identity_judgements_lint.py`, fails, as it fails an expectation or a tolerance they contradict):
 *
 *  - One listing is right for one film of a database, unless it is listed below with why: TMDB holding one film twice,
 *    or a venue billing two films under one title, which a judgement's venue key cannot tell apart.
 */
class LabelsConsistencySpec extends AnyFlatSpec with Matchers {

  private val labels = IdentityJudgements.labels()

  private val RoyalOperaCarmen =
    "TMDB holds the Royal Opera's 10 November 2026 Carmen broadcast twice: 1151422 and 1702759, the season's record taken first"
  /** Listings right for two films of one database, (country, venue, raw title, database) → why. */
  private val twoRight: Map[(String, String, String, String), String] = Map(
    ("de", "*", "Ghost School", "tmdb") -> "TMDB holds Seemab Gul's film twice: 1516456 (2025) and 1599768 \"Geisterschule\" (2026)",
    ("pl", "*", "EVEREST: DRUGA STRONA", "tmdb") -> "TMDB holds Chin and Vasarhelyi's film twice: 1603262 and 1714568",
    ("pl", "*", "Everest: Druga strona", "tmdb") -> "TMDB holds Chin and Vasarhelyi's film twice: 1603262 and 1714568",
    ("uk", "*", "Royal Opera House: Carmen", "tmdb") -> RoyalOperaCarmen,
    ("uk", "*", "Carmen (ROH)", "tmdb") -> RoyalOperaCarmen,
    ("uk", "*", "ROYAL BALLET & OPERA LIVE_ CARMEN", "tmdb") -> RoyalOperaCarmen,
    ("de", "camera zwo - das arthouse kino", "Sinn und Sinnlichkeit", "tmdb") ->
      "the venue bills Ang Lee's 1995 film and Georgia Oakley's 2026 one under one title, two listings one label key",
    ("de", "Apollo Hannover", "Emil und die Detektive", "tmdb") ->
      "the venue bills Franziska Buch's 2001 film and David Dietl's 2026 one under one title, each kinoheld page stating its own")

  "a listing" should "be right for one film of a database, or say why it is two" in {
    val two = labels.filter(_.right).groupBy(l => (l.country, l.venue, l.rawTitle, database(l))).filter(_._2.map(_.film).distinct.sizeIs > 1)
    val unexplained = two.keySet -- twoRight.keySet
    withClue(unexplained.toSeq.sorted.map(key => s"${key.productIterator.mkString("\t")}: ${two(key).map(_.film).distinct.sorted.mkString(", ")}")
        .mkString("right for two films of one database:\n", "\n", "\n")) {
      unexplained shouldBe empty
    }
    withClue("struck from the judgements — strike the exemption: ")((twoRight.keySet -- two.keySet) shouldBe empty)
  }

  private def database(label: Label): String = label.film.takeWhile(_ != ':')
}
