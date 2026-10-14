package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.UnmatchedClusters
import tools.UnmatchedClusters.Label

/**
 * `labels.tsv` agrees with itself and with the lists judged from it. Several agents label at once, and a listing judged
 * both ways is judged by whichever line [[UnmatchedClusters.verdict]] reads first — a verdict nobody chose.
 *
 *  - One listing (its venue, or every venue's "*") never calls one film both right and wrong. A venue's own label
 *    overruling every venue's is no contradiction: that is how a venue billing the other film is told apart (PL Kino
 *    Iluzjon's "Lalka", Has' 1968 film where every other "Lalka" is Kawalski's).
 *  - One listing is right for one film of a database, unless it is listed below with why: TMDB holding one film twice,
 *    or a venue billing two films under one title, which the label's venue key cannot tell apart.
 *  - `expected-matches.tsv` holds no take the labels call wrong, `known-wrong.tsv` none they call right.
 */
class LabelsConsistencySpec extends AnyFlatSpec with Matchers {

  private val labels = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))

  /** Listings right for two films of one database, (country, venue, raw title, database) → why. */
  private val twoRight: Map[(String, String, String, String), String] = Map(
    ("de", "*", "Ghost School", "tmdb") -> "TMDB holds Seemab Gul's film twice: 1516456 (2025) and 1599768 \"Geisterschule\" (2026)",
    ("pl", "*", "EVEREST: DRUGA STRONA", "tmdb") -> "TMDB holds Chin and Vasarhelyi's film twice: 1603262 and 1714568",
    ("pl", "*", "Everest: Druga strona", "tmdb") -> "TMDB holds Chin and Vasarhelyi's film twice: 1603262 and 1714568",
    ("de", "camera zwo - das arthouse kino", "Sinn und Sinnlichkeit", "tmdb") ->
      "the venue bills Ang Lee's 1995 film and Georgia Oakley's 2026 one under one title, two listings one label key",
    ("de", "Apollo Hannover", "Emil und die Detektive", "tmdb") ->
      "the venue bills Franziska Buch's 2001 film and David Dietl's 2026 one under one title, each kinoheld page stating its own")

  "a listing" should "never be labelled both right and wrong for one film by the same venue (or by every venue)" in {
    val both = labels.groupBy(l => (l.country, l.venue, l.rawTitle, l.film)).filter(_._2.map(_.right).distinct.sizeIs > 1)
    withClue(both.keys.toSeq.sorted.map(_.productIterator.mkString("\t")).mkString("judged both right and wrong:\n", "\n", "\n")) {
      both shouldBe empty
    }
  }

  it should "be right for one film of a database, or say why it is two" in {
    val two = labels.filter(_.right).groupBy(l => (l.country, l.venue, l.rawTitle, database(l))).filter(_._2.map(_.film).distinct.sizeIs > 1)
    val unexplained = two.keySet -- twoRight.keySet
    withClue(unexplained.toSeq.sorted.map(key => s"${key.productIterator.mkString("\t")}: ${two(key).map(_.film).distinct.sorted.mkString(", ")}")
        .mkString("right for two films of one database:\n", "\n", "\n")) {
      unexplained shouldBe empty
    }
    withClue("struck from labels.tsv — strike the exemption: ")((twoRight.keySet -- two.keySet) shouldBe empty)
  }

  "expected-matches.tsv" should "hold no take the labels call wrong" in {
    val wrong = UnmatchedClusters.readTakeLines(UnmatchedClusters.Directory.resolve("expected-matches.tsv")).filter { case (cc, venue, raw, film) =>
      verdictOf(cc, venue, raw, film).contains(false)
    }
    withClue(wrong.toSeq.sorted.mkString("labelled wrong:\n", "\n", "\n"))(wrong shouldBe empty)
  }

  "known-wrong.tsv" should "hold no take the labels call right at every venue" in {
    val right = UnmatchedClusters.readKnownWrong(UnmatchedClusters.KnownWrongPath)
      .filter { case (cc, raw, film) => verdictOf(cc, "*", raw, film).contains(true) }
    withClue(right.toSeq.sorted.mkString("labelled right:\n", "\n", "\n"))(right shouldBe empty)
  }

  private def database(label: Label): String = label.film.takeWhile(_ != ':')

  /** The verdict the labels give `film` at `venue`, its own label overruling every venue's ([[UnmatchedClusters.verdict]]). */
  private def verdictOf(cc: String, venue: String, raw: String, film: String): Option[Boolean] = {
    val about = labels.filter(l => l.country == cc && l.rawTitle == raw && l.film == film)
    about.find(_.venue == venue).orElse(about.find(_.venue == "*")).map(_.right)
  }
}
