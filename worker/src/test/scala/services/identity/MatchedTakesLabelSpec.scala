package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentityUnifiedFit
import tools.UnmatchedClusters
import tools.UnmatchedClusters.Take

/**
 * The model's MATCHED takes graded against the hand labels — the clusters [[UnmatchedClustersRatchetSpec]] never sees:
 * its fixture captures only the clusters the model leaves unmatched, so a wrong film the model takes outright is
 * invisible to it (2026-10-06: four such takes found only by an offline ML audit of the unified rows).
 *
 * Every cluster of the unified evidence rows ([[IdentityUnifiedFit.Training]]: every cluster of the five recorded
 * corpora, matched ones included) whose take today's stack makes is judged by `labels.tsv` as the ratchet judges a
 * take, by the cluster's venue and most common raw title. A take a label denies or contradicts fails unless it is
 * acknowledged below with why it still stands — a wrong take is either fixed, or its label is, or it is named here.
 * The rows are re-emitted by `integration.IdentityUnifiedDataset`; a resolver change grades here once they are.
 */
class MatchedTakesLabelSpec extends AnyFlatSpec with Matchers {

  private val labels = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
  private val rows   = IdentityUnifiedFit.read(IdentityUnifiedFit.Training)

  private lazy val multiFilmBill = "a multi-film bill: no film is taken for it since 4456afbbc — struck once the rows are re-emitted"

  /** Known wrong takes in the recorded rows, (country, raw title, taken film) → why each stands. */
  private val acknowledged: Map[(String, String, String), String] = Map(
    ("pl", "Teksańska masakra piłą mechaniczną", "tmdb:632727") ->
      "the rows record the model's take before agreement.Correction, which switches it to the 1974 film (prod, 2026-10-06)",
    ("pl", "Ktoś całkiem obcy", "tmdb:7183") ->
      "Correction switches it to I Was a Stranger once Kino Kryterium's poster is hashed — through the Zyte poster route",
    ("us", "NT Live: All My Sons", "tmdb:568683") ->
      "the venue's feed (Flicks) links the Oriental's 2026 relay to its 2019 page — year, director, cast and synopsis all 2019's",
    // fixed after the rows were recorded: 4456afbbc matches no film for a listing a word bills as several
    ("pl", "Maraton Horrorów", "tmdb:1193501") -> multiFilmBill,
    ("uk", "The Dark Knight Trilogy", "tmdb:155") -> multiFilmBill,
    ("uk", "Triple Feature: Lord of the Rings", "tmdb:122") -> multiFilmBill,
    ("us", "Triple Feature: Lord of the Rings", "tmdb:122") -> multiFilmBill)

  private def take(row: IdentityUnifiedFit.Row): Take = Take(row.country, row.venue, row.rawTitle,
    row.film.stripPrefix("tmdb:").toIntOption.filter(_ => row.film.startsWith("tmdb:")),
    Option.when(row.film.startsWith("imdb:"))(row.film.stripPrefix("imdb:")), "", row.filmTitle)

  private lazy val wrong = rows.filter(_.today).map(take).filter(UnmatchedClusters.verdict(_, labels).contains(false))

  "the model's matched takes" should "take no film a hand label denies or contradicts, beyond the acknowledged ones" in {
    val unacknowledged = wrong.filterNot(t => acknowledged.contains((t.country, t.rawTitle, t.film)))
    withClue(unacknowledged.map(UnmatchedClusters.takeLine).mkString("WRONG matched takes:\n", "\n", "\n")) { unacknowledged shouldBe empty }
  }

  it should "still take every acknowledged wrong film — one fixed is struck from the list" in {
    val stale = acknowledged.keySet -- wrong.map(t => (t.country, t.rawTitle, t.film)).toSet
    withClue(stale.mkString("no longer taken wrong — remove from `acknowledged`:\n", "\n", "\n")) { stale shouldBe empty }
  }
}
