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
 * corpora, matched ones included) whose take the model makes is judged by `labels.tsv` as the ratchet judges a take, by
 * the cluster's venue and most common raw title. The rows hold the MODEL's take, before the agreement stage reads it
 * again (`agreement.Correction`), and as the code stood when they were emitted (`integration.IdentityUnifiedDataset`).
 * So a wrong take fails unless it is listed below as one a later stage or a later change no longer serves — each with
 * the spec that proves it — or, named OPEN, a wrong take prod still serves that no fix took at 0 lost (one, 2026-10-06).
 */
class MatchedTakesLabelSpec extends AnyFlatSpec with Matchers {

  private val labels = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
  private val rows   = IdentityUnifiedFit.read(IdentityUnifiedFit.Training)

  /** The model's takes in the recorded rows a label judges wrong, (country, raw title, taken film) → why they stand: prod no
   *  longer serves them, the labels cannot tell two listings apart, or an OPEN finding. */
  private val noLongerServed: Map[(String, String, String), String] = Map(
    ("pl", "Teksańska masakra piłą mechaniczną", "tmdb:632727") ->
      "corrected to the 1974 film by the venue poster and the families (AgreementCorrectionSpec; live in prod 2026-10-06)",
    ("pl", "Ktoś całkiem obcy", "tmdb:7183") ->
      ("corrected once Kino Kryterium's poster is hashed through its Zyte route, or read without it once given up on " +
        "(AgreementCorrectionSpec, AgreementPostersSpec, ArchiveReplayEnrichmentWiringSpec)"),
    ("us", "NT Live: All My Sons", "tmdb:568683") ->
      "withdrawn as a superseded relay: TMDB dates it 2019, the screenings and the 2026 record of its title 2026 (AgreementCorrectionSpec)",
    ("pl", "Manon", "tmdb:132332") ->
      ("corrected to the Met's 2026/27 relay its 3 April 2027 screening names, as an agreed take yields to it " +
        "(AgreementBroadcastSpec; re-capture 2026-10-06)"),
    ("pl", "Lalka", "tmdb:81315") ->
      ("Kino Iluzjon bills two \"Lalka\" listings, its page 2457 Has's 1968 film (1968, Wojciech Jerzy Has), its page 7917 " +
        "Kawalski's 2026 one: the labels, keyed by venue and title, cannot tell the two apart — the take is right (re-capture 2026-10-06)"),
    ("us", "NT Live: Hamlet", "tmdb:396227") ->
      ("OPEN (re-capture 2026-10-06, served in prod): Burns Court Sarasota bills the 2026 Abeysekera broadcast; its listing " +
        "now clusters with venues whose 2015 relay the review page labels right, so the superseded-relay withdrawal (which " +
        "reads every listing of the cluster) keeps it — a cluster split, not fixed here"))

  private def take(row: IdentityUnifiedFit.Row): Take = Take(row.country, row.venue, row.rawTitle,
    row.film.stripPrefix("tmdb:").toIntOption.filter(_ => row.film.startsWith("tmdb:")),
    Option.when(row.film.startsWith("imdb:"))(row.film.stripPrefix("imdb:")), "", row.filmTitle)

  private lazy val wrong = rows.filter(_.today).map(take).filter(UnmatchedClusters.verdict(_, labels).contains(false))

  "the model's matched takes" should "take no film a hand label denies or contradicts, beyond those prod no longer serves" in {
    val standing = wrong.filterNot(t => noLongerServed.contains((t.country, t.rawTitle, t.film)))
    withClue(standing.map(UnmatchedClusters.takeLine).mkString("WRONG matched takes:\n", "\n", "\n")) { standing shouldBe empty }
  }

  it should "list only takes the rows still hold — one gone from them is struck" in {
    val stale = noLongerServed.keySet -- wrong.map(t => (t.country, t.rawTitle, t.film)).toSet
    withClue(stale.mkString("no longer in the rows — remove from `noLongerServed`:\n", "\n", "\n")) { stale shouldBe empty }
  }
}
