package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentityUnifiedFit
import tools.{IdentityJudgements, UnmatchedClusters}
import tools.UnmatchedClusters.Take

/**
 * The model's MATCHED takes graded against the hand labels — the clusters [[UnmatchedClustersRatchetSpec]] never sees:
 * its fixture captures only the clusters the model leaves unmatched, so a wrong film the model takes outright is
 * invisible to it (2026-10-06: four such takes found only by an offline ML audit of the unified rows).
 *
 * Every cluster of the unified evidence rows ([[IdentityUnifiedFit.Training]]: every cluster of the five recorded
 * corpora, matched ones included) whose take the model makes is judged by the identity judgements as the ratchet judges
 * a take, by the cluster's venue and most common raw title. The rows hold the MODEL's take, before the agreement stage
 * reads it again (`agreement.Correction`), and as the code stood when they were emitted
 * (`integration.IdentityUnifiedDataset`). So a wrong take fails unless the judgements hold it known-wrong in scope `rows`
 * — one a later stage or a later change no longer serves ("no longer served: …", with the spec that proves it), or one
 * no safe fix removes yet — each with why. They only shrink: a row the rows no longer hold is struck.
 */
class MatchedTakesLabelSpec extends AnyFlatSpec with Matchers {

  private val judged = IdentityJudgements.read()
  private val labels = IdentityJudgements.labels(judged)
  private val rows   = IdentityUnifiedFit.read(IdentityUnifiedFit.Training)

  private def take(row: IdentityUnifiedFit.Row): Take = Take(row.country, row.venue, row.rawTitle,
    row.film.stripPrefix("tmdb:").toIntOption.filter(_ => row.film.startsWith("tmdb:")),
    Option.when(row.film.startsWith("imdb:"))(row.film.stripPrefix("imdb:")), "", row.filmTitle)

  private val listed = IdentityJudgements.tolerated(judged, IdentityJudgements.Scope.Rows).keySet
  private def key(take: Take) = (take.country, IdentityJudgements.normalise(take.rawTitle), take.film)

  private lazy val wrong = rows.filter(_.today).map(take).filter(UnmatchedClusters.verdict(_, labels).contains(false))

  "the model's matched takes" should "take no film a judgement denies or contradicts, beyond those known-wrong in scope rows" in {
    val standing = wrong.filterNot(t => listed.contains(key(t)))
    withClue(standing.map(UnmatchedClusters.takeLine).mkString("WRONG matched takes:\n", "\n", "\n")) { standing shouldBe empty }
  }

  it should "list only takes the rows still hold — one gone from them is struck" in {
    val stale = listed -- wrong.map(key).toSet
    withClue(stale.mkString("no longer in the rows — drop `rows` from the known-wrong judgement's scope:\n", "\n", "\n")) { stale shouldBe empty }
  }
}
