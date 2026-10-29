package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A recording asks OMDb and TMDB whether they still take its keys before any leg starts.
 *
 * A refused OMDb key (spent daily quota or a rotated key) ended 5 of the 14 recordings that failed or were cancelled
 * in the week to 2026-10-26, each 22-125 min in, after all five legs had booted and replayed. One request at t=0 sees
 * a bad or rotated key and fails (scripts/ci/recording-preflight.sh, tested by recording-preflight-test.sh). OMDb's
 * spent daily quota only warns: the capture records without OMDb, its best-effort origin, and failing on it made the
 * nightly #2426 red with nothing recorded.
 */
class RecordingPreflightWiringSpec extends AnyFlatSpec with Matchers {

  private lazy val workflow = RepoFile.read(".github/workflows/record-scrape-fixtures.yml")
  private lazy val jobs     = RepoFile.jobs(workflow)

  "the recording's preflight job" should "probe both keys with the recording preflight" in {
    val preflight = RepoFile.withoutComments(jobs("preflight"))
    preflight should include("run: scripts/ci/alert-step.sh 1 -- scripts/ci/recording-preflight.sh")
    preflight should include("OMDB_API_KEY: ${{ secrets.OMDB_API_KEY }}")
    preflight should include("TMDB_API_KEY: ${{ secrets.TMDB_API_KEY }}")
  }

  it should "check out the script it runs" in {
    RepoFile.withoutComments(jobs("preflight")) should include("sparse-checkout: scripts/ci")
  }

  "every recording leg" should "wait for the preflight" in {
    RepoFile.withoutComments(jobs("enrichment")) should include("needs: [ preflight ]")
  }
}
