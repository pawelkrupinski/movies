package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Each country's corpus is recorded by that country's RECORDING leg, as the first thing its job
 * does — not by five `scrapes (<country>)` jobs that every leg had to wait for.
 *
 * `needs:` joins jobs, not matrix rows, so the old shape started no leg until the slowest
 * country's capture was uploaded, and paid five more runners, checkouts and JDK/sbt setups to
 * hand tarballs over through artifacts. Folded into the leg, the capture reuses the leg's setup.
 *
 * The move must not widen what the prod-Mongo route is exposed to. The scrape job ran nothing
 * after the read but a tar and two uploads; the leg runs project code for hours with a
 * write-scoped token and the TMDB/OMDb keys. So the tunnel's key reaches only the step that
 * opens it, and the tunnel is closed — key deleted, credential blanked — straight after the read.
 */
class RecordCorpusInLegWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val legFile     = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val convergence = RepoFile.block(legFile, "convergence")
  private lazy val recorder    = RepoFile.read(".github/workflows/record-scrape-fixtures.yml")
  private lazy val setup       = RepoFile.read(".github/actions/convergence-setup/action.yml")

  private val Tunnel  = "Tunnel to prod Mongo"
  private val Record  = "Record ${{ inputs.country }}'s corpus"
  private val Close   = "Close the tunnel to prod Mongo"
  private val Pack    = "Pack ${{ inputs.country }}'s corpus"
  private val Suite   = "Run the ${{ inputs.country }} ${{ matrix.phase }} suite"
  private val Restore = "Restore the corpus the convergence row records"
  private val InRecordingRow = "inputs.mode == 'record' && matrix.phase == 'convergence'"

  private def at(marker: String): Int = RepoFile.positionOf(convergence, marker)

  "the recorder" should "have no scrape jobs ahead of its legs, which every leg would wait for" in {
    RepoFile.jobs(recorder).keySet shouldBe Set("preflight", "enrichment", "timings", "report")
    RepoFile.block(recorder, "enrichment") should include("needs: [ preflight ]\n")
  }

  // The corpus is recorded by the convergence row only: a second tunnel would race the first for
  // the same artifact name and cache key, and read a DIFFERENT corpus a moment later. A
  // recording's order row (the UK's, whose replays were ~6 of its 14 minutes behind a boot they
  // never touch — run 37105119296) replays the convergence row's own upload instead.
  it should "split only the UK's replays into a row of their own" in {
    RepoFile.block(recorder, "enrichment") should include("order-command:  ${{ matrix.order || '' }}")
    // `order: ''` is a row WITHOUT an order row, spelled out for Forgejo's strict matrix reads.
    val ordered = recorder.linesIterator.filter(l => l.contains("order: ") && !l.contains("order: ''")).toSeq
    ordered.map(_.contains("code: uk,")) shouldBe Seq(true)
    ordered.head should include("cmd: convergenceUkWithoutOrder, order: convergenceUkOrder,")
  }

  it should "hand a recording's order row the corpus its convergence row records, through the run's artifact" in {
    val restore = RepoFile.step(convergence, Restore)
    restore should include("if: inputs.mode == 'record' && matrix.phase != 'convergence'\n")
    restore should include("""scripts/ci/wait-for-run-artifact.sh "scrape-fixtures-${{ inputs.code }}" "convergence (${{ inputs.country }})"""")
    restore should include("""gh run download "$GITHUB_RUN_ID" --name "scrape-fixtures-${{ inputs.code }}" --dir scrape-archive""")
    withClue("restored before anything replays it: ")(at(s"- name: $Restore") should be < at(s"- name: $Suite"))
  }

  // Gap-filled: no boot of its own to have fetched what its passes ask, so what the restored tree lacks is fetched ONCE
  // between them (SharedLiveAnswers).
  it should "replay a recording's order row gap-filled" in {
    RepoFile.step(convergence, Suite) should include(
      "KINOWO_CONVERGENCE_FILL_ONLY: ${{ inputs.mode == 'record' && matrix.phase != 'convergence' }}")
  }

  "a recording leg" should "record, close the tunnel and upload its corpus before the suite replays it" in {
    val order = Seq("uses: ./.github/actions/convergence-setup", s"- name: $Tunnel", s"- name: $Record",
      s"- name: $Close", s"- name: $Pack", "uses: actions/upload-artifact@", s"- name: $Suite")
    order.map(at) shouldBe order.map(at).sorted
  }

  // e2e depends on worker's fixtures, never its specs, so no leg's build cache holds worker's
  // test-classes current: recording from `worker/Test` recompiled up to all 744 of them first.
  it should "record from the Fixtures configuration, which every leg's build cache holds compiled" in {
    RepoFile.step(convergence, Record) should include("""sbt -J-Xmx${{ inputs.heap }} "${jvm[@]}" "worker/Fixtures/runMain scripts.RecordCorpusFixture ${{ inputs.code }}"""")
    RepoFile.exists("worker/src/fixtures/scala/scripts/RecordCorpusFixture.scala") shouldBe true
    RepoFile.exists("worker/src/test/scala/scripts/RecordCorpusFixture.scala") shouldBe false
  }

  // No recording runs the ~100-film sample (2026-10-14): a recording never requests a bisect, the one thing that still
  // replays it. Every row runs the sample's corpus-shape specs ahead of its suite instead, reporting what they find.
  it should "run no sample, but every country's corpus-shape specs" in {
    val rows = RepoFile.matrixRows(RepoFile.block(recorder, "enrichment"))
    rows.foreach { row =>
      withClue(s"${row("country")}: ") {
        row.keySet should (contain("shape") and not contain "sample" and not contain "sampleRow")
      }
    }
    RepoFile.block(recorder, "enrichment") should include("shape-command:  ${{ matrix.shape }}")
  }

  it should "run the corpus steps in a recording's convergence row only, and close the tunnel whatever happened" in {
    Seq(Tunnel, Record, Pack).foreach { name =>
      withClue(s"$name: ")(RepoFile.step(convergence, name) should include(s"if: $InRecordingRow\n"))
    }
    RepoFile.step(convergence, Close) should include(s"if: always() && $InRecordingRow\n")
    RepoFile.step(convergence, Close) should include("run: scripts/ci/close-mongo-tunnel.sh 27018")
    withClue("a failed read must stop the leg, not record a tree over yesterday's corpus: ") {
      RepoFile.step(convergence, Record) should not include "continue-on-error"
    }
  }

  // The scrape job's posture: the ssh key in the tunnel step's env and nowhere else.
  it should "hand the tunnel key to the step that opens the tunnel and to no other" in {
    val tunnel = RepoFile.step(convergence, Tunnel)
    val keyRefs = """secrets\.MONGO_CI_SSH_\w+""".r
    keyRefs.findAllIn(tunnel).toSet shouldBe Set("secrets.MONGO_CI_SSH_KEY", "secrets.MONGO_CI_SSH_HOST_KEY")
    keyRefs.findAllIn(legFile).size shouldBe keyRefs.findAllIn(tunnel).size
    """secrets\.\w+""".r.findAllIn(tunnel).toSet shouldBe Set("secrets.MONGO_CI_SSH_KEY", "secrets.MONGO_CI_SSH_HOST_KEY")
    withClue("the recorder maps the key for its preflight only: ") {
      keyRefs.findAllIn(RepoFile.jobs(recorder).removed("preflight").values.mkString("\n")) shouldBe empty
    }
  }

  // The leg's own throwaway Mongo holds 27017; the tunnel must stay off it.
  it should "open and close the tunnel on 27018, clear of the leg's own database on 27017" in {
    RepoFile.step(convergence, Tunnel) should include("run: scripts/ci/wait-for-mongo-tunnel.sh 27018")
    RepoFile.step(convergence, Suite) should include("MONGODB_URI: mongodb://127.0.0.1:27017/")
  }

  // Hermetic legs, the provenance diff, identity-decorations and hard-clusters.sh all read these.
  it should "publish the corpus under the names its readers already read" in {
    RepoFile.step(convergence, Pack) should include("tar -czf \"scrape-archive/scrapes-${{ inputs.code }}.tar.gz\"")
    Seq("cinema-scrapes-${{ inputs.code }}.json.gz", "prod-coverage-${{ inputs.code }}.json", "prod-films-${{ inputs.code }}.json.gz",
        "cinema-scrapes-${{ inputs.code }}-sample.json.gz", "prod-coverage-${{ inputs.code }}-sample.json",
        "prod-films-${{ inputs.code }}-sample.json.gz").foreach { file =>
      RepoFile.step(convergence, Pack) should include(s"\"test/resources/fixtures/corpus/$file\"")
    }
    // The artifact is the one copy readers read (hermetic legs, the provenance diff, restore-country-corpus.sh);
    // a cache entry beside it had no reader and was evicted within hours anyway.
    convergence should not include("key: scrape-fixtures-")
    convergence should include("name: scrape-fixtures-${{ inputs.code }}\n                  path: scrape-archive/scrapes-${{ inputs.code }}.tar.gz")
    convergence should include("retention-days: 14")
  }

  it should "not try to download a corpus from its own run, which no job uploads before it" in {
    setup should not include "uses: actions/download-artifact"
  }

  /** Matrix rows are queued in the order they are listed, so with runners short the LAST row waits
   *  longest; the US, the longest leg and so the critical path, waited 1.7 min as the last row
   *  (run 37111868620). */
  "the recorder's matrix" should "list the longest leg, the United States, first" in {
    val countries = """(?m)^\s+- \{ country: ([a-z-]+),""".r.findAllMatchIn(recorder).map(_.group(1)).toList
    countries should not be empty
    countries.head shouldBe "united-states"
  }

  /** Recordings land every ~15 min; keeping only the newest two pinned trees per country evicted a
   *  pair before an identity measure dispatched on it could restore it (measure 37114004953). */
  "the publish" should "keep each country's newest five pinned trees" in {
    RepoFile.read(".github/scripts/convergence-publish.sh") should include("| sort -r | tail -n +6 | cut -f2")
    ScriptTests.runInCi(".github/scripts/convergence-publish-test.sh") shouldBe true
    RepoFile.read(".github/scripts/restore-enrichment-tree.sh") should include("(it keeps the newest five)")
  }
}
