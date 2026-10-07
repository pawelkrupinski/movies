package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every hermetic convergence leg publishes what its tree lacked, and the next run's rows fetch it
 * before their suites: the suite's refetch list goes up beside the pinned pair, each row of the next
 * leg fetches it for ~90 s and lays what it got over its own tree, and publishes that as a fill every
 * later leg replays (docs/design/convergence-fixture-fill.md). A longer fill is the hand-dispatched
 * `Convergence fill`. Each rule below is one way that loop silently stops, or starts costing a verdict
 * its hermeticity or its bisect's reproducibility.
 */
class ConvergenceFillWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val leg     = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val setup   = RepoFile.read(".github/actions/convergence-setup/action.yml")
  private lazy val publish = RepoFile.read(".github/actions/convergence-publish/action.yml")
  private lazy val lane    = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private lazy val fill    = RepoFile.read(".github/actions/convergence-fill/action.yml")
  private lazy val manual  = RepoFile.read(".github/workflows/convergence-fill.yml")
  private lazy val rows    = RepoFile.jobs(leg)("convergence")

  private val FillStep  = "Fill the gaps the last leg listed, before the suite"
  private val SuiteStep = "Run the ${{ inputs.country }} ${{ matrix.phase }} suite"
  private val Sample    = "Run the ${{ inputs.country }} sample ahead of the suite"
  private val MainOnly  = "github.ref == 'refs/heads/main'"

  private lazy val fillStep = RepoFile.step(leg, FillStep)

  "a hermetic convergence row" should "publish the gaps its tree could not answer, for the next leg's rows" in {
    rows should include(
      "refetch-list: ${{ inputs.mode == 'hermetic' && format('test/resources/fixtures/enrichment-{0}.refetch.tsv', inputs.code) || '' }}")
    // ...the file the suite writes it to.
    tools.MissingFixtures.refetchListBeside(java.nio.file.Paths.get("test/resources/fixtures/enrichment-us")).toString shouldBe
      "test/resources/fixtures/enrichment-us.refetch.tsv"
  }

  // The fill before the suite, in EVERY hermetic row: what the previous run listed is replayed by this run's
  // suites, not the run after next.
  "every hermetic row" should "fetch the previous leg's gaps after setup and before its sample and suite" in {
    fillStep should include("if: inputs.mode == 'hermetic'\n")
    withClue("every phase, not only the convergence row: ")(fillStep should not include "matrix.phase")
    fillStep should include("convergence-fill.sh gaps")
    fillStep should include("scripts.FillMissingFixtures")
    val at = RepoFile.positionOf(rows, s"- name: $FillStep")
    RepoFile.positionOf(rows, "uses: ./.github/actions/convergence-setup") should be < at
    at should be < RepoFile.positionOf(rows, s"- name: $Sample")
    at should be < RepoFile.positionOf(rows, s"- name: $SuiteStep")
    // ...and before a sample row stamps its tree, so a fill is never taken for the sample's recordings.
    at should be < RepoFile.positionOf(rows, "- name: Mark the tree before the sample records into it")
  }

  it should "stop starting requests within 90 seconds of the step's start, and never fail the row" in {
    val seconds = """FILL_SECONDS:\s*(\d+)""".r.findFirstMatchIn(fillStep).map(_.group(1).toInt)
    seconds.getOrElse(fail("the fill step names no FILL_SECONDS")) should be <= 90
    fillStep should include("until=$(( $(date +%s) + FILL_SECONDS ))")
    fillStep should include("$until\"")
    fillStep should include("continue-on-error: true")
    """timeout-minutes:\s*(\d+)""".r.findFirstMatchIn(fillStep).map(_.group(1).toInt).getOrElse(99) should be <= 5
  }

  it should "skip what its restored tree holds, and lay what it fetched over that tree for its suite" in {
    fillStep should include("""fixtures="$GITHUB_WORKSPACE/test/resources/fixtures"""")
    fillStep should include("$list $fixtures $out/test/resources/fixtures $until")
    fillStep should include("""cp -a "$out/test/resources/fixtures/." "$fixtures/"""")
    fillStep should include("convergence-fill.sh pack \"$out\" fill-upload-source/fill.tar.zst")
  }

  // The suite is still hermetic: no fetching step after the fill, and nothing of the fill in the suite.
  it should "keep the suite itself off the network" in {
    RepoFile.step(leg, SuiteStep) should include("KINOWO_CONVERGENCE_HERMETIC: ${{ inputs.mode == 'hermetic' }}")
    RepoFile.withoutComments(leg).linesIterator.count(_.contains("scripts.FillMissingFixtures")) shouldBe 1
  }

  // A TMDB gap is listed with its key masked; the fill signs it again (`FillCredentials`) — the key in the step
  // that fetches, never in the ones that pack or publish what it fetched.
  it should "hold TMDB's key in the fetching step, and not in the publishes of what it fetched" in {
    fillStep should include("TMDB_API_KEY:     ${{ secrets.TMDB_API_KEY }}")
    publish should not include "TMDB_API_KEY"
    rows.linesIterator.dropWhile(!_.contains("id: publish")).take(25).mkString("\n") should not include "TMDB"
  }

  // A request the origin refuses CI's own address (Cineworld's 403s) is asked again through the residential proxy
  // (`MissingFixtureFill.route`): its credentials, like TMDB's key, in the fetching step alone.
  // The fill runs inside sbt's JVM, which reads the tunnel-auth policy once: set at the JVM's start, so nothing sbt
  // touched first can have read the JDK's default, which answers every proxied request 407 (run 37677032787).
  it should "start the fill's JVM with Basic allowed on the proxy's tunnels" in {
    val option = "-J" + tools.ProxyTunnelAuthentication.BasicAllowed.jvmOption
    fillStep should include(s"$option \"worker/Fixtures/runMain scripts.FillMissingFixtures")
    RepoFile.step(fill, "Fetch them until the fill's minutes run out") should include(s"$option \"worker/Fixtures/runMain")
  }

  it should "hold the residential proxy's credentials in the fetching step alone" in {
    Seq("KINOWO_PROXY_USER", "KINOWO_PROXY_PASS").foreach { name =>
      fillStep should include(s"$name: $${{ secrets.$name }}")
      publish should not include name
      RepoFile.withoutComments(leg).linesIterator.count(_.contains(s"secrets.$name")) shouldBe 1
    }
  }

  it should "publish what it fetched under a name of its own row, from every phase" in {
    val others = rows.substring(RepoFile.positionOf(rows, "id: publish-row-fill"))
    rows.substring(RepoFile.positionOf(rows, "id: publish\n"), RepoFile.positionOf(rows, "id: publish-row-fill")) should
      include("fill-row:     ${{ matrix.phase }}")
    others should include("if: always() && inputs.mode == 'hermetic' && matrix.phase != 'convergence'")
    others should include("fill-archive: fill-upload-source/fill.tar.zst")
    others should include("fill-row:     ${{ matrix.phase }}")
    val named = RepoFile.step(publish, "Publish what this leg's fill fetched")
    named should include("$GITHUB_RUN_ID${ROW:+-$ROW}.tar.zst")
    named should include("""echo "asset=$named" >> "$GITHUB_OUTPUT"""")
  }

  // A bisect replays the pair the request names: without the row's own fill it would replay a tree the
  // verdict was never decided on.
  it should "carry its own fill in the pair its bisect request replays" in {
    rows should include(
      "pair:           ${{ format('{0} {1}{2}', steps.setup.outputs.hermetic-pair, steps.publish.outputs.fill-asset, steps.publish-row-fill.outputs.fill-asset) }}")
    publish should include("value: ${{ steps.fill.outputs.asset }}")
    RepoFile.positionOf(rows, "id: publish-row-fill") should be < RepoFile.positionOf(rows, "uses: ./.github/actions/convergence-bisect-request")
    RepoFile.read(".github/actions/convergence-bisect-request/action.yml") should include("""--arg pair "${words[*]}"""")
  }

  "the identity lane" should "run no fill job of its own any more" in {
    RepoFile.jobs(lane).keySet should not contain "fill"
    RepoFile.withoutComments(lane) should not include "convergence-fill"
  }

  "the manual fill" should "be dispatched by hand only, per country, through the fill action" in {
    val on = RepoFile.block(manual, "on")
    on should include("workflow_dispatch:")
    Seq("push:", "schedule:", "workflow_run:", "workflow_call:", "pull_request").foreach(t => on should not include t)
    on should include("default: pl,de,uk,es,us")
    """(?s)minutes:.*?default:\s*(\d+)""".r.findFirstMatchIn(on).map(_.group(1).toInt) shouldBe Some(7)
    val job = RepoFile.jobs(manual)("fill")
    job should include("code: ${{ fromJson(needs.countries.outputs.codes) }}")
    job should include("uses: ./.github/actions/convergence-fill")
    job should include("tmdb-api-key: ${{ secrets.TMDB_API_KEY }}")
    job should include("proxy-user: ${{ secrets.KINOWO_PROXY_USER }}")
    job should include("proxy-pass: ${{ secrets.KINOWO_PROXY_PASS }}")
    withClue("its own run id names its fill, so it never writes a row's name: ")(fill should not include "fill-row:")
  }

  "the fill action" should "stop starting requests at its minute, with TMDB's key in the fetching step alone" in {
    fill should include("""echo "FILL_UNTIL=$(( $(date +%s) + ${{ inputs.minutes }} * 60 ))" >> "$GITHUB_ENV"""")
    RepoFile.step(fill, "Fetch them until the fill's minutes run out") should include("$FILL_UNTIL")
    RepoFile.step(fill, "Fetch them until the fill's minutes run out") should include("TMDB_API_KEY:     ${{ inputs.tmdb-api-key }}")
    fill.linesIterator.count(_.contains("inputs.tmdb-api-key")) shouldBe 1
    RepoFile.step(fill, "Fetch them until the fill's minutes run out") should include("KINOWO_PROXY_USER: ${{ inputs.proxy-user }}")
    RepoFile.step(fill, "Fetch them until the fill's minutes run out") should include("KINOWO_PROXY_PASS: ${{ inputs.proxy-pass }}")
    Seq("inputs.proxy-user", "inputs.proxy-pass").foreach(i => fill.linesIterator.count(_.contains(i)) shouldBe 1)
  }

  it should "replay nothing and compile only the fixtures, without becoming the build cache's entry" in {
    fill should include("replay: 'false'")
    setup should include("restore-only: ${{ inputs.replay != 'true' }}")
    Seq("Start MongoDB (single-node replica set, for change streams)", "Fetch the enrichment tree in the background",
        "Restore the recorded scrape corpus", "Unpack whichever fixtures are present", "Wait for MongoDB, started in the background above")
      .foreach(name => withClue(s"$name: ")(RepoFile.step(setup, name) should include("if: inputs.replay == 'true'")))
  }

  it should "publish what it fetched through the gated action, as a fill of the pair" in {
    fill should include("uses: ./.github/actions/convergence-publish")
    fill should include("fill-archive: fill-upload-source/fill.tar.zst")
  }

  // A `--clobber` deletes the asset before re-uploading it: a leg restoring in that moment finds nothing.
  "a fill and a refetch list" should "go up from main only, under a name nobody else writes" in {
    Seq("Publish the gaps this leg's tree could not answer", "Publish what this leg's fill fetched").foreach { name =>
      val step = RepoFile.step(publish, name)
      withClue(s"$name: ") {
        step should include(MainOnly)
        step should include("inputs.mode == 'hermetic'")
        step should include("$KINOWO_CONVERGENCE_PIN_CORPUS_RUN-$GITHUB_RUN_ID")
        RepoFile.withoutComments(step) should not include "--clobber"
        withClue("a publish that fails must not turn the verdict red: ")(RepoFile.withoutComments(step) should not include "|| exit 1")
      }
    }
  }

  "a hermetic leg's pair" should "name its fills once, at setup, and carry them to a bisect" in {
    val resolve = RepoFile.step(setup, "Resolve the recorded pair to replay")
    resolve should include("read -r corpus recorded tree fills <<< \"$pair\"")
    resolve should include("convergence-fill.sh fills")
    resolve should include("""pair="$corpus $recorded $tree${fills:+ $fills}"""")
    resolve should include("KINOWO_CONVERGENCE_FILL_ASSETS=")
    RepoFile.read(".github/scripts/restore-enrichment-tree.sh") should include("""convergence-fill.sh" unpack "$stage" "${fills[@]}"""")
  }

  "a new pin" should "prune the fills — a row's included — and lists of the pairs it prunes" in {
    RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay") should include(
      "^(fill|refetch)-$code-[0-9]+-[0-9]+(-[a-z][a-z-]*)?\\\\.(tar\\\\.zst|tsv)$")
  }

  "the fill's release script" should "be run by CI's shell specs" in {
    RepoFile.read(".github/workflows/ci.yml") should include("bash .github/scripts/convergence-fill-test.sh")
  }
}
