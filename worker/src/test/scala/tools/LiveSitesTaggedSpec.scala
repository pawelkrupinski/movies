package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.SourceTrees

/**
 * An integration spec that talks to a LIVE third-party site carries `@LiveSites` (worker/src/it/java/integration/
 * LiveSites.java), or it runs in `itAll`: Main's integration rows, the pre-push `itrest` and the landing check, where
 * a site that changes its answer blocks every unrelated landing (Metacritic answered 200 for any slug, 2026-11-25).
 * build.sbt's `it` config excludes the tag from those runs; only the nightly live-sites.yml selects it.
 *
 * A spec REACHES a live site when it names `LiveUpstream.Probes`: the stable URL it asks when a case fails, which
 * every live spec needs for `LiveUpstream.orCancel` to tell a site that is down from a contract that changed.
 */
class LiveSitesTaggedSpec extends AnyFlatSpec with Matchers {
  import LiveSitesTaggedSpec._

  "the matcher" should "ask the tag of a spec naming a live probe, and of no other" in {
    untagged("""class MovieSitesIntegrationSpec extends AnyFlatSpec { probe(LiveUpstream.Probes.Metacritic) }""") shouldBe true
    untagged("""@LiveSites
                |class FilmwebIntegrationSpec extends AnyFlatSpec { probe(LiveUpstream.Probes.Filmweb) }""".stripMargin) shouldBe false
    untagged("""@LiveSites class A { LiveUpstream.Probes.Imdb }""") shouldBe false
    untagged("""// @LiveSites
               |class A { LiveUpstream.Probes.Imdb }""".stripMargin) shouldBe true
    untagged("""class LiveUpstreamSpec { LiveUpstream.orCancel("Test upstream", fake, clock) }""") shouldBe false
  }

  "integration specs" should "carry @LiveSites when they reach a live site" in {
    val files = ScalaSourceScan.scalaFiles(ItTrees)
    files.size should be > 20
    val offenders = files.filter(p => untagged(ScalaSourceScan.read(p))).map(_.toString)
    withClue("Tag the spec `@LiveSites` (it then leaves itAll for the nightly live-sites.yml), or stop it reaching the site:\n" +
      offenders.mkString("\n") + "\n")(offenders shouldBe empty)
  }
}

object LiveSitesTaggedSpec {
  /** Every integration tree: what `itAll` selects its specs from. */
  val ItTrees: SourceTrees = SourceTrees("web/src/it", "worker/src/it")

  private val LiveProbe = """\bLiveUpstream\.Probes\b""".r
  private val Tagged    = """@LiveSites\b(?s:.*?)\bclass\b""".r

  /** `source`, its comments dropped, names a live probe but no `@LiveSites` on a class. */
  def untagged(source: String): Boolean = {
    val code = source.linesIterator.map(ScalaSourceScan.code).mkString("\n")
    LiveProbe.findFirstIn(code).isDefined && Tagged.findFirstIn(code).isEmpty
  }
}
