package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Where CI may hand out the Zyte key.
 *
 * Zyte is billed per request and is the Decodo residential proxy's FALLBACK, never a leg of
 * its own (the `feedback_zyte_is_decodo_fallback_only` rule). A test run answers from fixtures
 * and must not reach it at all; the one recorder that may (`RecordAllDataToFixture`) builds a
 * Zyte leg only behind the proxy, so its step must carry the proxy credentials beside the key.
 * The unit, integration, e2e and order-independence runs used to be handed `ZYTE_API_KEY`
 * "for parity" — every one of them a hermetic run with no use for it but to spend it.
 */
class PaidEgressWorkflowSpec extends AnyFlatSpec with Matchers {

  private val ZyteKey = "ZYTE_API_KEY:"

  /** Every `ZYTE_API_KEY:` directive in `yaml` with the env mapping it sits in: the keys at its
   *  own indentation, contiguous with it, comments and blank lines skipped. */
  private def zyteEnvBlocks(yaml: String): Seq[Seq[String]] = {
    val lines  = yaml.linesIterator.toVector
    def indent(line: String) = line.takeWhile(_ == ' ').length
    def ignorable(line: String) = line.trim.isEmpty || line.trim.startsWith("#")
    lines.indices.filter(i => lines(i).trim.startsWith(ZyteKey)).map { at =>
      val level = indent(lines(at))
      def sibling(line: String) = ignorable(line) || indent(line) >= level
      val before = lines.take(at).reverse.takeWhile(sibling)
      val after  = lines.drop(at + 1).takeWhile(sibling)
      (before.reverse ++ (lines(at) +: after)).filterNot(ignorable).filter(indent(_) == level).map(_.trim)
    }
  }

  private lazy val zyteSites: Seq[(String, Seq[String])] =
    RepoFile.ciFiles().flatMap(path => zyteEnvBlocks(RepoFile.read(path)).map(path -> _))

  "every CI step handed the Zyte key" should "carry the residential-proxy credentials beside it" in {
    withClue("the sweep must see the recorder's key, or it would pass over nothing: ") {
      zyteSites.map(_._1) should contain(".github/workflows/country-fixture-artifact.yml")
    }
    zyteSites.foreach { case (path, env) =>
      withClue(s"$path hands out ZYTE_API_KEY without Decodo ahead of it (env: ${env.mkString(", ")}): ") {
        env.exists(_.startsWith("KINOWO_PROXY_USER:")) shouldBe true
        env.exists(_.startsWith("KINOWO_PROXY_PASS:")) shouldBe true
      }
    }
  }

  "a test run" should "never be handed the Zyte key" in {
    Seq(".github/workflows/ci.yml", ".github/workflows/order-independence.yml", ".github/workflows/filmweb-diff.yml")
      .foreach(path => withClue(s"$path: ")(zyteEnvBlocks(RepoFile.read(path)) shouldBe empty))
  }

  "the fixture recorder's script" should "require the proxy credentials, not the Zyte key" in {
    val script = RepoFile.read(".github/scripts/record-country-fixture.sh")
    script should include("""missing="$missing KINOWO_PROXY_USER"""")
    script should include("""missing="$missing KINOWO_PROXY_PASS"""")
    script should not include """missing="$missing ZYTE_API_KEY""""
  }
}
