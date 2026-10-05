package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Where CI may hand out the Zyte key: nowhere.
 *
 * Zyte is billed per request, and since 2026-10-05 only the worker uses it — for the one
 * venue the Decodo proxy cannot reach (Kino Kryterium) and Odeon's token harvest. No CI
 * tool builds a Zyte leg any more (the recorder and the diagnostics go proxy → direct), and
 * a test run answers from fixtures, so a `ZYTE_API_KEY` in a workflow could only be spent
 * by accident. The unit, integration, e2e and order-independence runs used to be handed it
 * "for parity", and the recorder and FilmwebDiff behind the proxy.
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

  "no CI workflow" should "be handed the Zyte key" in {
    val files = RepoFile.ciFiles()
    withClue("the sweep must see the workflows, or it would pass over nothing: ") {
      files should contain allOf(".github/workflows/country-fixture-artifact.yml", ".github/workflows/filmweb-diff.yml")
    }
    files.foreach(path => withClue(s"$path: ")(zyteEnvBlocks(RepoFile.read(path)) shouldBe empty))
  }

  "the fixture recorder's script" should "require the proxy credentials, not the Zyte key" in {
    val script = RepoFile.read(".github/scripts/record-country-fixture.sh")
    script should include("""missing="$missing KINOWO_PROXY_USER"""")
    script should include("""missing="$missing KINOWO_PROXY_PASS"""")
    script should not include "ZYTE_API_KEY"
  }
}
