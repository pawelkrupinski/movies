package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** RosterAudit's one argument: the roster workflow's line reads, and a stray one stops it before the audit's reads. */
class RosterAuditArgumentsSpec extends AnyFlatSpec with Matchers {

  "RosterAudit" should "append to the step summary the workflow names, and refuse a flag or a second argument" in {
    val workflow = Files.readString(Paths.get(".github/workflows/roster-audit.yml"), StandardCharsets.UTF_8)
    """runMain tools\.RosterAudit ([^"]*)"""".r.findFirstMatchIn(workflow).map(_.group(1).trim) shouldBe Some("$GITHUB_STEP_SUMMARY")
    RosterAudit.reportPath(Seq("summary.md")) shouldBe Right(Some(Paths.get("summary.md")))
    RosterAudit.reportPath(Nil) shouldBe Right(None)
    // args.headOption read only the first and ignored the rest
    RosterAudit.reportPath(Seq("summary.md", "other.md")).isLeft shouldBe true
    RosterAudit.reportPath(Seq("--dry-run")).isLeft shouldBe true
  }
}
