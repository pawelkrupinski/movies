package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.roster.AuditedVenue
import services.cinemas.roster.RosterFinding.{DirectoryNotRead, LowCoverage, SourceGone}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** RosterAudit's one argument: the roster workflow's line reads, and a stray one stops it before the audit's reads;
 *  and its exit code, which scripts/ci/roster-audit.sh reads. */
class RosterAuditArgumentsSpec extends AnyFlatSpec with Matchers {

  "RosterAudit" should "append to the step summary the workflow names, and refuse a flag or a second argument" in {
    val workflow = Files.readString(Paths.get(".github/workflows/roster-audit.yml"), StandardCharsets.UTF_8)
    val script   = Files.readString(Paths.get("scripts/ci/roster-audit.sh"), StandardCharsets.UTF_8)
    """scripts/ci/roster-audit\.sh ([^\n]*)""".r.findFirstMatchIn(workflow).map(_.group(1).trim) shouldBe Some("\"$GITHUB_STEP_SUMMARY\"")
    """runMain tools\.RosterAudit ([^"]*)"""".r.findFirstMatchIn(script).map(_.group(1).trim) shouldBe Some("$report")
    RosterAudit.reportPath(Seq("summary.md")) shouldBe Right(Some(Paths.get("summary.md")))
    RosterAudit.reportPath(Nil) shouldBe Right(None)
    // args.headOption read only the first and ignored the rest
    RosterAudit.reportPath(Seq("summary.md", "other.md")).isLeft shouldBe true
    RosterAudit.reportPath(Seq("--dry-run")).isLeft shouldBe true
  }

  it should "exit DriftFound on data drift alone, and red only when it could not judge" in {
    val gone = SourceGone(AuditedVenue("konin", "Kino nad Wartą", "https://example.org/venue", Seq("Konin")), "404")
    RosterAudit.verdict(Nil) shouldBe 0
    RosterAudit.verdict(Seq(DirectoryNotRead("Multikino", 30, "403"))) shouldBe 0 // read from a datacenter alone: a note
    RosterAudit.verdict(Seq(gone)) shouldBe RosterAudit.DriftFound
    RosterAudit.verdict(Seq(gone, DirectoryNotRead("Multikino", 30, "403, through the proxy too", failing = true))) shouldBe 1
    RosterAudit.verdict(Seq(gone, LowCoverage(40, 50))) shouldBe 1
    val script = Files.readString(Paths.get("scripts/ci/roster-audit.sh"), StandardCharsets.UTF_8)
    script should include(s"\n  ${RosterAudit.DriftFound})\n")
  }
}
