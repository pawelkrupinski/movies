package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * NO SHARED FILE THAT EVERY CHANGE EDITS. Parallel agents kept conflicting on the same few files (2026-10-13..27, git log):
 * the experiments log (52 commits in two weeks, each adding a section at its top), the no-drift audit table (a row per
 * piece of state, plus a counts table every row change had to edit too -- it said "the 27" over 31 items), and lists
 * where each change appends one line at the end (the convergence paths, the drift allow-list).
 *
 *  - an experiment is one file, `docs/identity-experiments/<YYYY-MM-DD>-<slug>.md`, starting with its `# ` title; the old
 *    page keeps only the introduction and grows no dated section again;
 *  - a piece of state the incremental path carries is one file, `docs/design/identity-no-drift/state/<NN>-<slug>.md`
 *    (`<NN><letter>` for an item between two), titled `# <NN>. <state>`, with its Where and Verdict lines; the design
 *    page holds no audit row again;
 *  - an append-only list merges as a union (.gitattributes), so two additions at its end never conflict.
 */
class SharedFileLayoutSpec extends AnyFlatSpec with Matchers {

  import SharedFileLayoutSpec.*

  "the experiments log" should "be one file per entry, the old page holding none" in {
    datedSections(RepoFile.read(ExperimentsPage)) shouldBe empty
    val entries = RepoFile.listed(ExperimentsDir)(_.getName.endsWith(".md"))
    entries.size should be > 40
    entries.flatMap(f => experimentProblems(f.getName, RepoFile.read(f.getPath))) shouldBe empty
  }

  "the no-drift audit" should "be one file per item, the design page holding no row" in {
    auditRows(RepoFile.read(NoDriftPage)) shouldBe empty
    val items = RepoFile.listed(NoDriftStateDir)(_.getName.endsWith(".md"))
    items.size should be > 25
    items.flatMap(f => stateProblems(f.getName, RepoFile.read(f.getPath))) shouldBe empty
    items.map(_.getName.takeWhile(_ != '-')).groupBy(identity).collect { case (id, n) if n.size > 1 => id } shouldBe empty
  }

  "an append-only list" should "merge as a union" in {
    val attributes = RepoFile.withoutComments(RepoFile.read(".gitattributes")).linesIterator.map(_.trim).toSet
    UnionLists.filterNot(path => attributes(s"$path merge=union")) shouldBe empty
    UnionLists.filterNot(RepoFile.exists) shouldBe empty
  }

  "the rules" should "go red on a dated section, a misnamed entry or an audit row" in {
    datedSections("# Intro\n\n## Measuring-tool caveat\n\n## 2026-10-28 — a new experiment\n") shouldBe
      Seq("## 2026-10-28 — a new experiment")
    experimentProblems("new-experiment.md", "# x\n") should not be empty
    experimentProblems("2026-10-28-a-thing.md", "no title\n") should not be empty
    experimentProblems("2026-10-28-a-thing.md", "# 2026-10-28 — a thing\n") shouldBe empty
    auditRows("| # | State |\n|---|---|\n| 30 | `x` | `Y` | (a) | why |\n") shouldBe Seq("| 30 | `x` | `Y` | (a) | why |")
    stateProblems("30-x.md", "# 31. x\n\n- **Where:** y\n- **Verdict:** (a)\n") should not be empty
    stateProblems("30-x.md", "# 30. x\n\n- **Where:** y\n") should not be empty
    stateProblems("30b-x.md", "# 30b. x\n\n- **Where:** y\n- **Verdict:** (a)\n") shouldBe empty
  }
}

object SharedFileLayoutSpec {
  val ExperimentsPage = "docs/identity-experiments.md"
  val ExperimentsDir  = "docs/identity-experiments"
  val NoDriftPage     = "docs/design/identity-no-drift.md"
  val NoDriftStateDir = "docs/design/identity-no-drift/state"

  /** Files whose changes only add independent lines: kept merge=union. */
  val UnionLists: Seq[String] = Seq(
    "test/resources/fixtures/identity-judgements.tsv",
    "test/resources/fixtures/identity-unmatched/expected-matches.tsv",
    ".github/convergence-paths.txt",
    "test/resources/fixtures/identity-drift-allow.tsv",
  )

  private val EntryName = """\d{4}-\d{2}-\d{2}-[a-z0-9-]+\.md""".r
  private val ItemName  = """(\d{2}[a-z]?)-[a-z0-9-]+\.md""".r

  def datedSections(page: String): Seq[String] =
    page.linesIterator.filter(_.matches("""##\s+\d{4}-\d{2}-\d{2}.*""")).toSeq

  def experimentProblems(name: String, text: String): Seq[String] =
    Option.when(!EntryName.matches(name))(s"$name: not <YYYY-MM-DD>-<slug>.md").toSeq ++
      Option.when(!text.startsWith("# "))(s"$name: does not start with a `# ` title").toSeq

  def auditRows(page: String): Seq[String] = page.linesIterator.filter(_.matches("""\|\s*\d+[a-z]?\s*\|.*""")).toSeq

  def stateProblems(name: String, text: String): Seq[String] = name match {
    case ItemName(id) =>
      val id0 = id.dropWhile(_ == '0')
      Option.when(!text.startsWith(s"# $id0. ") && !text.startsWith(s"# $id. "))(s"$name: title is not `# $id0. ...`").toSeq ++
        Seq("- **Where:**", "- **Verdict:**").filterNot(text.contains).map(line => s"$name: no `$line` line")
    case _ => Seq(s"$name: not <NN>-<slug>.md")
  }
}
