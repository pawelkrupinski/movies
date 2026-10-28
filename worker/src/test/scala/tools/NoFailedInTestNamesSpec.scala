package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * No test's name says FAILED.
 *
 * FAILED is ScalaTest's verdict word: the reporter ends a failing test's line with `*** FAILED ***`, and every
 * reader of a CI log keys on it. agentctl files a red under the FIRST line of the failed job that carries the
 * word, the pre-push known-red baseline compares those lines, and a person skims for it. A passing test whose
 * name says FAILED prints `- should … FAILED …` in every run, so it is that first line whenever its suite logs
 * before the real failure. Main #2164: `integration-test (rest)` failed in
 * MoviesWriteSkippedWhenUnchangedIntegrationSpec, and the red was filed under
 * ChangeStreamResumeTokenIntegrationSpec's passing "answer FAILED, not absent, when the position cannot be
 * read": fixer after fixer was sent at a test that never failed, and the job's later, unrelated failures were
 * read as the same red come back. Name the outcome instead (`Failed`, "a read that failed").
 *
 * A name is a string literal where ScalaTest's DSL takes one: after `should` / `must` / `can`, `behavior of`
 * and `test(` / `describe(` / `it(` / `ignore(`, or before `in`, `taggedAs`, `when {` / `which {` / `- {`, or a
 * `should` / `must` / `can` that a name or a scope follows (FlatSpec subjects, WordSpec and FreeSpec scopes; a
 * matcher's `"…" should include (…)` is no name). A clue or an assertion's message is no name either: it prints
 * only beside a failure.
 */
class NoFailedInTestNamesSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{TestRoots, codeOf, read, scalaFiles}

  private val Literal = """s?"((?:[^"\\\n]|\\.)*)"""".r
  private val Before  = """(?:\b(?:should|must|can|of)\s+|\b(?:test|describe|it|ignore)\(\s*)$""".r
  private val After   = """^\s*(?:(?:should|must|can)\s+(?:s?"|\{)|(?:when|which|-)\s*\{|(?:in|taggedAs)\b)""".r
  // `\w` on either side keeps an identifier such as PROBE_FAILED out, as a log scanner's \bFAILED\b does
  private val Verdict = """(?<!\w)FAILED(?!\w)""".r

  /** The 1-based lines of `src` (comment-free) holding a test name that says FAILED. */
  private[tools] def verdictNameLines(src: String): Seq[Int] =
    Literal.findAllMatchIn(src).collect {
      case m if Verdict.findFirstIn(m.group(1)).isDefined &&
        (Before.findFirstIn(src.substring(math.max(0, m.start - 40), m.start)).isDefined ||
          After.findFirstIn(src.substring(m.end, math.min(src.length, m.end + 40))).isDefined) =>
        src.substring(0, m.start).count(_ == '\n') + 1
    }.toSeq.distinct

  "the name matcher" should "catch the verdict word in every naming position and nowhere else" in {
    verdictNameLines("""it should "keep a row when the read FAILED" in {""") shouldBe Seq(1)
    verdictNameLines(""""Token.load" should "answer FAILED, not absent" in {""") shouldBe Seq(1)
    verdictNameLines(""""a FAILED read" should "keep the row" in {""") shouldBe Seq(1)
    verdictNameLines("""test("a FAILED write") {""") shouldBe Seq(1)
    verdictNameLines("""behavior of "a FAILED probe"""") shouldBe Seq(1)
    verdictNameLines(""""when the read FAILED" - {""") shouldBe Seq(1)
    verdictNameLines("""it should s"keep ${name} when the read FAILED" in {""") shouldBe Seq(1)
    verdictNameLines("it should \"keep a row when the read\" +\n  \" FAILED, said once\" in {") shouldBe Seq(2)
    verdictNameLines("""it should "keep a row when the read failed" in {""") shouldBe empty
    verdictNameLines("""it should "back off on a PROBE_FAILED" in {""") shouldBe empty
    verdictNameLines("""said should not include "pl: FAILED"""") shouldBe empty   // an assertion's operand
    verdictNameLines(""""the FAILED line" should include ("FAILED")""") shouldBe empty
    verdictNameLines("""withClue("the write FAILED: ") { n shouldBe 1 }""") shouldBe empty
    verdictNameLines("""println(s"[ratchet] $n check(s) FAILED")""") shouldBe empty
  }

  "test sources" should "keep ScalaTest's verdict word out of every test name" in {
    val sources = scalaFiles(TestRoots)
    sources.size should be > 100
    val found = sources.flatMap { p =>
      val raw = read(p).linesIterator.toIndexedSeq
      verdictNameLines(codeOf(p)).map(n => s"$p:$n: ${raw(n - 1).trim}")
    }
    withClue("These test names say FAILED, which a log reader takes for a failure in every run they pass. Name the " +
      "outcome instead (`Failed`, \"a read that failed\"):\n" + found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }
}
