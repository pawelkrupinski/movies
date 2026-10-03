package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{MainRoots, codeOf, scalaFiles}

/**
 * A read that can fail answers a `tools.ReadOutcome` (or a `tools.ScanOutcome`), never a
 * `(value, Boolean)` "checked" pair. The pair was this repository's first answer to "a failed read
 * is not data", and it half worked: a caller could take `._1` and drop the flag, and the unchecked
 * wrappers did exactly that (`findById = findByIdChecked(id)._1`, `findAllMovieIds = …._1`), so a
 * failed read came back as `None` or an empty collection all the same. A ReadOutcome cannot be
 * unwrapped without saying what a failure means, and the build will not let one be dropped.
 *
 * Flags, in main sources and the testkit, a `def …Checked(…)` whose result type is a tuple ending
 * in `Boolean`. An entry in [[Allowlist]] says WHY its pair is right.
 */
class NoCheckedTupleReadSpec extends AnyFlatSpec with Matchers {

  private val Roots = MainRoots :+ "testkit/src/main"

  private val CheckedTuple = """\bdef\s+(\w+Checked)\s*(?:\[[^\]]*\])?\s*\([^)]*\)\s*:\s*\([^)]*,\s*Boolean\s*\)""".r

  /** (file, method) → why its `(…, Boolean)` is right. Empty: none is. */
  private val Allowlist: Map[(String, String), String] = Map.empty

  private def checkedTuples(src: String): Seq[String] = CheckedTuple.findAllMatchIn(src).map(_.group(1)).toSeq.distinct

  private lazy val found: Set[(String, String)] =
    scalaFiles(Roots).flatMap(p => checkedTuples(codeOf(p)).map(p.toString -> _)).toSet

  "A read that can fail" should "answer a ReadOutcome, not a (value, Boolean) pair" in {
    val offenders = (found -- Allowlist.keySet).toSeq.sorted
    withClue(s"${offenders.size} checked read(s) answer a pair a caller can take `._1` of — answer a tools.ReadOutcome:\n  " +
      offenders.mkString("\n  ") + "\n")(offenders shouldBe empty)
  }

  "The allowlist" should "name only reads that still answer a pair" in {
    val stale = (Allowlist.keySet -- found).toSeq.sorted
    withClue(s"stale entries: ${stale.mkString(", ")} — ")(stale shouldBe empty)
  }

  "The lint" should "see a checked pair, and not a ReadOutcome or an unchecked read" in {
    val src =
      """trait A {
        |  def findByIdChecked(id: FilmId): (Option[Row], Boolean)
        |  def rowsChecked[T](c: C[T]): (Seq[T], Boolean) = ???
        |  def findAllChecked(): tools.ReadOutcome[Seq[Row]]
        |  def findById(id: FilmId): (Option[Row], Boolean)
        |}""".stripMargin
    checkedTuples(src) shouldBe Seq("findByIdChecked", "rowsChecked")
  }
}
