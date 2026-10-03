package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The read → decide → write-over-what-was-read loop, against an in-memory "row" whose version a
 *  second writer can move between a read and its write. */
class GuardedWriteSpec extends AnyFlatSpec with Matchers {

  private final class Row(var version: Int, var value: String) {
    /** The guarded write: lands only over the version read. */
    def writeOver(readVersion: Int, next: String): Boolean = synchronized {
      if (version != readVersion) false else { version += 1; value = next; true }
    }
  }

  "GuardedWrite" should "land the decision over the state it was made on" in {
    val row = new Row(1, "a")
    GuardedWrite(3)(() => (row.version, row.value))(s => Some(s._2 + "b"))((s, next) => row.writeOver(s._1, next)) shouldBe
      GuardedWrite.Landed("ab")
    row.value shouldBe "ab"
  }

  it should "decide again on the new state when another writer moved the row in between — never replay the old decision" in {
    val row = new Row(1, "a")
    var reads = 0
    val outcome = GuardedWrite(3) { () =>
      reads += 1
      val read = (row.version, row.value)
      if (reads == 1) row.writeOver(row.version, "other")   // lands between this read and its write
      read
    }(s => Some(s._2 + "+mine"))((s, next) => row.writeOver(s._1, next))
    outcome shouldBe GuardedWrite.Landed("other+mine")
    row.value shouldBe "other+mine"
  }

  it should "answer Unneeded when the decision, made on the current state, is to write nothing" in {
    val row = new Row(1, "a")
    GuardedWrite(3)(() => (row.version, row.value))(_ => Option.empty[String])((_, _) => fail("wrote")) shouldBe GuardedWrite.Unneeded
  }

  it should "say the row kept changing under it after its attempts, rather than writing over it" in {
    val row = new Row(1, "a")
    val outcome = GuardedWrite(3) { () =>
      val read = (row.version, row.value)
      row.writeOver(row.version, "busy")
      read
    }(s => Some("mine"))((s, next) => row.writeOver(s._1, next))
    outcome shouldBe GuardedWrite.ChangedUnderYou(3)
    row.value shouldBe "busy"
  }

  it should "let a failed read propagate — an unread row is not a lost race" in {
    a[java.io.IOException] should be thrownBy
      GuardedWrite(3)(() => throw new java.io.IOException("down"))(_ => Some("x"))((_, _) => true)
  }
}
