package services.readmodel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScreeningMemoSpec extends AnyFlatSpec with Matchers {

  private def written(n: Int) = WrittenScreening(n, Some(n + 1))

  "the screening memo" should "give back every row id exactly as it was put, the card's own and a legacy one" in {
    val memo = new ScreeningMemo
    val rows = Map("lalka|2026|poznan|Kino Muza" -> written(1), "lalka|2026|warszawa|Kinoteka" -> written(2),
                   "legacy-row-id" -> written(3))
    memo.update("lalka|2026", rows)
    memo.of("lalka|2026") shouldBe rows
    memo.ids("lalka|2026").toSet shouldBe rows.keySet
    memo.holdsAny("lalka|2026") shouldBe true
    memo.holds("lalka|2026|poznan|Kino Muza") shouldBe true
    memo.holds("legacy-row-id") shouldBe true
    memo.holds("other|2026|poznan|Kino Muza") shouldBe false
  }

  it should "hold one venue suffix string for every card screening at that venue" in {
    // Held whole, US's 99k row ids were 10.8 MB of distinct strings; the venue suffix is shared.
    val memo = new ScreeningMemo
    memo.update("lalka|2026", Map("lalka|2026|poznan|Kino Muza" -> written(1)))
    memo.update("vincent|2026", Map("vincent|2026|poznan|Kino Muza" -> written(2)))
    val suffixes = classOf[ScreeningMemo].getDeclaredField("byCard")
    suffixes.setAccessible(true)
    val byCard = suffixes.get(memo).asInstanceOf[scala.collection.mutable.HashMap[String, Map[String, WrittenScreening]]]
    val (a, b) = (byCard("lalka|2026").keys.head, byCard("vincent|2026").keys.head)
    a shouldBe "poznan|Kino Muza"
    assert(a eq b)
  }

  it should "forget a row of a card, a row of whichever card holds it, and a card with its last row" in {
    val memo = new ScreeningMemo
    memo.update("lalka|2026", Map("lalka|2026|poznan|Kino Muza" -> written(1), "lalka|2026|poznan|Rialto" -> written(2)))
    memo.update("vincent|2026", Map("vincent|2026|poznan|Rialto" -> written(3)))
    memo.forget("lalka|2026", "lalka|2026|poznan|Kino Muza")
    memo.ids("lalka|2026") shouldBe Seq("lalka|2026|poznan|Rialto")
    memo.forget("vincent|2026|poznan|Rialto")
    memo.holdsAny("vincent|2026") shouldBe false
    memo.update("lalka|2026", Map.empty)
    memo.holdsAny("lalka|2026") shouldBe false
    memo.of("lalka|2026") shouldBe Map.empty
  }

  it should "keep finding a whole id by id alone after some of the card's rows are replaced" in {
    val memo = new ScreeningMemo
    memo.update("c", Map("c|v" -> written(1), "stray1" -> written(2)))
    memo.updateRows("c", Seq("stray1" -> written(3), "stray2" -> written(4), "c|w" -> written(5)))
    memo.get("c", "stray1") shouldBe Some(written(3))
    memo.size("c") shouldBe 4
    memo.forget("stray1")
    memo.holds("stray2") shouldBe true
    memo.forget("stray2")
    memo.holds("stray2") shouldBe false
    memo.holds("c|w") shouldBe true
  }

  it should "find a row by id alone through its card prefix, and a whole one only while any is held" in {
    val memo = new ScreeningMemo
    memo.update("a|b", Map("a|b|c|d" -> written(1)))
    memo.update("a", Map("a|x" -> written(2), "stray" -> written(3)))
    memo.holds("a|b|c|d") shouldBe true
    memo.holds("a|x") shouldBe true
    memo.holds("stray") shouldBe true
    memo.holds("a|b|c") shouldBe false
    memo.forget("stray")
    memo.holds("stray") shouldBe false
    memo.update("b", Map("stray2" -> written(4)))
    memo.holds("stray2") shouldBe true
    memo.update("b", Map("b|y" -> written(5)))
    memo.holds("stray2") shouldBe false
    memo.forget("a|b|c|d")
    memo.holdsAny("a|b") shouldBe false
    memo.forgetCard("a")
    memo.holds("a|x") shouldBe false
  }

  // The heal asks `holds` / `forget` by id alone, ~3 times per missing venue, under the projector's
  // lock: a walk of every card, building a key per card, made a mass heal on the US corpus millions
  // of allocations long. A lookup now costs the id's own few `|`-prefixes.
  it should "look a row up by id without walking every card" in {
    val memo  = new ScreeningMemo
    (0 until 50_000).foreach(n => memo.update(s"film$n|2026", Map(s"film$n|2026|chicago|Venue $n" -> written(n))))
    val clock = tools.Stopwatch.start()
    (0 until 2_000).foreach { n =>
      memo.holds(s"absent$n|2026|chicago|Venue") shouldBe false
      memo.forget(s"absent$n|2026|chicago|Venue")
    }
    memo.holds("film7|2026|chicago|Venue 7") shouldBe true
    clock.millis should be < 1000L
  }

  it should "read and replace single rows of a card, the card's own and a legacy one, keeping the rest" in {
    val memo = new ScreeningMemo
    memo.update("lalka|2026", Map("lalka|2026|poznan|Kino Muza" -> written(1), "lalka|2026|poznan|Rialto" -> written(2)))
    memo.get("lalka|2026", "lalka|2026|poznan|Rialto") shouldBe Some(written(2))
    memo.contains("lalka|2026", "lalka|2026|poznan|Absent") shouldBe false
    memo.size("lalka|2026") shouldBe 2
    memo.updateRows("lalka|2026", Seq("lalka|2026|poznan|Rialto" -> written(7), "legacy-row-id" -> written(8)))
    memo.of("lalka|2026") shouldBe Map("lalka|2026|poznan|Kino Muza" -> written(1),
      "lalka|2026|poznan|Rialto" -> written(7), "legacy-row-id" -> written(8))
    memo.holds("legacy-row-id") shouldBe true
    memo.forget("legacy-row-id")
    memo.holds("legacy-row-id") shouldBe false
  }

  // Each of the US's ~100k rows held its input hash as Some(Integer): 32 bytes beside its own 24 (dump 2026-10-04).
  // A whole-film reprojection hands the memo every row of the card anew — ~3.9k a call on worker-us, 321 calls an hour —
  // almost all as they were. Replaced whole, each card's rows lived until its next reprojection, minutes: promoted to the
  // old generation every time, to die there.
  it should "keep the rows a card is given again as they were, the very objects, and the card's rows whole when none moved" in {
    val memo  = new ScreeningMemo
    val ids   = (1 to 200).map(n => s"lalka|2026|poznan|Venue $n")
    memo.update("lalka|2026", ids.zipWithIndex.map { case (id, n) => id -> written(n) }.toMap)
    def byCard = { val f = classOf[ScreeningMemo].getDeclaredField("byCard"); f.setAccessible(true)
      f.get(memo).asInstanceOf[scala.collection.mutable.HashMap[String, Map[String, WrittenScreening]]]("lalka|2026") }
    val held  = byCard
    memo.update("lalka|2026", ids.zipWithIndex.map { case (id, n) => id -> written(n) }.toMap)
    assert(byCard eq held, "the card's rows were replaced though none moved")
    memo.update("lalka|2026", ids.zipWithIndex.map { case (id, n) => id -> written(if (n == 7) 99 else n) }.toMap.removed(ids(3)))
    memo.get("lalka|2026", ids(7)) shouldBe Some(written(99))
    memo.get("lalka|2026", ids(3)) shouldBe None
    memo.size("lalka|2026") shouldBe 199
    ids.indices.filterNot(Set(3, 7)).foreach(n => assert(memo.get("lalka|2026", ids(n)).get eq held(s"poznan|Venue ${n + 1}"), ids(n)))
  }

  "a written row" should "keep its input hash unboxed, and read back what it was given" in {
    classOf[WrittenScreening].getDeclaredFields.map(_.getType).filterNot(_.isPrimitive) shouldBe empty
    WrittenScreening(7, Some(9)).input shouldBe Some(9)
    WrittenScreening(7, Some(0)).input shouldBe Some(0)
    WrittenScreening(7, None).input shouldBe None
    WrittenScreening(7, None) should not be WrittenScreening(7, Some(0))
    WrittenScreening(7, Some(9)) shouldBe WrittenScreening(7, Some(9))
    WrittenScreening(7, Some(9)).output shouldBe 7
  }
}
