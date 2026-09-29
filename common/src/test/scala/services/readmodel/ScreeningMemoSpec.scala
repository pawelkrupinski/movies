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
}
