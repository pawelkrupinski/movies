package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class SortedLongMapSpec extends AnyFlatSpec with Matchers {
  "a sorted long map" should "answer as the map it was merged from, removals applied before additions" in {
    val rng = new Random(5)
    var model = Map.empty[Long, String]
    var map   = SortedLongMap.empty[String]
    (1 to 40).foreach { round =>
      val added   = Seq.fill(rng.nextInt(50))((rng.nextInt(200) - 100).toLong -> s"v$round-${rng.nextInt()}").toMap
      val removed = Set.fill(rng.nextInt(20))((rng.nextInt(200) - 100).toLong)
      model = (model -- removed) ++ added
      map   = map.merged(added, removed)
      map.size shouldBe model.size
      (-110L to 110L).foreach(k => withClue(k)(map.get(k) shouldBe model.get(k)))
      map.valuesIterator.toSeq should contain theSameElementsAs model.values
    }
    map.get(Long.MinValue) shouldBe None
    SortedLongMap.empty[String].merged(Map(Long.MinValue -> "min", Long.MaxValue -> "max")).get(Long.MaxValue) shouldBe Some("max")
  }

  // The venue slot memo merges, every light projection, the tens of thousands of entries it looked up — almost all the very
  // ones it holds — and a few it built, into ~200k. Copied whole each time, its arrays (~2.4 MB a projection on worker-us)
  // lived until the next projection: promoted to the old generation every time, to die there (JFR OldObjectSample,
  // 2026-10-05: dead VenueSlotMemo.endTick arrays aged 2-8 min).
  it should "be itself when merged with only what it holds" in {
    val map = SortedLongMap.empty[String].merged((1L to 1000L).map(k => k -> s"v$k").toMap)
    map.merged(Map(5L -> map.get(5L).get, 6L -> new String("v6"))) should be theSameInstanceAs map
    map.merged(Map.empty[Long, String], k => k > 5000) should be theSameInstanceAs map
  }

  // A whole-corpus projection rebuilds the venue slot memo from empty (~100k entries on worker-us, twice): folded through
  // the overlay an entry at a time, every boot's first projection made ~100 MB of `LongMap` path copies to build two maps.
  it should "be built from empty straight into its arrays" in {
    val entries = scala.collection.mutable.LongMap.from((1L to 200000L).map(k => (k * 7919L) % 1000003L -> s"v$k"))
    val (built, allocated) = tools.ThreadAllocation.of(SortedLongMap.empty[String].merged(entries))
    built.size shouldBe entries.size
    entries.forall { case (k, v) => built.get(k).contains(v) } shouldBe true
    built.valuesIterator.toSet shouldBe entries.values.toSet
    withClue(s"allocated $allocated bytes: ")(allocated should be < 12000000L)
    SortedLongMap.empty[String].merged(Map(1L -> null, 2L -> "two")).size shouldBe 1
    SortedLongMap.empty[String].merged(Map.empty[Long, String]).size shouldBe 0
  }

  it should "take a few changes into a large map without copying it" in {
    val large = SortedLongMap.empty[String].merged((1L to 200000L).map(k => k * 7 -> s"v$k").toMap)
    val added = (1L to 30000L).map(k => k * 7 -> large.get(k * 7).get).toMap ++ (1L to 100L).map(k => (k * 7 + 1) -> s"new$k")
    val (merged, allocated) = tools.ThreadAllocation.of(large.merged(added, _ == 700000L))
    merged.size shouldBe 200000 + 100 - 1
    merged.get(700000L) shouldBe None
    merged.get(8L) shouldBe Some("new1")
    merged.get(21L) shouldBe Some("v3")
    withClue(s"allocated $allocated bytes: ")(allocated should be < 1000000L)
  }

  it should "answer as the map it was merged from across many merges, its changes folded into its arrays as they pile up" in {
    val rng   = new Random(11)
    var model = (1L to 5000L).map(k => k -> s"v$k").toMap
    var map   = SortedLongMap.empty[String].merged(model)
    (1 to 300).foreach { round =>
      val added   = Seq.fill(rng.nextInt(40))((rng.nextInt(7000) + 1).toLong -> s"r$round-${rng.nextInt(3)}").toMap
      val removed = Set.fill(rng.nextInt(10))((rng.nextInt(7000) + 1).toLong)
      model = (model -- removed) ++ added
      map   = map.merged(added, removed)
      withClue(s"round $round: ") {
        map.size shouldBe model.size
        (0L to 7001L by 13).foreach(k => map.get(k) shouldBe model.get(k))
      }
    }
    map.valuesIterator.toSeq should contain theSameElementsAs model.values
  }
}
