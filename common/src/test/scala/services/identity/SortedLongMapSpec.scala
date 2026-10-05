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
}
