package services.readmodel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The film -> cities index a movie change bumps validators through, and its rebuild on a reload. */
class FilmCitiesSpec extends AnyFlatSpec with Matchers {

  private def next(pairs: (String, Set[String])*): java.util.Map[String, java.util.Set[String]] = {
    val map = new java.util.HashMap[String, java.util.Set[String]]()
    pairs.foreach { case (film, cities) => val set = new java.util.HashSet[String](); cities.foreach(set.add); map.put(film, set) }
    map
  }

  // A movie change applied while a reload rebuilds the index reads it then: a pair the rebuild keeps
  // must be there throughout, or that change bumps no city and its pages stay stale behind a 304.
  "a rebuild" should "never hide, even mid-rebuild, a film's city it keeps" in {
    val index = new FilmCities
    index.add("belle", "wroclaw")
    index.add("dune", "krakow")
    val seenMidRebuild = scala.collection.mutable.ArrayBuffer.empty[Seq[String]]
    index.rebuild(next("belle" -> Set("wroclaw")), (_, _) => { seenMidRebuild += index.of("belle"); false })
    seenMidRebuild should not be empty
    all(seenMidRebuild) shouldBe Seq("wroclaw")
    index.of("belle") shouldBe Seq("wroclaw")
    index.of("dune") shouldBe empty
  }

  it should "keep a pair it was told to, though the reload's read did not see it" in {
    val index = new FilmCities
    index.add("belle", "poznan")
    index.rebuild(next("belle" -> Set("wroclaw")), (film, city) => film == "belle" && city == "poznan")
    index.of("belle") should contain theSameElementsAs Seq("wroclaw", "poznan")
  }
}
