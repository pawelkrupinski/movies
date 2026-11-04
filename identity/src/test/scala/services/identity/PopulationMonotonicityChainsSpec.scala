package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The population property's reading ([[PopulationMonotonicity]]): what an A -> B -> A take is, and what an arrival chain
 *  is. The property itself runs over real families in the worker's `PopulationMonotonicitySpec`. */
class PopulationMonotonicityChainsSpec extends AnyFlatSpec with Matchers {
  import PopulationMonotonicity.returns

  "A take sequence" should "go back to a value it left only as A -> B -> A, Pending steps skipped and repeats collapsed" in {
    returns(Seq(Some("tmdb:1"), Some("tmdb:2"), Some("tmdb:1"))) shouldBe true
    returns(Seq(Some("tmdb:1"), Some("none"), Some("tmdb:1"))) shouldBe true                // Torrington
    returns(Seq(Some("none"), Some("tmdb:1"), Some("none"), Some("tmdb:1"))) shouldBe true  // Keswick
    returns(Seq(Some("tmdb:1"), None, Some("tmdb:1"))) shouldBe false                       // Pending is no take
    returns(Seq(Some("tmdb:1"), Some("tmdb:1"), Some("tmdb:2"))) shouldBe false             // refined once
    returns(Seq(Some("tmdb:1"), Some("none"))) shouldBe false                               // dropped to none, never back
  }

  "A flip" should "name its shape by the value it went back to" in {
    def shape(takes: String*) = PopulationMonotonicity.Flip(FilmTable.listing(models.KinoMuza, "T").key, 0,
      takes.zipWithIndex.map { case (t, i) => (i + 1) -> Some(t) }).shape
    shape("tmdb:1", "tmdb:2", "tmdb:1") shouldBe "film-film-film"
    shape("tmdb:1", "none", "tmdb:1") shouldBe "film-none-film"
    shape("none", "tmdb:1", "none") shouldBe "none-film-none"
  }

  "The lattice" should "find an A -> B -> A take along some arrival order, and none for a take that only refines" in {
    // three listings, the walked one index 0: the film alone, none beside listing 1, the film again with all three (Buddies)
    def buddies(m: Int): Option[String] = m match {
      case 0x1 => Some("tmdb:1")
      case 0x3 => Some("none")
      case 0x7 => Some("tmdb:1")
      case _   => None
    }
    PopulationMonotonicity.latticeFlip(3, 0, buddies) shouldBe Some(Seq(0x1 -> "tmdb:1", 0x3 -> "none", 0x7 -> "tmdb:1"))
    // none alone, the film once any sibling arrived: refined, never back
    PopulationMonotonicity.latticeFlip(3, 0, m => Some(if (m == 0x1) "none" else "tmdb:1")) shouldBe None
    // two incomparable subsets taking apart is no arrival flip: {0,1} none, {0,2} the film, the rest Pending
    PopulationMonotonicity.latticeFlip(3, 0, { case 0x3 => Some("none"); case 0x5 => Some("tmdb:1"); case _ => None }) shouldBe None
    PopulationMonotonicity.subsets(3) shouldBe Seq(0, 1, 2, 4, 3, 5, 6, 7)
  }

  "An arrival chain" should "be nested prefixes ending in the whole population, at most the steps asked" in {
    val listings = (1 to 10).map(i => FilmTable.listing(models.KinoMuza, s"Title $i"))
    PopulationMonotonicity.chains(listings, 7L, 3, 4).foreach { chain =>
      chain.size should be <= 4
      chain.last.map(_.key).toSet shouldBe listings.map(_.key).toSet
      chain.sliding(2).foreach { case Seq(a, b) => b.take(a.size) shouldBe a; case _ => () }
    }
    PopulationMonotonicity.chains(listings.take(3), 7L, 2, 8).foreach(_.map(_.size) shouldBe Seq(1, 2, 3))
  }
}
