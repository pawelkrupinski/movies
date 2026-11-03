package services.movies

import models.SourceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The listings a film's title searches are measured for are a pure function of its slots, whatever order the slots
 * arrive in -- the order a hash map of them iterates in, which differed between JVMs (Germany's order passes: "Der
 * Fremde" measured under "The Stranger" in one JVM and "L'Etranger" in the other).
 */
class TitleSearchListingsSpec extends AnyFlatSpec with Matchers {

  private def slot(title: String, original: Option[String], year: Option[Int]) =
    SourceData(title = Some(title), rawTitle = Some(title), originalTitle = original, releaseYear = year)

  private val slots = Seq(
    slot("Der Fremde", Some("The Stranger"), Some(2026)),
    slot("Der Fremde", Some("L’Étranger"), Some(2025)),
    slot("Der Fremde", None, None),
    slot("Paw Patrol: Der Dino Film", Some("PAW Patrol: The Dino Movie"), Some(2026)),
    slot("PAW Patrol: Der Dino Film", Some("Paw Patrol 3"), Some(2026)),
    slot("NTL: Fleabag (Aufz.)", None, None),
    slot("NTL: Fleabag (Aufz.)", Some("National Theatre Live: Fleabag"), Some(2026)))

  "the listings measured" should "be the same in every order of the slots" in {
    val expected = MovieService.titleSearchListings(slots)
    val outcomes = slots.permutations.map(MovieService.titleSearchListings).toSet
    outcomes shouldBe Set(expected)
  }

  they should "keep one listing per title key" in {
    MovieService.titleSearchListings(slots).map(_.title) shouldBe Seq("Der Fremde", "NTL: Fleabag (Aufz.)", "PAW Patrol: Der Dino Film")
  }
}
