package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `ServiceTags` without a monitor around it: no collection means the in-memory
 *  map is the whole state, and the skip-if-unchanged guard decides from it. */
class ServiceTagsSpec extends AnyFlatSpec with Matchers {

  "ServiceTags.tagService" should "report a write only when the tags changed, and snapshot the in-memory view" in {
    val tags = new ServiceTags(None)

    tags.tagService("Kino Rialto", Set("custom:RialtoClient")) shouldBe true
    tags.tagService("Kino Rialto", Set("custom:RialtoClient")) shouldBe false
    tags.tagService("Kino Rialto", Set("shared:HeliosClient")) shouldBe true

    tags.snapshot() shouldBe Map("Kino Rialto" -> Set("shared:HeliosClient"))
  }

  "urlFromTags" should "extract the venue source page from the url: tag, ignoring other tags" in {
    ServiceTags.urlFromTags(Set("shared:FilmwebShowtimesClient", "url:https://www.filmweb.pl/cinema/-1714")) shouldBe
      Some("https://www.filmweb.pl/cinema/-1714")
    ServiceTags.urlFromTags(Set("custom:RialtoClient")) shouldBe None
    ServiceTags.urlFromTags(Set.empty) shouldBe None
  }

  "cinemaUrls" should "map every service carrying a url: tag to its URL, skipping the rest" in {
    val snapshot = Map(
      "DKF Rumcajs"              -> Set("shared:FilmwebShowtimesClient", "url:https://www.filmweb.pl/cinema/-1714"),
      "Multikino Stary Browar"  -> Set("shared:MultikinoClient"),
      "Kino Rialto"             -> Set("custom:RialtoClient", "url:https://www.kinorialto.poznan.pl"),
    )
    ServiceTags.cinemaUrls(snapshot) shouldBe Map(
      "DKF Rumcajs" -> "https://www.filmweb.pl/cinema/-1714",
      "Kino Rialto" -> "https://www.kinorialto.poznan.pl",
    )
  }
}
