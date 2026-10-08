package services.venuepages

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.FilmDetail

/** The fields a re-read unsets ([[MongoVenuePageStore.ReadFields]]) are every one a read can write: a field the document
 *  gained but the list lacks would outlive the read that stopped stating it. */
class MongoVenuePageDocumentSpec extends AnyFlatSpec with Matchers {
  "a venue page's read fields" should "be every field a read or a gone page writes, besides its key and stamp" in {
    val whole = FilmDetail(synopsis = Some("s"), cast = Seq("c"), director = Seq("d"), runtimeMinutes = Some(90), releaseYear = Some(2026),
      originalTitle = Some("o"), countries = Seq("PL"), genres = Seq("g"), posterUrl = Some("p"), trailerUrl = Some("t"), ageRating = Some("12"),
      format = List("NAP"))
    whole.productArity shouldBe 12   // a FilmDetail field added: state it in `whole` above
    val at   = java.time.Instant.parse("2026-10-08T00:00:00Z")
    val key  = VenuePageKey("group", "page")
    val read = MongoVenuePageStore.documentOf(VenuePage(key, VenuePage.Read(whole), at)).keySet
    val gone = MongoVenuePageStore.documentOf(VenuePage(key, VenuePage.Gone(404), at)).keySet
    ((read ++ gone) -- Set("_id", "group", "page", "readAt")) shouldBe MongoVenuePageStore.ReadFields.toSet
  }
}
