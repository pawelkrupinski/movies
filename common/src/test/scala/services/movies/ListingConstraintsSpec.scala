package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingConstraints.{CannotLink, ListingEvidence}

/**
 * The constraint model names WHY two pieces of evidence cannot be one film — the reason the
 * identity resolver labels its cannot-link edges with — and asks the rules in a fixed order.
 * The behaviour of each rule is pinned where it was found (DeniedCandidateSpec,
 * DecorationVetoSpec, ContainmentDeniedByVenueSpec, StagingFoldSpec, BareListingOneHomeSpec).
 */
class ListingConstraintsSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.Poland)

  // Kinoteka's Wong Kar Wai "Happy Together" beside Kim Jeong-hwan's 2018 film of the name.
  private val kinoteka = SourceData(title = Some("Happy Together"), releaseYear = Some(2026), director = Seq("Wong Kar Wai"),
    runtimeMinutes = Some(96))
  private val kim      = SourceData(title = Some("Happy Together"), releaseYear = Some(2018), director = Seq("Kim Jeong-hwan"),
    runtimeMinutes = Some(110))
  private val row      = MovieRecord(data = Map[Source, SourceData](Kinoteka -> kinoteka))
  private val kimRow   = MovieRecord(tmdbId = Some(551655), data = Map[Source, SourceData](Tmdb -> kim))

  "a venue whose own year and director deny a film" should "cannot-link to it, and name the denying slot" in {
    ListingConstraints.slotDeniesFilm(kinoteka, kim, normalizer) shouldBe Some(CannotLink.VenueDeniesFilm)
    ListingConstraints.rowDeniesFilms(row, Seq(kim), normalizer) shouldBe Some(CannotLink.VenueDeniesFilm)
    ListingConstraints.denyingSlot(row, Seq(kim), normalizer) shouldBe Some(kinoteka)
    ListingConstraints.foldRefused(row, Seq(kimRow), normalizer) shouldBe Some(CannotLink.VenueDeniesFilm)
  }

  it should "not cannot-link to the film it published" in {
    val wong = kim.copy(releaseYear = Some(1997), director = Seq("Wong Kar Wai"))
    ListingConstraints.rowDeniesFilms(row, Seq(wong), normalizer) shouldBe None
    ListingConstraints.foldRefused(row, Seq(kimRow.copy(data = Map[Source, SourceData](Tmdb -> wong))), normalizer) shouldBe None
  }

  "a listing matched by its title's shape" should "be refused on its own crew and runtime" in {
    val itEnds = MovieRecord(tmdbId = Some(1422011), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("It Ends"), releaseYear = Some(2026), runtimeMinutes = Some(89), director = Seq("Alexander Ullom"))))
    val baldoni = ListingEvidence(originalTitle = None, runtime = Some(130), year = None, director = Seq("Justin Baldoni"))
    ListingConstraints.originalTitleNamesAnotherFilm(baldoni, itEnds, normalizer) shouldBe None
    ListingConstraints.landingRefused(baldoni, itEnds, normalizer) shouldBe Some(CannotLink.ListingDeniesFilm)
    ListingConstraints.landingRefused(baldoni.copy(director = Nil), itEnds, normalizer) shouldBe None
  }

  "a bare listing" should "keep its incumbent home, and a listing naming a year or runtime not" in {
    ListingConstraints.keepsIncumbentHome(None, None) shouldBe true
    ListingConstraints.keepsIncumbentHome(Some(2026), None) shouldBe false
    ListingConstraints.keepsIncumbentHome(None, Some(234)) shouldBe false
  }

  "two rows one venue lists under one title" should "cannot-link only when their directors share no person" in {
    ListingConstraints.venueCreditsApart(Seq("Franklin J. Schaffner"), Seq("Tim Burton"), normalizer) shouldBe
      Some(CannotLink.VenueCreditsApart)
    ListingConstraints.venueCreditsApart(Seq("Makoto Shinkai"), Seq("SHINKAI Makoto"), normalizer) shouldBe None
    ListingConstraints.venueCreditsApart(Seq("Tim Burton"), Seq(" "), normalizer) shouldBe None
    ListingConstraints.venueCreditsApart(Nil, Seq("Tim Burton"), normalizer) shouldBe None
  }
}
