package services.movies

import tools.SpecClock.given

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

  "a venue whose own year and director deny a film" should "cannot-link to it, and name the denying slot" in {
    ListingConstraints.slotDeniesFilm(kinoteka, kim, normalizer) shouldBe Some(CannotLink.VenueDeniesFilm)
    ListingConstraints.rowDeniesFilms(row, Seq(kim), normalizer) shouldBe Some(CannotLink.VenueDeniesFilm)
    ListingConstraints.denyingSlot(row, Seq(kim), normalizer) shouldBe Some(kinoteka)
  }

  it should "not cannot-link to the film it published" in {
    val wong = kim.copy(releaseYear = Some(1997), director = Seq("Wong Kar Wai"))
    ListingConstraints.rowDeniesFilms(row, Seq(wong), normalizer) shouldBe None
  }

  "a director in another script" should "never deny a film" in {
    // Helios's "Mandalorets' i Grogu - UA" credits "Джон Фавро" — Jon Favreau, whom nothing here
    // can read as one man — beside a runtime a cinema rounded otherwise.
    val favreau   = MovieRecord(tmdbId = Some(1022789), data = Map[Source, SourceData](Tmdb ->
      SourceData(title = Some("The Mandalorian & Grogu"), releaseYear = Some(2026), director = Seq("Jon Favreau"), runtimeMinutes = Some(132))))
    val ukrainian = SourceData(title = Some("Mandalorets' i Grogu - UA"), releaseYear = Some(2025), director = Seq("Джон Фавро"),
      runtimeMinutes = Some(140))
    ListingConstraints.landingRefused(ListingEvidence(None, Some(140), Some(2025), Seq("Джон Фавро")), favreau, normalizer) shouldBe None
    ListingConstraints.rowDeniesFilms(MovieRecord(data = Map[Source, SourceData](HeliosMagnolia -> ukrainian)), favreau.data.get(Tmdb).toSeq, normalizer) shouldBe None
  }

  "a listing matched by its title's shape" should "be refused on its own crew and runtime" in {
    val itEnds = MovieRecord(tmdbId = Some(1422011), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("It Ends"), releaseYear = Some(2026), runtimeMinutes = Some(89), director = Seq("Alexander Ullom"))))
    val baldoni = ListingEvidence(originalTitle = None, runtime = Some(130), year = None, director = Seq("Justin Baldoni"))
    ListingConstraints.originalTitleNamesAnotherFilm(baldoni, itEnds, normalizer) shouldBe None
    ListingConstraints.landingRefused(baldoni, itEnds, normalizer) shouldBe Some(CannotLink.ListingDeniesFilm)
    ListingConstraints.landingRefused(baldoni.copy(director = Nil), itEnds, normalizer) shouldBe None
  }

  "a title naming a season" should "cannot-link another season, or a year outside the season's two" in {
    ListingConstraints.seasonsApart(Some(2026), None, Some(2023)) shouldBe Some(CannotLink.SeasonsApart)
    ListingConstraints.seasonsApart(Some(2026), Some(2024), Some(2026)) shouldBe Some(CannotLink.SeasonsApart)
    ListingConstraints.seasonsApart(Some(2026), None, Some(2028)) shouldBe Some(CannotLink.SeasonsApart)
    ListingConstraints.seasonsApart(Some(2026), Some(2026), Some(2027)) shouldBe None
    ListingConstraints.seasonsApart(Some(2026), None, Some(2026)) shouldBe None
    // Nothing to compare: no season on the listing, or nothing dated on the other side.
    ListingConstraints.seasonsApart(None, Some(2024), Some(1949)) shouldBe None
    ListingConstraints.seasonsApart(Some(2026), None, None) shouldBe None
  }

  "two rows one venue lists under one title" should "cannot-link only when their directors share no person" in {
    ListingConstraints.venueCreditsApart(Seq("Franklin J. Schaffner"), Seq("Tim Burton"), normalizer) shouldBe
      Some(CannotLink.VenueCreditsApart)
    ListingConstraints.venueCreditsApart(Seq("Makoto Shinkai"), Seq("SHINKAI Makoto"), normalizer) shouldBe None
    ListingConstraints.venueCreditsApart(Seq("Tim Burton"), Seq(" "), normalizer) shouldBe None
    ListingConstraints.venueCreditsApart(Nil, Seq("Tim Burton"), normalizer) shouldBe None
  }
}
