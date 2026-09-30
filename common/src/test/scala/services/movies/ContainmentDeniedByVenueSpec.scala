package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A title that merely ENDS with a film's name is not that film when its venue says otherwise.
 *
 * DE convergence, 2026-09-25: Kulturfabrik Meda's "Zärtlich kreist die Faust" (1990, Hilde
 * Bechert and Klaus Dexel, 70 min) matched nothing on TMDB, and the settle's containment edge
 * adopted it onto Murnau's "Faust – Eine deutsche Volkssage" (1926, 107 min) — whose English
 * title, "Faust", the longer title ends with. The venue's own year and directors both deny
 * that film; the edge must refuse it, as the fold and the landing already do.
 */
class ContainmentDeniedByVenueSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.Germany)
  private val meda       = Cinema.byDisplayName("Kulturfabrik Meda")
  private val murnau     = Cinema.byDisplayName("Murnau-Filmtheater")

  private val faust = CacheKey("Faust - Eine deutsche Volkssage", Some(1926), normalizer) -> MovieRecord(tmdbId = Some(10728),
    data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Faust - Eine deutsche Volkssage"), englishTitle = Some("Faust"),
        releaseYear = Some(1926), runtimeMinutes = Some(107), director = Seq("F. W. Murnau")),
      CinemaShowing.keyFor(murnau, "Faust - Eine deutsche Volkssage", normalizer) ->
        SourceData(title = Some("Faust - Eine deutsche Volkssage"), releaseYear = Some(1926), director = Seq("F.W. Murnau"))))

  private val zartlich = CacheKey("Zärtlich kreist die Faust", Some(1990), normalizer) -> MovieRecord(
    tmdbAttempt = Some(services.resolution.TmdbAttempt("searched", java.time.Instant.EPOCH)),
    data = Map[Source, SourceData](CinemaShowing.keyFor(meda, "Zärtlich kreist die Faust", normalizer) ->
      SourceData(title = Some("Zärtlich kreist die Faust"), releaseYear = Some(1990), runtimeMinutes = Some(70),
        director = Seq("Hilde Bechert", "Klaus Dexel"))))

  "the settle's containment edge" should "not adopt a row whose own year and directors deny the film" in {
    val clusters = FilmCanonicalizer.groupByFilm(Seq(faust, zartlich), normalizer).flatMap(FilmCanonicalizer.clusterByFilm(_, normalizer))
    clusters.map(_.map(_._1.cleanTitle).toSet).toSet shouldBe
      Set(Set("Faust - Eine deutsche Volkssage"), Set("Zärtlich kreist die Faust"))
  }
  // PL, 2026-09-30: Cinema City's "Lalka (ale to horror)" — Rod Blackhurst's "Dolly", 2025 and
  // 82 minutes — when TMDB resolves nothing for it, contains Kawalski's "Lalka" (2026, 162 min)
  // whole. A year apart is ordinary; a director the film does not credit AND a runtime 80
  // minutes off is the landing's `listingDeniesFilm`, and the settle must agree with it.
  private val pl         = TitleNormalizer.forCountry(Country.Poland)
  private val plaza      = Cinema.byDisplayName("Cinema City Poznań Plaza")
  private val multikino  = Cinema.byDisplayName("Multikino Stary Browar")
  private val kawalski = CacheKey("Lalka", Some(2026), pl) -> MovieRecord(tmdbId = Some(1321666),
    data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Lalka"), releaseYear = Some(2026), runtimeMinutes = Some(162), director = Seq("Maciej Kawalski")),
      CinemaShowing.keyFor(multikino, "Lalka", pl) ->
        SourceData(title = Some("Lalka"), releaseYear = Some(2026), runtimeMinutes = Some(162), director = Seq("Maciej Kawalski"))))
  private val horror = CacheKey("Lalka (ale to horror)", Some(2025), pl) -> MovieRecord(
    tmdbAttempt = Some(services.resolution.TmdbAttempt("searched", java.time.Instant.EPOCH)),
    data = Map[Source, SourceData](CinemaShowing.keyFor(plaza, "Lalka (ale to horror)", pl) ->
      SourceData(title = Some("Lalka (ale to horror)"), releaseYear = Some(2025), runtimeMinutes = Some(82),
        director = Seq("Rod Blackhurst"))))

  it should "not adopt a row whose own runtime and director deny the film, a year apart" in {
    val clusters = FilmCanonicalizer.groupByFilm(Seq(kawalski, horror), pl).flatMap(FilmCanonicalizer.clusterByFilm(_, pl))
    clusters.map(_.map(_._1.cleanTitle).toSet).toSet shouldBe Set(Set("Lalka"), Set("Lalka (ale to horror)"))
  }
}
