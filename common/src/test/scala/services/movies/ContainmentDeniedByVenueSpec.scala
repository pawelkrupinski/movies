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
}
