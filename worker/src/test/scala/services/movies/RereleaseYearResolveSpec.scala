package services.movies

import clients.TmdbClient
import models.{BreweryArtsCentreKendal, CinemaShowing, Country, Imdb, MovieRecord, OdeonCinemaBridgend, OdeonLuxeEastKilbride, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.tasks.ResolveMode
import tools.{GetOnlyHttpFetch, HttpFetch, RecordedResponses}

/**
 * A rerelease listed at its SCREENING year must not resolve to the director's film OF that year,
 * even when the row it sits on also holds a venue showing that film.
 *
 * UK prod, 2026-10-01 17:32 UTC, an operator's forced re-resolve of the row whose permanent id is
 * `hungergamesballadofsongbirdssnakes|2026`:
 * {{{
 * retitle thehungergamesmockingjaypart1|2026 -> thehungergamesmockingjaypart1| (…, ForcedReset)
 * TMDB: resolving 'The Hunger Games: Mockingjay - Part 1 (2026)' (?) [director hint: Francis Lawrence]
 * Director-walk: 'Francis Lawrence' (person 10943) year=2026 → tmdbId=1300968 'The Hunger Games: Sunrise on the Reaping'
 * }}}
 * The row as prod stores it (read 2026-10-01): Odeon Bridgend and Odeon Luxe East Kilbride list
 * "The Hunger Games: Mockingjay - Part 1 (2026)", Brewery Arts Centre Kendal lists "The Hunger
 * Games: Sunrise on the Reaping", and the TMDB/IMDb slots are Sunrise's. A MIXED row: Odeon's
 * rerelease had been mis-resolved to 1300968 and the fold joined Kendal's correct listing onto it
 * (d0f30dd10). The forced reset keeps every cinema slot, so the walk's title tier found Kendal's
 * exact title and bound all three venues to Sunrise again. The guards an earlier version of this
 * spec pinned only saw a row carrying the Mockingjay listing alone, which they already refuse.
 *
 * The right end state is Odeon off 1300968 and Kendal on it. One row cannot be both, so the
 * resolve leaves it unresolved and the settle's split sends Kendal back to staging, where it
 * resolves on its own title and the fold gives it its own document (`StagingFoldSpec`,
 * `StagingFoldIntegrationSpec`).
 *
 * Driven through the real entry point, `MovieService.resolveTmdbOnce` with `ResolveMode.Force`,
 * against the UK hard cluster's RECORDED TMDB answers
 * (`test/resources/fixtures/corpus/hard-clusters-responses-uk.json.gz`).
 */
class RereleaseYearResolveSpec extends AnyFlatSpec with Matchers {

  private val uk        = TitleNormalizer.forCountry(Country.UnitedKingdom)
  private val Title     = "The Hunger Games: Mockingjay - Part 1 (2026)"
  private val SunriseTitle = "The Hunger Games: Sunrise on the Reaping"
  private val Sunrise   = 1300968
  private val kendal    = CinemaShowing(BreweryArtsCentreKendal, "thehungergamessunriseonthereaping")

  private val odeon = SourceData(title = Some(Title), rawTitle = Some(Title),
    synopsis = Some("Katniss Everdeen (Jennifer Lawrence) is rescued by the rebels and brought to District 13 after " +
      "she shatters the Hunger Games forever. Each Hunger Games re-release will also include a different exclusive " +
      "theatrical sneak peek at The Hunger Games: Sunrise on the Reaping, releasing 20/11/2026."),
    cast = Seq("Jennifer Lawrence", "Donald Sutherland", "Liam Hemsworth", "Josh Hutcherson"),
    director = Seq("Francis Lawrence"), runtimeMinutes = Some(123))

  /** The stored row exactly as prod holds it. */
  private val mixedRow: MovieRecord = MovieRecord(
    tmdbId = Some(Sunrise), imdbId = Some("tt32558705"),
    data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some(SunriseTitle), originalTitle = Some(SunriseTitle), releaseYear = Some(2026),
        director = Seq("Francis Lawrence")),
      Imdb -> SourceData(title = Some(SunriseTitle)),
      CinemaShowing(OdeonCinemaBridgend, "thehungergamesmockingjaypart1")   -> odeon,
      CinemaShowing(OdeonLuxeEastKilbride, "thehungergamesmockingjaypart1") -> odeon,
      kendal -> SourceData(title = Some(SunriseTitle), rawTitle = Some(SunriseTitle))))

  /** Answers the recording lacks, captured from TMDB (en-GB) on 2026-10-01: the year-scoped
   *  exact-title search the director-bearing fallback asks for Kendal's title (it finds Sunrise),
   *  and Sunrise's external ids and images, which the WRONG resolution fetches — without them
   *  the pre-fix pick would die as a "dead id" and this spec would pass on the bug. */
  private val captured = Map(
    "query=The+Hunger+Games%3A+Sunrise+on+the+Reaping&year=2026" -> "fixtures/tmdb/search_sunrise_on_the_reaping_2026.json",
    "/movie/1300968/external_ids"                                -> "fixtures/tmdb/movie_1300968_external_ids.json",
    "/movie/1300968/images"                                      -> "fixtures/tmdb/movie_1300968_images.json")

  private def withCaptured(recorded: RecordedResponses): HttpFetch = new GetOnlyHttpFetch {
    override def get(url: String): String = captured.collectFirst { case (fragment, path) if url.contains(fragment) =>
      scala.io.Source.fromResource(path).mkString
    }.getOrElse(recorded.get(url))
  }

  "a forced re-resolve of the mixed Hunger Games rerelease row" should
    "not bind the Mockingjay rerelease venues to Sunrise on the Reaping" in {
    val recorded = RecordedResponses.replaying(RecordedResponses.pathFor(Country.UnitedKingdom.code))
    val tmdb     = new TmdbClient(withCaptured(recorded), apiKey = Some(settings.TmdbApiKey("replay")), language = Country.UnitedKingdom.language)
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepository(Seq((Title, Some(2026), mixedRow)), normalizer = uk), normalizer = uk)
    val service  = new MovieService(cache, new InProcessEventBus(), tmdb)

    service.resolveTmdbOnce(Title, Some(2026), originalTitle = None, director = None, mode = ResolveMode.Force)

    val rows = cache.entries.map(_._2)
    rows should have size 1
    // Unresolved beats wrong: the walk finds Kendal's exact title, but the row is named for
    // another entry of the same franchise, and two of its three venues show that one.
    rows.flatMap(_.tmdbId) shouldBe empty
    withClue(s"requests the recording does not hold: ${recorded.missedKeys}\n")(recorded.misses shouldBe 0)
    // …and Kendal is what the settle's split sends back to staging, to resolve on its own title.
    MixedFilmDetector.strays(rows.head, uk).map(_._1) shouldBe Seq(kendal)
  }
}
