package services.movies

import clients.TmdbClient
import models.{CinemaShowing, Country, MovieRecord, OdeonCinemaBridgend, OdeonLuxeEastKilbride, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.tasks.ResolveMode
import tools.{GetOnlyHttpFetch, HttpFetch, RecordedResponses}

/**
 * A rerelease listed at its SCREENING year must not resolve to the director's film OF that year.
 *
 * UK prod, 2026-10-01 17:32 UTC, an operator's forced re-resolve of the row whose permanent id is
 * `hungergamesballadofsongbirdssnakes|2026`:
 * {{{
 * retitle thehungergamesmockingjaypart1|2026 -> thehungergamesmockingjaypart1| (…, ForcedReset)
 * TMDB: resolving 'The Hunger Games: Mockingjay - Part 1 (2026)' (?) [director hint: Francis Lawrence]
 * Director-walk: 'Francis Lawrence' (person 10943) year=2026 → tmdbId=1300968 'The Hunger Games: Sunrise on the Reaping'
 * }}}
 * The guards an earlier version of this spec pinned (`SequelMarker.differentInstalments` on the
 * year-pinned tier, `titleNamesAnotherCredit`) do hold for a row carrying only the Mockingjay
 * listing. The prod row carried MORE: the fold had merged every Hunger Games rerelease that
 * resolved to 1300968 into it, and the forced reset keeps every cinema slot. One of those is the
 * 2012 film's rerelease, "The Hunger Games (2026)". The walk's TITLE tier compares a credit's
 * main title too, and Sunrise's is "The Hunger Games"; the sequel guard there saw the venue's
 * year token, not a franchise base, and let the pair through — so the row re-resolved onto the
 * very film that had merged it, every time.
 *
 * Driven through the real entry point, `MovieService.resolveTmdbOnce` with `ResolveMode.Force`,
 * against the UK hard cluster's RECORDED TMDB answers
 * (`test/resources/fixtures/corpus/hard-clusters-responses-uk.json.gz`).
 */
class RereleaseYearResolveSpec extends AnyFlatSpec with Matchers {

  private val uk        = TitleNormalizer.forCountry(Country.UnitedKingdom)
  private val Title     = "The Hunger Games: Mockingjay - Part 1 (2026)"
  private val Sunrise   = 1300968

  private def odeon(title: String) = SourceData(title = Some(title), rawTitle = Some(title),
    synopsis = Some("Katniss Everdeen (Jennifer Lawrence) is rescued by the rebels and brought to District 13 after " +
      "she shatters the Hunger Games forever. Each Hunger Games re-release will also include a different exclusive " +
      "theatrical sneak peek at The Hunger Games: Sunrise on the Reaping, releasing 20/11/2026."),
    cast = Seq("Jennifer Lawrence", "Donald Sutherland", "Liam Hemsworth", "Josh Hutcherson"),
    director = Seq("Francis Lawrence"), runtimeMinutes = Some(123))

  /** The stored row as the fold left it: resolved to Sunrise, keyed at the screening year, and
   *  holding the venues of every rerelease that resolved there. */
  private def mergedRow(otherListing: String): MovieRecord = MovieRecord(
    tmdbId = Some(Sunrise), imdbId = Some("tt32558705"),
    data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("The Hunger Games: Sunrise on the Reaping"), releaseYear = Some(2026),
        director = Seq("Francis Lawrence")),
      CinemaShowing(OdeonCinemaBridgend, "thehungergamesmockingjaypart1")   -> odeon(Title),
      CinemaShowing(OdeonLuxeEastKilbride, "thehungergamesmockingjaypart1") -> odeon(Title),
      CinemaShowing(OdeonLuxeEastKilbride, "thehungergames")                -> odeon(otherListing)))

  /** Answers the recording lacks, captured from TMDB (en-GB) on 2026-10-01: the year-scoped
   *  searches the director-bearing branch's exact-title fallback asks once the walk refuses.
   *  The rerelease spellings find nothing or no exact title; Sunrise's own title finds Sunrise.
   *  Plus Sunrise's external ids and images, which the WRONG resolution fetches — without them
   *  the pre-fix walk's pick would die as a "dead id" and this spec would pass on the bug. */
  private val captured = Map(
    "query=The+Hunger+Games+%282026%29&year=2026"                 -> "fixtures/tmdb/search_the_hunger_games_2026_rerelease.json",
    "query=The+Hunger+Games&year=2026"                             -> "fixtures/tmdb/search_the_hunger_games_year_2026.json",
    "query=The+Hunger+Games%3A+Sunrise+on+the+Reaping&year=2026"   -> "fixtures/tmdb/search_sunrise_on_the_reaping_2026.json",
    "/movie/1300968/external_ids"                                  -> "fixtures/tmdb/movie_1300968_external_ids.json",
    "/movie/1300968/images"                                        -> "fixtures/tmdb/movie_1300968_images.json")

  private def withCaptured(recorded: RecordedResponses): HttpFetch = new GetOnlyHttpFetch {
    override def get(url: String): String = captured.collectFirst { case (fragment, path) if url.contains(fragment) =>
      scala.io.Source.fromResource(path).mkString
    }.getOrElse(recorded.get(url))
  }

  private def forceResolve(row: MovieRecord): (Option[Int], RecordedResponses) = {
    val recorded   = RecordedResponses.replaying(RecordedResponses.pathFor(Country.UnitedKingdom.code))
    val tmdb       = new TmdbClient(withCaptured(recorded), apiKey = Some(settings.TmdbApiKey("replay")), language = Country.UnitedKingdom.language)
    val cache      = new CaffeineMovieCache(new InMemoryMovieRepository(Seq((Title, Some(2026), row)), normalizer = uk), normalizer = uk)
    val service    = new MovieService(cache, new InProcessEventBus(), tmdb)
    service.resolveTmdbOnce(Title, Some(2026), originalTitle = None, director = None, mode = ResolveMode.Force)
    (cache.entries.flatMap(_._2.tmdbId).headOption, recorded)
  }

  "a forced re-resolve of a merged rerelease row" should
    "not walk the original film's rerelease onto the director's film of the screening year" in {
    val (resolved, recorded) = forceResolve(mergedRow("The Hunger Games (2026)"))

    resolved should not contain Sunrise
    // A rerelease row is left unresolved rather than guessed; the fold reclaims it onto the
    // 2014 film (`expected-hard-clusters-uk.txt`: Odeon Birmingham on 131631).
    resolved shouldBe None
    withClue(s"requests the recording does not hold: ${recorded.missedKeys}\n")(recorded.misses shouldBe 0)
  }

  it should "not bind a row named for one instalment to another instalment a different venue names" in {
    val (resolved, recorded) = forceResolve(mergedRow("The Hunger Games: Sunrise on the Reaping"))

    resolved should not contain Sunrise
    withClue(s"requests the recording does not hold: ${recorded.missedKeys}\n")(recorded.misses shouldBe 0)
  }
}
