package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.GetOnlyHttpFetch

/**
 * A row can carry a known `imdbId` while still missing its `tmdbId`: OMDb
 * backfill ([[services.enrichment.OmdbBackfill]]) recovers the IMDb id by
 * title+year search for exactly the films TMDB's fuzzy title search couldn't
 * resolve, and writes it onto a `tmdbId`-less row. The
 * [[services.tasks.PremiereResolveReaper]] then re-runs resolution on that row
 * in the week before its first screening.
 *
 * When that imdbId is present, TMDB's `/find/{imdbId}?external_source=imdb_id`
 * returns the EXACT tmdbId — no title/year/director guessing. This is strictly
 * better than re-running the fuzzy search that already missed, so the resolve
 * path prefers it.
 */
class TmdbFindByImdbIdResolveSpec extends AnyFlatSpec with Matchers {

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  private def tmdb(routes: (String, String)*): TmdbClient =
    new TmdbClient(http = new StubFetch(routes), apiKey = Some("stub"))

  // A row OMDb backfill would leave behind: one cinema slot, no tmdbId, but a
  // recovered imdbId. Fuzzy search already failed on this title.
  private def backfilledRow(title: String, imdbId: String): CaffeineMovieCache = {
    val seed = MovieRecord(
      imdbId = Some(imdbId),
      data   = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(title)))
    )
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq((title, None, seed))))
  }

  "the TMDB stage" should "resolve a tmdbId-less row via /find when it carries a known imdbId that fuzzy search missed" in {
    val cache = backfilledRow("Zupełnie Nieznany Tytuł", "tt7772222")
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        // Fuzzy search MISSES — the realistic OMDb-backfill case (the title is
        // one TMDB's search couldn't resolve, which is why OMDb was consulted).
        "/search/movie"             -> """{"results":[]}""",
        // Exact reverse lookup: /find returns the one film under "movie_results".
        "/find/tt7772222"           -> """{"movie_results":[{"id":777,"title":"Zupełnie Nieznany Tytuł","original_title":"Totally Unknown","release_date":"2019-04-01","popularity":1.2}]}""",
        // Downstream calls the resolve makes on the /find hit.
        "/movie/777/external_ids"   -> """{"id":777,"imdb_id":"tt7772222"}""",
        "language=en-US"            -> """{"id":777,"title":"Totally Unknown","release_date":"2019-04-01"}""",
        "append_to_response=credits" -> """{"id":777,"title":"Zupełnie Nieznany Tytuł","release_date":"2019-04-01","credits":{"crew":[],"cast":[]}}"""
      ))

    service.reEnrichSync("Zupełnie Nieznany Tytuł", None)

    // The /find hit's release year (2019) re-keys the row, so assert on the snapshot.
    cache.snapshot().flatMap(_.record.tmdbId) should contain (777)
  }
}
