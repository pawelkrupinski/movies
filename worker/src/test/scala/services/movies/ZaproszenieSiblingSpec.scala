package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieDetailsComplete}
import tools.GetOnlyHttpFetch

/**
 * Regression for the "Zaproszenie 2026 stuck tmdb-unresolved" bug.
 *
 * Two genuinely different films share the normalised clean title
 * `zaproszenie`:
 *   - `zaproszenie|2022` → "The Invitation" (tmdb 830788) — already resolved
 *   - `zaproszenie|2026` → "The Invite"     (tmdb 950028) — its own row, three
 *                          real cinema slots (Helios / Spójnia / Multikino),
 *                          tmdbId still None
 *
 * `needsTmdbResolution`'s sibling short-circuit
 * (`hasResolvedSiblingByTitle`) matched on cleanTitle ALONE, ignoring year —
 * so the resolved 2022 row made it skip resolution of the 2026 row forever.
 * Both recovery paths were defeated: the scrape-trigger path skipped it here,
 * and the daily `retryUnresolvedTmdb` sweep re-dispatches non-forced, hitting
 * this same guard again. Only an operator force-reenrich got past it.
 *
 * The fix gates the short-circuit on the `(title, year)` key carrying NO
 * cinema slots of its own. The redirect that the guard protects (Mortal
 * Kombat II, same film across adjacent production/release years, collapsed
 * onto one key — see `MortalKombatDisappearanceSpec`) leaves the other year's
 * key empty; a genuinely distinct different-year film keeps its own slots and
 * must resolve.
 */
class ZaproszenieSiblingSpec extends AnyFlatSpec with Matchers {

  private val Title       = "Zaproszenie"
  private val Sibling2022 = 830788 // "The Invitation" (2022)
  private val Invite2026  = 950028 // "The Invite" (2026)
  private val InviteImdb  = "tt14173636"

  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  // TMDB stub: a "Zaproszenie" search returns the 2026 film (tmdb 950028,
  // Polish title "Zaproszenie." — note the trailing period, which is NOT an
  // exact match for the query, so `pickBest` lands it via the year-distance
  // tiebreak). `/external_ids` → tt14173636. `fullDetails` is Try-wrapped in
  // the client, so leaving `/movie/950028?…` unstubbed (it throws) falls back
  // to the search-hit shape — the tmdbId still lands.
  private def inviteTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Map(
      "/search/movie" ->
        s"""{"results":[{"id":$Invite2026,"title":"Zaproszenie.","original_title":"The Invite","release_date":"2026-06-25","popularity":4.3}]}""",
      s"/movie/$Invite2026/external_ids" -> s"""{"id":$Invite2026,"imdb_id":"$InviteImdb"}"""
    )),
    apiKey = Some("stub")
  )

  // A resolved 2022 sibling + an unresolved 2026 row that carries its own
  // Helios cinema slot — the exact prod shape read off `movies`.
  private def seededRepository(): InMemoryMovieRepository = new InMemoryMovieRepository(Seq(
    (Title, Some(2022), MovieRecord(imdbId = Some("tt12873562"), tmdbId = Some(Sibling2022))),
    (Title, Some(2026), MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(Title)))))
  ))

  "needsTmdbResolution" should
    "resolve a different-year film that carries its own cinema slots, despite a resolved same-title sibling" in {
    val cache = new CaffeineMovieCache(seededRepository())
    val bus   = new InProcessEventBus()
    val svc   = new MovieService(cache, bus, inviteTmdb())
    bus.subscribe(svc.onMovieDetailsComplete)

    // A re-scrape of the 2026 film: same key, no director / originalTitle hint
    // — exactly what a director-less venue (Spójnia/Charlie) re-emits. With the
    // year-blind guard this was dropped; with the fix it resolves on its own.
    bus.publish(MovieDetailsComplete(Title, Some(2026), originalTitle = None, director = None))
    svc.stop() // drain the inline ec pool

    val row2026 = cache.get(cache.keyOf(Title, Some(2026)))
    row2026.flatMap(_.tmdbId) shouldBe Some(Invite2026)
    row2026.flatMap(_.imdbId) shouldBe Some(InviteImdb)

    // The 2022 sibling is untouched — different film, different id.
    cache.get(cache.keyOf(Title, Some(2022))).flatMap(_.tmdbId) shouldBe Some(Sibling2022)
  }

  // Negative control — preserve the phantom-prevention the guard exists for:
  // when this key has NO cinema slots of its own (its slot was redirected onto
  // the resolved sibling, the Mortal Kombat II collapse), resolution stays
  // short-circuited and TMDB is never called.
  it should "still short-circuit when the key has no cinema slots of its own" in {
    val repository = new InMemoryMovieRepository(Seq(
      (Title, Some(2022), MovieRecord(imdbId = Some("tt12873562"), tmdbId = Some(Sibling2022)))
    ))
    val cache = new CaffeineMovieCache(repository)
    val bus   = new InProcessEventBus()
    // TMDB stub that throws on any access — proves we never tried.
    val tmdb = new TmdbClient(http = new GetOnlyHttpFetch {
      override def get(url: String): String =
        throw new RuntimeException(s"TMDB should not be called: $url")
    }, apiKey = Some("stub"))
    val svc = new MovieService(cache, bus, tmdb)
    bus.subscribe(svc.onMovieDetailsComplete)

    noException should be thrownBy bus.publish(MovieDetailsComplete(Title, Some(2026), None, None))
    svc.stop()

    cache.get(cache.keyOf(Title, Some(2026))).flatMap(_.tmdbId) shouldBe None
  }
}
