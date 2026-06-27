package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.InMemoryStagingRepository

/**
 * Reproduces the staging RE-DIVERT flap (the Trójmiasto / Gdyńskie Centrum Filmowe
 * served-count swing) with a SYNTHETIC banner that no title rule strips — a stand-in
 * for any FUTURE festival prefix we don't yet support.
 *
 * The real "Federico Fellini: ciao a tutti!" case can no longer reproduce this: its
 * canonical rule now collapses the decorated key onto the bare film, so there's no
 * drift left to observe. This spec deliberately uses a banner that NO rule (query OR
 * canonical) will ever touch, so the drift is pure and rule-independent — proving the
 * recognition mechanism, not the Fellini patch.
 *
 * The state seeded below is the confirmed prod fold state (commit ed6ce010): a folded
 * `movies` row keyed by the BARE display title still carries the decorated per-cinema
 * scrape slot. The next scrape of that decorated form sanitizes to the decorated key,
 * which the row (keyed bare) does not match via `knownSanitized` — so the unwidened
 * divert gate re-incubates a known film into staging on EVERY tick. The widened gate
 * recognises the film via the cinema slot already sitting in `movies` and keeps it.
 */
class UnknownBannerReDivertSpec extends AnyFlatSpec with Matchers {

  // A banner no GlobalStructural or Canonical rule strips — `sanitize` keeps it whole.
  private val Banner    = "Zzz Nonexistent Fest 2099: "
  private val film      = "Toy Story 5"
  private val decorated = Banner + film
  private val cinema: Cinema = KinoMuza

  // Guard: the banner really is unsupported. If a rule ever starts stripping it, the
  // decorated and bare forms would share a key and this spec would silently stop
  // exercising the drift — fail loudly instead so it gets a fresh synthetic banner.
  require(TitleNormalizer.sanitize(decorated) != TitleNormalizer.sanitize(film),
    s"the synthetic banner is now collapsed by a rule — pick a different one")

  // The post-fold prod state: a concluded row keyed by the BARE display title that
  // carries THIS cinema's decorated scrape slot.
  private def rowWithDecoratedSlot: MovieRecord =
    MovieRecord(tmdbId = Some(1084244),
      data = Map[Source, SourceData](
        (cinema: Source) -> SourceData(
          title = Some(decorated), rawTitle = Some(decorated), releaseYear = Some(2026))))

  private def decoratedScrape: CinemaMovie =
    CinemaMovie(Movie(title = decorated, releaseYear = Some(2026)),
      cinema, posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Nil)

  "recordCinemaScrape" should
    "NOT re-divert a decorated scrape whose film already sits in movies under this cinema's slot (unknown-banner flap)" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging))
    cache.put(CacheKey(film, Some(2026)), rowWithDecoratedSlot)

    val before   = staging.findAll().toSet
    cache.recordCinemaScrape(cinema, Seq(decoratedScrape))
    val diverted = staging.findAll().toSet -- before

    withClue(
      s"a KNOWN film (this cinema's decorated slot is already in `movies`) was re-incubated " +
        s"into staging — that re-divert every tick IS the served-count flap. Staging delta: $diverted\n") {
      diverted shouldBe empty
    }
  }
}
