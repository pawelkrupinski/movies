package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * One venue, one title, TWO films: Arc Cinema Blackpool lists "Belle (2013)" (104 min, Amma
 * Asante) and "Belle (2021)" (122 min, Mamoru Hosoda) side by side (UK corpus, 2026-09-25).
 * Both clean to "Belle", so the listing's same-slot fold — there for a venue that lists ONE
 * film twice (a dub beside a subtitled print) — unioned them into one slot on one film: one
 * of the two films was served the other's showtime, and withdrawing either listing moved the
 * survivor's showtimes between films. Each film keeps its own listing and its own showtime.
 */
class SameTitleTwoFilmsOneVenueSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.UnitedKingdom)
  private val venue      = Cinema.byDisplayName("Arc Cinema Blackpool")
  private val at2013     = LocalDateTime.of(2026, 9, 26, 18, 0)
  private val at2021     = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def listing(title: String, runtime: Int, director: String, at: LocalDateTime) =
    CinemaMovie(Movie(title, runtimeMinutes = Some(runtime)), venue, None, None, None, Nil, Seq(director), Seq(Showtime(at, None)))

  "a venue listing two films under one title" should "keep each film's showtime on that film, and stay put on a rescrape" in
    run(heal = false)

  it should "split a slot an earlier tick merged, on the next scrape" in run(heal = true)

  private def run(heal: Boolean) = {
    val repository = new InMemoryMovieRepository(normalizer = normalizer)
    val cache      = new CaffeineMovieCache(repository, normalizer = normalizer,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-25T12:00:00Z"), java.time.ZoneOffset.UTC))
    def film(tmdbId: Int, year: Int, runtime: Int, director: String) = MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Belle"), releaseYear = Some(year), runtimeMinutes = Some(runtime), director = Seq(director))))
    // What the merge left in storage: ONE slot, on the 2013 film, holding both showtimes.
    val merged = if (!heal) Map.empty[Source, SourceData] else Map[Source, SourceData](
      CinemaShowing.keyFor(venue, "Belle", normalizer) -> SourceData(title = Some("Belle"), rawTitle = Some("Belle (2013)"),
        showtimes = Seq(Showtime(at2013, None), Showtime(at2021, None))))
    cache.put(CacheKey("Belle", Some(2013), normalizer), film(157827, 2013, 104, "Amma Asante").let(r => r.copy(data = r.data ++ merged)))
    cache.put(CacheKey("Belle", Some(2021), normalizer), film(776305, 2021, 122, "Mamoru Hosoda"))

    val board = Seq(listing("Belle (2013)", 104, "Amma Asante", at2013), listing("Belle (2021)", 122, "Mamoru Hosoda", at2021))
    cache.recordCinemaScrape(venue, board)

    def showtimesOf(year: Int) = repository.findAll().filter(_.year.contains(year)).flatMap(_.record.cinemaSlots)
      .filter { case (s, _) => Source.cinemaOf(s).contains(venue) }.flatMap(_._2.showtimes.map(_.dateTime)).toSet
    showtimesOf(2013) shouldBe Set(at2013)
    showtimesOf(2021) shouldBe Set(at2021)

    val settled = repository.findAll().sortBy(_.id.value)
    val writes  = new java.util.concurrent.atomic.AtomicInteger
    repository.watchChanges(_ => { writes.incrementAndGet(); () }, _ => { writes.incrementAndGet(); () })
    cache.recordCinemaScrape(venue, board)
    withClue("an identical re-scrape moved something: ")(repository.findAll().sortBy(_.id.value) shouldBe settled)
    // Not merely the same result: no write at all. Landing the first listing used to drop the
    // sibling's slot off the other film, and landing the sibling put it back — both rows
    // rewritten on every tick (UK convergence: 20 writes a tick over 12 such venues).
    withClue("an identical re-scrape wrote to the corpus: ")(writes.get shouldBe 0)
  }

  "two films listed under an IDENTICAL title with their own years" should "each keep their showtime" in {
    // DE, 2026-09-25: Cinema-Arthouse lists "Sinn und Sinnlichkeit" twice — 1995 (Ang Lee, 135
    // min) at 19:30 and 2026 (Georgia Oakley, 132 min) at 20:00 — with the same raw title.
    val de       = TitleNormalizer.forCountry(Country.Germany)
    val arthouse = Cinema.byDisplayName("Cinema-Arthouse")
    val (at1995, at2026) = (LocalDateTime.of(2026, 10, 14, 19, 30), LocalDateTime.of(2026, 10, 14, 20, 0))
    val repository = new InMemoryMovieRepository(normalizer = de)
    val cache      = new CaffeineMovieCache(repository, normalizer = de,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-25T12:00:00Z"), java.time.ZoneOffset.UTC))
    def listing(year: Int, runtime: Int, at: LocalDateTime) = CinemaMovie(
      Movie("Sinn und Sinnlichkeit", releaseYear = Some(year), runtimeMinutes = Some(runtime),
        originalTitle = Some("Sense and Sensibility")), arthouse, None, None, None, Nil, Nil, Seq(Showtime(at, None)))
    val board = Seq(listing(2026, 132, at2026), listing(1995, 135, at1995))

    cache.recordCinemaScrape(arthouse, board)
    cache.recordCinemaScrape(arthouse, board)

    def showtimesOf(year: Int) = repository.findAll().filter(_.year.contains(year)).flatMap(_.record.cinemaSlots)
      .filter { case (s, _) => Source.cinemaOf(s).contains(arthouse) }.flatMap(_._2.showtimes.map(_.dateTime)).toSet
    showtimesOf(1995) shouldBe Set(at1995)
    showtimesOf(2026) shouldBe Set(at2026)
  }

  "two films under one title, only one of them with a year" should "each keep their showtime" in {
    // US, 2026-09-25 (recorder run 36153174348): Marion Theatre Ocala lists "Planet of the Apes"
    // (Franklin J. Schaffner, 112 min, no year) beside "Planet of the Apes (2001)" (Tim Burton,
    // 119 min), a double bill at 14:00. One year among the two is no year DISAGREEMENT, so the
    // fold read them as one film printed twice and unioned them onto one slot; the directors the
    // venue credits say two films.
    val us     = TitleNormalizer.forCountry(Country.UnitedStates)
    val marion = Cinema.byDisplayName("Marion Theatre Ocala")
    val at     = LocalDateTime.of(2026, 9, 26, 14, 0)
    val repository = new InMemoryMovieRepository(normalizer = us)
    val cache      = new CaffeineMovieCache(repository, normalizer = us,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-25T12:00:00Z"), java.time.ZoneOffset.UTC))
    def film(tmdbId: Int, year: Int, runtime: Int, director: String) = MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Planet of the Apes"), releaseYear = Some(year), runtimeMinutes = Some(runtime), director = Seq(director))))
    cache.put(CacheKey("Planet of the Apes", Some(1968), us), film(871, 1968, 112, "Franklin J. Schaffner"))
    cache.put(CacheKey("Planet of the Apes", Some(2001), us), film(869, 2001, 119, "Tim Burton"))
    def listing(title: String, runtime: Int, director: String, at: LocalDateTime) =
      CinemaMovie(Movie(title, runtimeMinutes = Some(runtime)), marion, None,
        Some(s"https://www.flicks.us/movie/${title.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripSuffix("-")}/"),
        None, Nil, Seq(director), Seq(Showtime(at, None)))
    val board = Seq(listing("Planet of the Apes", 112, "Franklin J. Schaffner", at),
                    listing("Planet of the Apes (2001)", 119, "Tim Burton", at))

    cache.recordCinemaScrape(marion, board)
    val settled = repository.findAll().sortBy(_.id.value)
    cache.recordCinemaScrape(marion, board)
    withClue("an identical re-scrape moved something: ")(repository.findAll().sortBy(_.id.value) shouldBe settled)

    // Two slots on two films, each holding its own listing's showtime — not one slot on one.
    val slots = repository.findAll().flatMap(r => r.record.cinemaSlots.collect {
      case (s, sd) if Source.cinemaOf(s).contains(marion) => (r.key(us), sd.showtimes.map(_.dateTime).toSet) })
    withClue(s"slots: $slots ") {
      slots.map(_._2) shouldBe Seq(Set(at), Set(at))
      slots.map(_._1).distinct should have size 2
    }
  }

  extension (r: MovieRecord) private def let(f: MovieRecord => MovieRecord): MovieRecord = f(r)
}
