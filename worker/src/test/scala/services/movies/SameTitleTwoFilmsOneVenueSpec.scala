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

  // Marion Theatre Ocala, US corpus 2026-09-25 (recording 36196248365): "Planet of the Apes"
  // (Schaffner, 112 min, no year) beside "Planet of the Apes (2001)" (Burton, 119 min), and
  // only Burton's film resolved in the corpus. The listing tells them apart by director, but
  // Schaffner's landed on Burton's row — the only resolved row of that title — and the two
  // took turns rewriting its slot: two writes on every identical tick of the US full leg.
  "a venue listing two films under one title, one of them undated" should "stay put on a rescrape" in {
    val us      = TitleNormalizer.forCountry(Country.UnitedStates)
    val marion  = Cinema.byDisplayName("Marion Theatre Ocala")
    val at      = LocalDateTime.of(2026, 9, 26, 14, 0)
    val repository = new InMemoryMovieRepository(normalizer = us)
    val cache      = new CaffeineMovieCache(repository, normalizer = us,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-25T12:00:00Z"), java.time.ZoneOffset.UTC))
    def film(tmdbId: Int, year: Int, runtime: Int, director: String) = MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Planet of the Apes"), releaseYear = Some(year), runtimeMinutes = Some(runtime), director = Seq(director))))
    cache.put(CacheKey("Planet of the Apes", Some(2001), us), film(869, 2001, 119, "Tim Burton"))
    def listing(title: String, runtime: Int, director: String, cast: Seq[String]) =
      CinemaMovie(Movie(title, runtimeMinutes = Some(runtime)), marion, None, None, None, cast, Seq(director), Seq(Showtime(at, None)))
    val board = Seq(
      listing("Planet of the Apes", 112, "Franklin J. Schaffner", Seq("Charlton Heston", "Roddy McDowall")),
      listing("Planet of the Apes (2001)", 119, "Tim Burton", Seq("Mark Wahlberg", "Tim Roth")))
    cache.recordCinemaScrape(marion, board)
    cache.recordCinemaScrape(marion, board)

    def directorsAt(year: Int) = repository.findAll().filter(_.year.contains(year)).flatMap(_.record.cinemaSlots)
      .filter { case (s, _) => Source.cinemaOf(s).contains(marion) }.flatMap(_._2.director).toSet
    // Burton's row carries Burton's listing only; Schaffner's lands on a row of its own.
    directorsAt(2001) shouldBe Set("Tim Burton")
    repository.findAll().filterNot(_.record.tmdbId.contains(869)).flatMap(_.record.cinemaSlots)
      .flatMap(_._2.director).toSet shouldBe Set("Franklin J. Schaffner")

    val settled = repository.findAll().sortBy(_.id.value)
    val writes  = new java.util.concurrent.atomic.AtomicInteger
    repository.watchChanges(_ => { writes.incrementAndGet(); () }, _ => { writes.incrementAndGet(); () })
    cache.recordCinemaScrape(marion, board)
    repository.findAll().sortBy(_.id.value) shouldBe settled
    writes.get shouldBe 0
  }

  // Landmark at The Glen, US recording 36584135207: a yearless "Street Fighter" credited to Kitao
  // Sakurai (the 2026 film), listed once, while the corpus held both Sakurai's 2026 film and
  // de Souza's 1994 one under the title. Listed once, the venue's credit was never consulted:
  // the listing landed on whichever row was concluded — the 1994 film — served there, and moved
  // between the two on an identical rescrape. A credit that names ONE of the same-titled films
  // and not the other is decisive.
  "a venue's undated listing of a title the corpus holds as two films" should
    "land on the film its own credit names, and stay put on a rescrape" in {
    val us    = TitleNormalizer.forCountry(Country.UnitedStates)
    val glen  = Cinema.byDisplayName("Landmark at The Glen")
    val at    = LocalDateTime.of(2026, 10, 15, 14, 15)
    val repository = new InMemoryMovieRepository(normalizer = us)
    val cache      = new CaffeineMovieCache(repository, normalizer = us,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-29T12:00:00Z"), java.time.ZoneOffset.UTC))
    def film(tmdbId: Int, year: Int, runtime: Int, director: String) = MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Street Fighter"), releaseYear = Some(year), runtimeMinutes = Some(runtime), director = Seq(director))))
    cache.put(CacheKey("Street Fighter", Some(1994), us), film(11667, 1994, 102, "Steven E. de Souza"))
    // TMDB carries no runtime for the unreleased 2026 film — so the runtime step compared the 1994
    // film alone, and it won by walkover, 102 minutes against the listing's 119.
    cache.put(CacheKey("Street Fighter", Some(2026), us), MovieRecord(tmdbId = Some(1153576), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Street Fighter"), releaseYear = Some(2026), director = Seq("Kitao Sakurai")))))
    val board = Seq(CinemaMovie(Movie("Street Fighter", runtimeMinutes = Some(119)), glen, None, None, None, Nil,
      Seq("Kitao Sakurai"), Seq(Showtime(at, None))))
    cache.recordCinemaScrape(glen, board)

    def holders = repository.findAll().filter(_.record.cinemaSlots.exists { case (s, _) => Source.cinemaOf(s).contains(glen) })
      .flatMap(_.record.tmdbId)
    holders shouldBe Seq(1153576)

    val settled = repository.findAll().sortBy(_.id.value)
    val writes  = new java.util.concurrent.atomic.AtomicInteger
    repository.watchChanges(_ => { writes.incrementAndGet(); () }, _ => { writes.incrementAndGet(); () })
    cache.recordCinemaScrape(glen, board)
    repository.findAll().sortBy(_.id.value) shouldBe settled
    writes.get shouldBe 0
  }

  // UK hard clusters, recording 36584135207: Showcase Bristol's bare "It" beside Muschietti's 2017
  // film and Cultplex's unresolved "It (1990)". Both rows are concluded — one resolved, one TMDB's
  // no-match — and the landing took the lower-ranked key, the 1990 row, while the settle folds a
  // fact-less yearless listing onto the ONE resolved film: the two moved it back and forth on
  // every identical rescrape. The landing now gives the settle's answer.
  "a bare listing beside a resolved film and an unresolved same-titled row" should
    "land on the resolved film, as the settle would fold it" in {
    val uk       = TitleNormalizer.forCountry(Country.UnitedKingdom)
    val bristol  = Cinema.byDisplayName("Showcase Bristol Avonmeads")
    val cultplex = Cinema.byDisplayName("Cultplex Manchester")
    val at       = LocalDateTime.of(2026, 10, 2, 18, 0)
    val repository = new InMemoryMovieRepository(normalizer = uk)
    val cache      = new CaffeineMovieCache(repository, normalizer = uk,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-29T12:00:00Z"), java.time.ZoneOffset.UTC))
    cache.put(CacheKey("It", Some(2017), uk), MovieRecord(tmdbId = Some(346364), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("It"), releaseYear = Some(2017), runtimeMinutes = Some(135), director = Seq("Andy Muschietti")))))
    cache.put(CacheKey("It (1990)", Some(1990), uk), MovieRecord(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy), data = Map[Source, SourceData](
      CinemaShowing.keyFor(cultplex, "It (1990)", uk) -> SourceData(title = Some("It (1990)"), rawTitle = Some("It (1990)"),
        runtimeMinutes = Some(168), director = Seq("Tommy Lee Wallace")))))
    cache.recordCinemaScrape(bristol, Seq(CinemaMovie(Movie("It"), bristol, None, None, None, Nil, Nil, Seq(Showtime(at, None)))))

    repository.findAll().filter(_.record.cinemaSlots.exists { case (s, _) => Source.cinemaOf(s).contains(bristol) })
      .flatMap(_.record.tmdbId) shouldBe Seq(346364)
  }

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
