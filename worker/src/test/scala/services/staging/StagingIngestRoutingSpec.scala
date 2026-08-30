package services.staging

import models.{CinemaMovie, Helios, Movie, Multikino, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, StagingNewcomerDiverted}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}

import java.time.LocalDateTime
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Phase 3: with a staging sink wired, a genuinely-new film is diverted to
 *  `pending_movies` (one row per cinema|title|year) instead of `movies`; a film
 *  already known to `movies` keeps the direct path; and a newcomer a cinema
 *  stops listing is pruned from staging. */
class StagingIngestRoutingSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val When = LocalDateTime.of(2026, 6, 14, 18, 0)

  private def scrape(title: String, year: Option[Int]): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year),
      cinema    = Helios,
      posterUrl = None, filmUrl = Some(s"https://example/$title"), synopsis = None,
      cast = Nil, director = Nil, showtimes = Seq(Showtime(When, bookingUrl = None)))

  private def cacheWithStaging(repository: InMemoryMovieRepository, staging: InMemoryStagingRepository): CaffeineMovieCache =
    new CaffeineMovieCache(repository, new InProcessEventBus, staging = Some(staging), normalizer = titleNormalizer)

  "a genuinely-new film" should "be diverted to staging, not movies" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)

    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))

    cache.entries shouldBe empty
    staging.findAll().map(r => (r.cinema, r.title, r.year)) shouldBe Seq((Helios, "Brand New Film", Some(2026)))
  }

  "a first-time diverted newcomer" should "publish StagingNewcomerDiverted so the reaper kicks its chain off an event, but not republish on re-scrape" in {
    val staging = new InMemoryStagingRepository
    val bus     = new InProcessEventBus
    val kicked  = scala.collection.mutable.ArrayBuffer.empty[String]
    bus.subscribe { case StagingNewcomerDiverted(title) => kicked += title }
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, bus, staging = Some(staging), normalizer = titleNormalizer)

    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))
    kicked.toSeq shouldBe Seq("Brand New Film")                 // first divert → one event

    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))
    kicked.toSeq shouldBe Seq("Brand New Film")                 // still incubating → no republish
  }

  /** Two DIFFERENT films legitimately share a title, one row per year — and a cinema
   *  screens the newer one, which already has its own row.
   *
   *  The different-film check asks `rowFor(sanitizedTitle)`, which returns ONE row out of
   *  the year group (the lowest `canonicalRank`). Ask it about the 1953 row and the answer
   *  is right but useless: yes, the 2023 listing is a different film FROM THAT ROW — so
   *  the cinema is diverted to staging, the fold resolves it and puts it straight back on
   *  the 2023 row where it already belonged, and the next identical scrape does the whole
   *  thing again. Forever.
   *
   *  Germany, 2026-08-30: "Die einfachen Dinge" exists as both a 1953 film (tmdbId 67600)
   *  and a 2023 one (1000572); Filmhauskino im Künstlerhaus screens the 2023 print, and
   *  the convergence leg caught it as `1 known film RE-DIVERTED to staging` plus 8
   *  redundant writes on EVERY tick.
   *
   *  A divert is only justified when the listing matches NO row under that title. */
  "a cinema screening one of two same-titled films" should "not be diverted when its own row already exists" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    // Same sanitized title, two years, each a genuinely different film. Both carry the
    // published identity the different-film check reads: original title, runtime, year —
    // and the runtimes are far enough apart to CORROBORATE the title difference, which is
    // what makes the check fire at all (`MixedFilmDetector.RuntimeAgreementMinutes` = 2).
    def slot(original: String, runtime: Int, year: Int) =
      MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(
        title = Some("Die einfachen Dinge"), originalTitle = Some(original),
        runtimeMinutes = Some(runtime), releaseYear = Some(year))))
    cache.put(cache.keyOf("Die einfachen Dinge", Some(1953)), slot("The Simple Things", 108, 1953))
    cache.put(cache.keyOf("Die einfachen Dinge", Some(2023)), slot("Les choses simples", 95, 2023))

    // Multikino screens the 2023 print — the row for it already exists.
    cache.recordCinemaScrape(Multikino, Seq(CinemaMovie(
      movie     = Movie(title = "Die einfachen Dinge", releaseYear = Some(2023),
                        originalTitle = Some("Les choses simples"), runtimeMinutes = Some(95)),
      cinema    = Multikino,
      posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Seq(Showtime(When, bookingUrl = None)))))

    withClue(s"staged: ${staging.findAll().map(r => (r.cinema, r.title, r.year))}\n") {
      staging.findAll() shouldBe empty
    }
    cache.get(cache.keyOf("Die einfachen Dinge", Some(2023))).value
      .cinemaData.keySet should contain (Multikino)
  }

  "a film already known to movies" should "stay on the direct path, not divert nor kick a newcomer event" in {
    val staging = new InMemoryStagingRepository
    val bus     = new InProcessEventBus
    val kicked  = scala.collection.mutable.ArrayBuffer.empty[String]
    bus.subscribe { case StagingNewcomerDiverted(title) => kicked += title }
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, bus, staging = Some(staging), normalizer = titleNormalizer)
    cache.put(cache.keyOf("Kumotry", Some(2026)),
      MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    cache.recordCinemaScrape(Helios, Seq(scrape("Kumotry", Some(2026))))

    kicked shouldBe empty
  }

  "a film already known to movies" should "stay on the direct path, not divert" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    // Seed a known film (any same-sanitize row in movies).
    cache.put(cache.keyOf("Kumotry", Some(2026)),
      MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    cache.recordCinemaScrape(Helios, Seq(scrape("Kumotry", Some(2026))))

    staging.findAll() shouldBe empty
    cache.get(cache.keyOf("Kumotry", Some(2026))).map(_.cinemaData.keySet) shouldBe Some(Set(Multikino, Helios))
  }

  "a known film listed under another language" should "land on the existing resolved row, not incubate as a newcomer" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    // A CONCLUDED row for Tangled, keyed under its Polish title. Its TMDB aliases
    // include the original "Tangled", so a cinema listing it in English is a known
    // film — it must NOT be diverted as a brand-new newcomer.
    cache.put(cache.keyOf("Zaplątani", Some(2010)),
      MovieRecord(tmdbId = Some(38757), data = Map[Source, SourceData](
        Tmdb      -> SourceData(title = Some("Zaplątani"), originalTitle = Some("Tangled"), releaseYear = Some(2010)),
        Multikino -> SourceData(title = Some("Zaplątani"), releaseYear = Some(2010)))))

    cache.recordCinemaScrape(Helios, Seq(scrape("Tangled", Some(2010))))

    staging.findAll() shouldBe empty   // not incubated as a newcomer
    cache.get(cache.keyOf("Zaplątani", Some(2010))).map(_.cinemaData.keySet) shouldBe Some(Set(Multikino, Helios))
  }

  "a newcomer a cinema stops listing" should "be pruned from staging" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    cache.recordCinemaScrape(Helios, Seq(scrape("Film A", Some(2026))))
    staging.findAll().map(_.title) should contain("Film A")

    // Next tick: Helios lists a different newcomer; "Film A" is no longer reported.
    cache.recordCinemaScrape(Helios, Seq(scrape("Film B", Some(2026))))
    staging.findAll().map(_.title) shouldBe Seq("Film B")
  }

  "no staging sink (default)" should "leave every scrape landing in movies" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository, new InProcessEventBus, normalizer = titleNormalizer) // staging = None
    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))
    cache.entries should have size 1
  }

  // ── A same-titled DIFFERENT film must not join the row ────────────────────
  //
  // A `movies` row is keyed by its TITLE alone, so a cinema listing an unrelated
  // film of the same name is merged straight in and the row then holds two films
  // with one tmdbId between them — production's "Joanna d'Arc" carried both
  // Besson's 1999 picture and Pálmason's 2025 one and resolved to neither.
  // Diverting sends it down the newcomer path instead, where it resolves on its
  // own hints and folds into a row of its own.
  //
  // The settle-time `MixedFilmSplitter` repairs a row that already merged; this is
  // the half that stops one forming. Its evidence has to survive raw listing data,
  // which is why it needs corroborating — see `MixedFilmDetector`.

  private def listing(title: String, original: Option[String], runtime: Option[Int], year: Option[Int]) =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year, originalTitle = original, runtimeMinutes = runtime),
      cinema    = Multikino,
      posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Seq(Showtime(When, bookingUrl = None)))

  private def rowFor(cache: CaffeineMovieCache, title: String, year: Option[Int], slot: SourceData): Unit =
    cache.put(cache.keyOf(title, year), MovieRecord(tmdbId = Some(1429348),
      data = Map[Source, SourceData](Helios -> slot)))

  "a cinema listing a DIFFERENT film of the same title" should "divert instead of joining the row" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    // The row is Ozon's "L'étranger", 120 minutes.
    rowFor(cache, "Obcy", Some(2025),
      SourceData(title = Some("Obcy"), originalTitle = Some("L'étranger"), runtimeMinutes = Some(120)))

    // Brandt Andersen's film, also released here as "Obcy": another original title
    // AND another runtime.
    cache.recordCinemaScrape(Multikino, Seq(listing("Obcy", Some("I Was A Stranger"), Some(103), Some(2024))))

    cache.get(cache.keyOf("Obcy", Some(2025))).map(_.cinemaSlots.size) shouldBe Some(1)  // row untouched
    staging.findAll().map(_.cinema) shouldBe Seq(Multikino: Source)                      // it went to staging
  }

  /** The listing field the smaller cinemas fill with the POLISH title. It differs
   *  from the row's real original title, but agrees on runtime — so it merges, as
   *  it must. An earlier gate without that corroboration re-diverted nine known
   *  films on EVERY tick (`ReScrapeIdempotencySpec`). */
  "a cinema whose originalTitle is really the Polish title" should "still join the row" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    rowFor(cache, "Obcy", Some(2025),
      SourceData(title = Some("Obcy"), originalTitle = Some("L'étranger"), runtimeMinutes = Some(120)))

    cache.recordCinemaScrape(Multikino, Seq(listing("Obcy", Some("Obcy"), Some(120), None)))

    cache.get(cache.keyOf("Obcy", Some(2025))).map(_.cinemaSlots.size) shouldBe Some(2)
    staging.findAll() shouldBe empty
  }

  "a cinema publishing nothing to corroborate a title difference" should "still join the row" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    rowFor(cache, "Obcy", Some(2025),
      SourceData(title = Some("Obcy"), originalTitle = Some("L'étranger"), runtimeMinutes = Some(120)))

    // Different original title, but no runtime and no year to back it up.
    cache.recordCinemaScrape(Multikino, Seq(listing("Obcy", Some("I Was A Stranger"), None, None)))

    cache.get(cache.keyOf("Obcy", Some(2025))).map(_.cinemaSlots.size) shouldBe Some(2)
    staging.findAll() shouldBe empty
  }
}
