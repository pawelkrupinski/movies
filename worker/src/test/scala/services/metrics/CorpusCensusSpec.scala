package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, CinemaShowing, City, Helios, HeliosMagnolia, Imdb, KinoApollo, MovieRecord, Rialto, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures.{cacheOver, censusOver, now, ready, row, splitRepository, tomorrow, warsaw}
import services.metrics.WorkerCorpusMetrics.Subset
import services.movies.{CacheKey, FilmWriteFence, InMemoryMovieRepository, SingleCountryNormalizer}

import java.time.{Duration, LocalDateTime}
import scala.util.Random

/**
 * [[CorpusCensus]] keeps each film's part as the cache changes, and must read exactly what the full corpus pass it
 * replaced ([[WorkerCorpusScan]], kept as the reference) reads over the store — after every write and delete any path
 * makes, and as the clock moves showtimes from upcoming to past.
 */
class CorpusCensusSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val families   = Seq(WorkerCorpusMetrics.Name, WorkerSourceFilmsMetrics.Name, WorkerShowtimesMetrics.Name, WorkerSlotFanoutMetrics.Name)

  /** Every census series in `registry`, as rendered. */
  private def series(registry: PrometheusRegistry): Seq[String] =
    PrometheusExposition.render(registry).linesIterator.filter(line => families.exists(f => line.startsWith(f + "{"))).toSeq.sorted

  // ── The equivalence ───────────────────────────────────────────────────────────────────────────

  // Venues in two cities, each listing a film under its own title or a variant one — a variant splits the row into
  // two cards — or under none (the row's own).
  private val venues: Seq[Cinema] = Seq(Helios, KinoApollo, Rialto, HeliosMagnolia)
  private val titles: Seq[Option[String]] = Seq(None, Some("Film"), Some("Film: Wersja Reżyserska"))

  /** Starts around the spec's "now", on the minute as every venue lists them: past, inside the grace, later today,
   *  tomorrow (late too), and further out. A film never lists one showtime twice at a venue — the one place the census
   *  and the read model part ways (see [[CorpusCensus]]). */
  private val starts: Seq[LocalDateTime] =
    Seq(-26 * 60, -150, -20, 0, 45, 6 * 60, 22 * 60, 30 * 60, 35 * 60 + 30, 50 * 60, 80 * 60).map(m => now.plusMinutes(m.toLong))

  private def film(rng: Random, n: Int): MovieRecord = {
    val pool  = rng.shuffle(starts).iterator
    // Each venue lists the film under one title, or under two (a re-listing whose old key outlived it: one card
    // or two, by whether the titles agree). A venue's listing key is the title it lists, as production keys it.
    val slots = rng.shuffle(venues).take(1 + rng.nextInt(venues.size)).flatMap { venue =>
      rng.shuffle(titles).take(1 + rng.nextInt(2)).map { title =>
        val listed = title.map(t => s"$t $n")
        CinemaShowing.keyFor(venue, listed.getOrElse(s"Listing $n"), normalizer) ->
          SourceData(title = listed, showtimes = pool.take(rng.nextInt(3)).toSeq.map(Showtime(_, bookingUrl = Some(s"https://book/$n"))))
      }
    }
    MovieRecord(
      tmdbId         = Option.when(rng.nextInt(4) > 0)(1000 + n),
      imdbId         = Option.when(rng.nextBoolean())(s"tt$n"),
      imdbRating     = Option.when(rng.nextBoolean())(6.5),
      rottenTomatoes = Option.when(rng.nextInt(3) == 0)(80),
      data           = slots.toMap ++ Option.when(rng.nextBoolean())(Tmdb -> SourceData(title = Some(s"Film $n"))) ++
                         Option.when(rng.nextInt(3) == 0)(Imdb -> SourceData(title = Some(s"Film $n"))))
  }

  /** One random write or delete, through one of the cache's paths. */
  private final class World(seed: Long) {
    val rng        = new Random(seed)
    val clock      = new tools.MutableClock(now.atZone(warsaw).toInstant)
    val repository = splitRepository()
    (1 to 6).foreach(n => repository.upsert(s"Film $n", Some(2026), film(rng, n)))
    val cache      = cacheOver(repository)
    val (registry, reference) = (new PrometheusRegistry(), new PrometheusRegistry())
    val census     = censusOver(cache, registry, clock)
    val collectors = ReferenceCensus.collectors(reference, "pl", City.all, clock, normalizer)
    census.seed()

    private def keys: Seq[CacheKey] = cache.entries.map(_._1).sortBy(_.cleanTitle)
    private def anyKey: Option[CacheKey] = Option.when(keys.nonEmpty)(keys(rng.nextInt(keys.size)))
    private var next = 7

    def step(): String = rng.nextInt(12) match {
      case 0 =>
        next += 1; cache.put(cache.keyOf(s"Film $next", Some(2026)), film(rng, next)); s"put Film $next"
      case 1 => anyKey.fold("—") { key =>
          val record = cache.get(key).get
          record.data.keys.filter(Source.cinemaOf(_).isDefined).toSeq.sortBy(_.toString).headOption.fold("—") { source =>
            val moved = SourceData(title = record.data(source).title,
              showtimes = rng.shuffle(starts).take(rng.nextInt(4)).map(Showtime(_, bookingUrl = Some("https://book/moved"))))
            cache.putSlotIfPresent(key, source, moved); s"landing at $source of ${key.cleanTitle}"
          }
        }
      case 2 => anyKey.fold("—") { key => cache.putIfPresent(key, _.copy(metascore = Some(50 + rng.nextInt(40)))); s"rating of ${key.cleanTitle}" }
      case 3 => anyKey.fold("—") { key =>
          val id = cache.idOf(key).get
          next += 1; cache.writeProjected(id, cache.keyOf(s"Film $next", Some(2026)), film(rng, next)); s"retitle ${key.cleanTitle} to Film $next"
        }
      case 4 => anyKey.fold("—") { key =>
          val id = cache.idOf(key).get
          val before = cache.get(key).get
          cache.patchProjected(id, key, before, before.copy(data = film(rng, 99).data)); s"patch ${key.cleanTitle}"
        }
      case 5 => anyKey.fold("—") { key => cache.retireProjected(cache.idOf(key).get); s"retire ${key.cleanTitle}" }
      case 6 => anyKey.fold("—") { key => cache.invalidate(key); s"invalidate ${key.cleanTitle}" }
      case 7 => anyKey.fold("—") { key =>
          // another process's write, through the change stream
          val stored = repository.findByIdChecked(cache.idOf(key).get).answered.get
          val record = stored.record.copy(imdbId = None, tmdbId = None)
          repository.upsert(stored.id, key, record)
          cache.applyUpsert(repository.findByIdChecked(stored.id).answered.get, FilmWriteFence.Unfenced); s"stream upsert of ${key.cleanTitle}"
        }
      case 8 => anyKey.fold("—") { key =>
          val id = cache.idOf(key).get
          repository.delete(id); cache.applyDelete(id); s"stream delete of ${key.cleanTitle}"
        }
      case 10 => anyKey.fold("—") { key =>
          // another process's change confined to one venue's showtimes, applied from that venue alone
          val stored = repository.findByIdChecked(cache.idOf(key).get).answered.get
          stored.record.data.keys.collect { case s: CinemaShowing => s.cinema }.toSeq.sortBy(_.displayName).headOption.fold("—") { at =>
            val moved = stored.record.copy(data = stored.record.data.map {
              case (s: CinemaShowing, slot) if s.cinema == at =>
                s -> slot.copy(showtimes = rng.shuffle(starts).take(rng.nextInt(3)).map(Showtime(_, bookingUrl = Some("https://book/venue"))))
              case other => other
            })
            repository.upsert(stored.id, key, moved)
            val read = repository.findByIdChecked(stored.id).answered.get.record
            val slots = read.data.toSeq.collect { case (s: CinemaShowing, slot) if s.cinema == at => s -> slot }
            cache.applyVenueSlots(services.movies.VenueSlots(stored.id, Map(at -> slots)), FilmWriteFence.Unfenced)
            s"venue apply at $at of ${key.cleanTitle}"
          }
        }
      case 9 => anyKey.fold("—") { key =>
          // another process's write the change stream missed, caught by the backstop rehydrate
          val stored = repository.findByIdChecked(cache.idOf(key).get).answered.get
          repository.upsert(stored.id, key, stored.record.copy(data = film(rng, 98).data))
          cache.rehydrate(); s"rehydrate over ${key.cleanTitle}"
        }
      case _ =>
        val minutes = 5 + rng.nextInt(18 * 60)
        clock.advance(Duration.ofMinutes(minutes.toLong)); s"clock +${minutes}m"
    }

    def check(what: String): Unit = {
      census.publish()
      WorkerCorpusScan.over(repository, collectors)
      val (counted, scanned) = (series(registry), series(reference))
      withClue(s"seed $seed, after $what — census only: ${counted.diff(scanned)}; scan only: ${scanned.diff(counted)}: ")(counted shouldBe scanned)
    }
  }

  "The corpus census" should "read what a full pass over the store reads, after every write, delete and tick" in {
    (1 to 12).foreach { seed =>
      val world = new World(seed.toLong)
      world.check("the seed")
      (1 to 60).foreach(_ => world.check(world.step()))
    }
  }

  it should "read the same over a cache that keeps the showtimes as over one that keeps their starts" in {
    val rng   = new Random(3)
    val films = (1 to 20).map(n => row(s"Film $n", film(rng, n)))
    val split = splitRepository()
    films.foreach(r => split.upsert(r.title, r.year, r.record))
    val whole = new InMemoryMovieRepository(films.map(r => (r.title, r.year, r.record)), normalizer = normalizer)
    def read(over: services.movies.MovieRepository) = {
      val registry = new PrometheusRegistry()
      val census   = censusOver(cacheOver(over), registry)
      census.seed(); census.publish()
      series(registry)
    }
    cacheOver(split).entries.forall(_._2.data.valuesIterator.forall(_.showtimes.isEmpty)) shouldBe true
    val (lean, stitched) = (read(split), read(whole))
    withClue(s"lean only: ${lean.diff(stitched)}; whole only: ${stitched.diff(lean)}: ")(lean shouldBe stitched)
  }

  // ── Honest about a cache that never read the corpus ─────────────────────────────────────────────

  // A cache that has only the films written since boot is not a smaller corpus. Published, it reads as a collapse.
  it should "publish nothing, and count the miss, while the cache has never read the whole corpus" in {
    val registry   = new PrometheusRegistry()
    val counter    = CorpusCensus.incompleteCounter(registry)
    val unreadable = new InMemoryMovieRepository(Seq(("Film", Some(2026), ready(Helios, 1, tomorrow))), normalizer = normalizer) {
      override def findAllChecked(): tools.ReadOutcome[Seq[services.movies.StoredMovieRecord]] =
        services.movies.UnreadableRepositories.failed
    }
    val cache  = cacheOver(unreadable)
    cache.put(cache.keyOf("Written Since Boot", Some(2026)), ready(Helios, 2, tomorrow))
    cache.hydrated shouldBe false
    val census = censusOver(cache, registry, metrics = CorpusCensusMetrics.prometheus(counter, "pl"))
    census.seed()
    census.publish()
    census.publish()

    val text = PrometheusExposition.render(registry)
    PrometheusExposition.sample(text, CorpusCensus.IncompleteMetricName, """country="pl"""") shouldBe Some(2.0)
    PrometheusExposition.sample(text, WorkerCorpusMetrics.Name, s"""country="pl",subset="${Subset.Total}"""") shouldBe None
    PrometheusExposition.sample(text, WorkerShowtimesMetrics.Name, """city="poznan",country="pl"""") shouldBe Some(0.0)
  }

  it should "not count a tick over a cache that holds the whole corpus" in {
    val registry = new PrometheusRegistry()
    val counter  = CorpusCensus.incompleteCounter(registry)
    val census   = censusOver(cacheOver(splitRepository()), registry, metrics = CorpusCensusMetrics.prometheus(counter, "pl"))
    census.seed()
    census.publish()
    PrometheusExposition.sample(PrometheusExposition.render(registry), CorpusCensus.IncompleteMetricName, """country="pl"""") shouldBe Some(0.0)
  }

  // ── Kept, not re-read ───────────────────────────────────────────────────────────────────────────

  // Told of a film under the cache's lock for it, on the writer's thread: a wide release lands once per venue, and
  // deriving its part there cost a pass over its thousands of slots per landing.
  it should "only note a changed film when told of it, and derive it at the next reading" in {
    val counting = new services.movies.CountingNormalizer(normalizer.rules, _.startsWith("Wide"))
    val cache    = new services.movies.CaffeineMovieCache(splitRepository(), normalizer = counting, clock = CorpusMetricsFixtures.clock)
    val census   = censusOver(cache, new PrometheusRegistry())
    census.seed()
    val wide = row("Wide Film", MovieRecord(tmdbId = Some(1), data = venues.map(v =>
      (CinemaShowing.keyFor(v, "Wide Film", normalizer): Source) -> SourceData(title = Some("Wide Film"),
        showtimes = Seq(Showtime(tomorrow, bookingUrl = None)))).toMap))
    val key = cache.keyOf("Wide Film", Some(2026))
    counting.reset()
    (1 to 50).foreach(_ => census.held(key, Some(wide)))
    counting.calls shouldBe 0
    census.reading().subset(Subset.Total) shouldBe 1
    counting.calls should be > 0
  }

  // A film whose part cannot be derived keeps its last part — and is tried again on the next reading, not left
  // wrong until its next write.
  it should "derive again at the next reading a film it could not derive" in {
    val census = censusOver(cacheOver(splitRepository()), new PrometheusRegistry())
    census.seed()
    val key    = CacheKey("Flaky", Some(2026), normalizer)
    var broken = true
    val film   = row("Flaky", ready(Helios, 1, tomorrow))
    val flaky  = film.copy(record = new MovieRecord(tmdbId = Some(1), data = film.record.data) {
      override def readyToProject: Boolean = if (broken) throw new IllegalStateException("bug") else super.readyToProject
    })
    census.held(key, Some(flaky))
    census.reading().subset(Subset.Total) shouldBe 0
    broken = false
    census.reading().subset(Subset.Total) shouldBe 1
  }

  // A scrape landing moves one slot of a film that can hold thousands: re-deriving every slot on each landing made a
  // film's landings cost the square of its venues.
  it should "re-derive only the slots a write moved" in {
    val record = ready(Helios, 1, tomorrow).copy(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("x"), showtimes = Seq(Showtime(tomorrow, bookingUrl = None))),
      KinoApollo -> SourceData(title = Some("x"), showtimes = Seq(Showtime(tomorrow, bookingUrl = None)))))
    val stored = row("Kept", record)
    val prior  = FilmCensus.of(stored, normalizer, None)
    val moved  = stored.copy(record = record.copy(data = record.data.updated(KinoApollo, SourceData(title = Some("x")))))
    val next   = FilmCensus.of(moved, normalizer, Some(prior))
    (next.partAt(Helios).get eq prior.partAt(Helios).get) shouldBe true
    (next.partAt(KinoApollo).get eq prior.partAt(KinoApollo).get) shouldBe false
    next.cards.flatten.size shouldBe 1
  }

  // A film the cache wraps anew with the slots it held — an echo, a write elsewhere on the record — derived its part's
  // maps and title groups whole again, each kept until the film's next change: promoted and left to die old (worker-us
  // heap dump 2026-10-05: census parts were the largest named holder of dead map nodes and lists).
  it should "keep its slots' map and title groups, the very objects, when no slot moved" in {
    val record = ready(Helios, 1, tomorrow).copy(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("x"), showtimes = Seq(Showtime(tomorrow, bookingUrl = None))),
      KinoApollo -> SourceData(title = Some("x"), showtimes = Seq(Showtime(tomorrow, bookingUrl = None)))))
    val stored = row("Kept", record)
    val prior  = FilmCensus.of(stored, normalizer, None)
    val again  = FilmCensus.of(stored.copy(record = record.copy(metascore = Some(70))), normalizer, Some(prior))
    prior.cards.flatten.size shouldBe 2
    (again.cards eq prior.cards) shouldBe true
    (again.sameSlots(prior)) shouldBe true
  }

  // ── A venue across a zone line from its city ─────────────────────────────────────────────────────

  // Key Twin Russell Springs keeps Central time inside Somerset, KY (Eastern). The web judges its showtimes started on
  // the venue's clock (`StartedShowtimeCut`); on the city's clock the census dropped a 20:30 show at 19:45 venue time
  // and, for the hour until the web dropped it too, ReadModelServingDiffersFromCorpus paged every US evening.
  it should "judge a venue across a zone line from its city on the venue's own clock, as the web does" in {
    val venue    = models.UsRoster.byDisplayName("Key Twin Russell Springs")
    val city     = City.forCinema(venue).get
    val cityTime = LocalDateTime.parse("2026-06-10T21:15")   // EDT; 20:15 CDT at the venue
    val at       = java.time.Clock.fixed(cityTime.atZone(city.zoneId).toInstant, java.time.ZoneOffset.UTC)
    val late     = CorpusMetricsFixtures.ready(venue, 1, LocalDateTime.parse("2026-06-10T20:30"))   // in 15 min, venue time
    val read     = CorpusMetricsFixtures.reading(Seq(row("Late Show", late)), at)
    read.served.get((city.slug, WorkerSourceFilmsMetrics.Scope.All)) shouldBe Some(1)
    read.showtimes.get(city.slug) shouldBe Some(1)
  }
}
