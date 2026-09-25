package services.readmodel

import services.movies.ChangeStreamLiveness
import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{FilmId, InMemoryMovieRepository, InMemoryScreeningsRepository, InMemorySlotsRepository, StoredMovieRecord}

import java.time.LocalDateTime

/**
 * The projector's minimal-write diff: the whole point of the read-model split
 * is that a showtime-only edit moves one screening document and a metadata-only edit
 * moves one movie document. Drives `onMovieUpsert` / `reconcile` directly against the
 * in-memory repos and asserts exactly which documents were written.
 */
class ReadModelProjectorSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  private val fid = s"${titleNormalizer.sanitize("Foo")}|2024"

  private def slot(showtimes: Seq[Showtime]) =
    SourceData(title = Some("Foo"), releaseYear = Some(2024), filmUrl = Some("https://mk/foo"), showtimes = showtimes)

  // tmdbId set → `readyToProject` (TMDB concluded); the projector only
  // publishes rows whose enrichment has settled.
  // One tmdbId per FILM, as the repository's unique index holds it: a spec seeding a second
  // film passes its own.
  private def record(rating: Option[Double], showtimes: Seq[Showtime], tmdbId: Int = 1): MovieRecord =
    MovieRecord(imdbRating = rating, tmdbId = Some(tmdbId), data = Map[Source, SourceData](Multikino -> slot(showtimes)))

  private def stored(record: MovieRecord): StoredMovieRecord = StoredMovieRecord.synthesised("Foo", Some(2024), record, services.movies.SingleCountryNormalizer.titleNormalizer)

  /** The clock this spec's projectors run on unless a case pins its own: the rolling content
   *  check's next sweeps start on the slice after Foo's, so a case counting projections across a
   *  few sweeps never also counts the check re-projecting its row. */
  private val specClock = clockSkippingSliceOf(FilmId(fid))

  private def fixture(): (ReadModelProjector, InMemoryMovieRepository, InMemoryReadModelRepository) = {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm   = new InMemoryReadModelRepository()
    (new ReadModelProjector(repository, rm, rm, clock = specClock), repository, rm)
  }


  /** CPU clock that advances by a FIXED amount per reading, so a test can assert the
   *  recorded CPU cost exactly instead of racing a real one. Wall-clock keeps running
   *  independently, which is the whole point: the two must not be the same number. */
  private class SteppingCpuClock(stepNanos: Long) extends tools.ThreadCpuClock {
    private var current = 0L
    def nanos(): Long = { val n = current; current += stepNanos; n }
  }

  /** A clock at which the rolling content check's next sweeps start on the slice AFTER `row`'s.
   *  The slice a sweep checks is numbered by the clock, so a spec counting projections across a
   *  few sweeps otherwise also counts the content check re-projecting its row — in the one or two
   *  half-hours a day that slice comes up, and nowhere else. */
  private def clockSkippingSliceOf(id: FilmId): java.time.Clock =
    java.time.Clock.fixed(java.time.Instant.ofEpochSecond(1800L * (ReadModelProjector.contentSliceOf(id) + 1)),
                          java.time.ZoneOffset.UTC)

  /** Fake scheduler that CAPTURES the fixed-rate tasks `start()` submits instead of
   *  running them on a timer, so a test can assert exactly what was scheduled and run
   *  the tasks deterministically. */
  private class CapturingScheduler extends java.util.concurrent.ScheduledThreadPoolExecutor(1) {
    val scheduled = scala.collection.mutable.Buffer.empty[Runnable]
    override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long,
                                     unit: java.util.concurrent.TimeUnit): java.util.concurrent.ScheduledFuture[?] = {
      scheduled += command
      null
    }
    def runAll(): Unit = scheduled.foreach(_.run())
  }

  // Whether splitting `synopsisByCity` off the card would spare any rewrites is a question
  // of how many card writes move ONLY that map. The projector names the parts each write moved.
  "a card write" should "be metered by the parts of the card that moved" in {
    val (projector, repository, rm) = fixture()
    val m = new RecordingReadModelProjectionMetrics()
    val metered = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    def withSynopsis(rating: Option[Double], blurb: String) =
      MovieRecord(imdbRating = rating, tmdbId = Some(1), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Foo"), synopsis = Some(blurb), showtimes = Seq(at("2026-06-12T20:00"))),
        Tmdb      -> SourceData(title = Some("Foo"), synopsis = Some("the shared blurb"))))
    repository.upsert("Foo", Some(2024), withSynopsis(Some(8.0), "Poznań's own blurb"))
    metered.onMovieUpsert(repository.findAll().head)
    m.cardWrites shouldBe Seq(Set.empty)                                                       // new: no card before

    repository.upsert("Foo", Some(2024), withSynopsis(Some(8.0), "Poznań's REVISED blurb"))   // only the city text moved
    metered.onMovieUpsert(repository.findAll().head)
    m.cardWrites.last shouldBe Set("synopsis-by-city")

    repository.upsert("Foo", Some(2024), withSynopsis(Some(9.1), "Poznań's REVISED blurb"))   // a rating moved
    metered.onMovieUpsert(repository.findAll().head)
    m.cardWrites.last shouldBe Set("ratings")

    repository.upsert("Foo", Some(2024), withSynopsis(Some(7.2), "Poznań's THIRD blurb"))     // both moved
    metered.onMovieUpsert(repository.findAll().head)
    m.cardWrites.last shouldBe Set("ratings", "synopsis-by-city")
    ReadModelProjectionMetrics.cardWriteCause(m.cardWrites.last) shouldBe "multiple"

    metered.onMovieUpsert(repository.findAll().head)                                          // nothing moved
    m.cardWrites should have size 4
    projector.stop(); metered.stop()
  }

  "the first projection of a row" should "write the movie document before its screenings" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))

    rm.movieUpserts should have size 1
    rm.screeningUpserts should have size 1
    rm.writeOrder.head        should startWith("movie:")
    rm.writeOrder(1)          should startWith("screening:")
    rm.screeningUpserts.head._id shouldBe s"$fid|poznan|Multikino Stary Browar"
  }

  // ── Enrichment gate ─────────────────────────────────────────────────────────
  // A row whose TMDB enrichment hasn't concluded (no tmdbId, no tmdbNoMatch) is
  // held back: publishing the pre-enrichment row is exactly what leaks the
  // duplicate `foo|` + `foo|2025` cards.
  private def unresolved(showtimes: Seq[Showtime]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](Multikino -> slot(showtimes)))

  "an un-enriched row" should "be held back from the read model" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(StoredMovieRecord.synthesised("Foo", Some(2024), unresolved(Seq(at("2026-06-12T20:00"))), services.movies.SingleCountryNormalizer.titleNormalizer))
    rm.movieUpserts     shouldBe empty
    rm.screeningUpserts shouldBe empty
  }

  "a row that concludes enrichment on a later upsert" should "then be projected" in {
    val (projector, _, rm) = fixture()
    val shows = Seq(at("2026-06-12T20:00"))
    projector.onMovieUpsert(StoredMovieRecord.synthesised("Foo", Some(2024), unresolved(shows), services.movies.SingleCountryNormalizer.titleNormalizer))
    rm.movieUpserts shouldBe empty  // still enriching
    // TMDB concludes as a definitive no-match → `tmdbNoMatch` → ready → projects.
    projector.onMovieUpsert(StoredMovieRecord.synthesised("Foo", Some(2024), unresolved(shows).copy(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy)), services.movies.SingleCountryNormalizer.titleNormalizer))
    rm.movieUpserts     should have size 1
    rm.screeningUpserts should have size 1
  }

  "reconcile" should "skip held-back rows while still projecting ready ones" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))   // ready (tmdbId)
    repository.upsert("Bar", Some(2024), unresolved(Seq(at("2026-06-12T20:00"))).copy(
      data = Map[Source, SourceData](Multikino ->
        SourceData(title = Some("Bar"), releaseYear = Some(2024), showtimes = Seq(at("2026-06-12T20:00"))))))
    projector.reconcile()
    rm.findAllMovies().map(_._id) should contain only fid  // Bar is held back
  }

  // Memory flatten: the 30-min reconcile holds the read model resident already, so
  // the prune must diff it off id-only projections (`findAllMovieIds` /
  // `findAllScreeningRefs`) — NOT a second full `findAllMovies` / `findAllScreenings`
  // decode of the whole corpus. A second full read on top of the resident copy is the
  // transient that exhausted the worker's 320m heap on the reconcile tick.
  "reconcile" should "prune off id-only projections, never a full findAllMovies/findAllScreenings" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                       // projects Foo
    repository.delete("Foo", Some(2024))         // source gone → next reconcile prunes
    projector.reconcile()
    rm.movieDeletes should contain(fid)          // the prune actually ran
    rm.findAllMoviesCalls.get()     shouldBe 0   // …off id-only projections, not full decodes
    rm.findAllScreeningsCalls.get() shouldBe 0
  }

  // Prune-safety: `foreachRecord` returns `false` when a Mongo batch read fails
  // mid-scan (a server-selection / socket timeout while the worker is CPU-throttled —
  // the 2026-06-29 served-films flap). `liveIds` is then a TRUNCATED view, so pruning
  // on it would delete the live cards the scan never reached. The reconcile must keep
  // them and skip the prune until a clean tick.
  private class IncompleteScanRepository(seed: Seq[(String, Option[Int], MovieRecord)])
    extends InMemoryMovieRepository(seed, normalizer = titleNormalizer) {
    @volatile var failScan = false
    // Mirrors the Mongo impl on a mid-scan read failure: deliver nothing further and
    // report the scan INCOMPLETE (rows before the failure would still have reached `f`;
    // here the very first batch dies, so none do).
    override def foreachRecord(f: StoredMovieRecord => Unit): Boolean =
      if (failScan) false else super.foreachRecord(f)
  }

  "reconcile" should "NOT prune live read-model rows when the source scan failed mid-way (incomplete)" in {
    val repository = new IncompleteScanRepository(Seq(("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))))
    val rm         = new InMemoryReadModelRepository()
    val projector  = new ReadModelProjector(repository, rm, rm, clock = specClock)

    projector.reconcile()                                  // clean scan → Foo is projected
    rm.findAllMovies().map(_._id) should contain(fid)
    val deletesBefore = rm.movieDeletes.size

    repository.failScan = true                             // next scan dies mid-way (incomplete)
    projector.reconcile()

    withClue("a live film was pruned from the read model on an INCOMPLETE source scan — the served-films flap: ") {
      rm.movieDeletes.size          shouldBe deletesBefore // no prune happened
      rm.findAllMovies().map(_._id) should contain(fid)    // Foo still served
    }
  }

  "a showtime-only change" should "move only the one screening document" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00"), at("2026-06-13T18:00")))))

    rm.movieUpserts     should have size 1  // metadata unchanged → not rewritten
    rm.screeningUpserts should have size 2  // the one changed screening document, again
  }

  // Hash-diff: the resident diff-state keeps only a 32-bit content HASH of each projected
  // document, not the full document (a heap-footprint cut — the read model already lives in
  // Mongo). Re-projecting byte-identical content must still hash-match and skip the write;
  // the change cases below (rating / showtime) guard that a differing field hash-differs and
  // writes. Together they lock the store-hash / compare-hash path.
  "re-projecting identical content" should "write nothing (hash-diff skip)" in {
    val (projector, _, rm) = fixture()
    val rec = record(Some(8.0), Seq(at("2026-06-12T20:00")))
    projector.onMovieUpsert(stored(rec))
    projector.onMovieUpsert(stored(rec))   // identical → hash matches → no rewrite
    rm.movieUpserts     should have size 1
    rm.screeningUpserts should have size 1
  }

  "a rating-only change" should "move only the movie document" in {
    val (projector, _, rm) = fixture()
    val shows = Seq(at("2026-06-12T20:00"))
    projector.onMovieUpsert(stored(record(Some(8.0), shows)))
    projector.onMovieUpsert(stored(record(Some(9.1), shows)))

    rm.movieUpserts     should have size 2  // rating changed → movie document rewritten
    rm.screeningUpserts should have size 1  // showtimes unchanged → no screening write
  }

  // ── Optimisation #1: metadata reuse across showtime-only changes ─────────────
  // The projected metadata (ResolvedMovie: title/synopsis/ratings/cast/…) is a pure
  // function of the row's cinema STRUCTURE, not its SHOWTIMES. A showtime-only change at
  // an already-present cinema must REUSE the cached ResolvedMovie (skip resolve/synopsis/
  // ratings) and recompute only the cheap screenings half; a genuine metadata change
  // (rating / synopsis / a NEW cinema→city) must RECOMPUTE and rewrite the movie document.
  "a showtime-only change at an existing cinema" should "REUSE cached metadata, not recompute it" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)

    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    m.metadataRecomputed shouldBe 1   // first projection → recomputed + cached
    m.metadataReused     shouldBe 0

    // Add a showtime at the SAME cinema — metadata inputs unchanged.
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00"), at("2026-06-13T18:00")))))
    withClue("metadata should have been reused (resolve/synopsisByCity/ratings NOT recomputed): ") {
      m.metadataReused     shouldBe 1
      m.metadataRecomputed shouldBe 1  // still just the first
    }
    rm.movieUpserts     should have size 1  // movie document NOT rewritten
    rm.screeningUpserts should have size 2  // the one changed screening document, re-written
  }

  "a rating change" should "RECOMPUTE metadata and rewrite the movie document" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val shows = Seq(at("2026-06-12T20:00"))

    projector.onMovieUpsert(stored(record(Some(8.0), shows)))
    projector.onMovieUpsert(stored(record(Some(9.1), shows)))  // rating changed, showtimes identical
    withClue("a rating change is a metadata change → must recompute, never reuse: ") {
      m.metadataRecomputed shouldBe 2
      m.metadataReused     shouldBe 0
    }
    rm.movieUpserts     should have size 2  // movie document rewritten
    rm.screeningUpserts should have size 1  // showtimes unchanged → no screening write
  }

  "a synopsis change at a cinema" should "RECOMPUTE metadata (a showtimes-stripped hash still sees it)" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val shows = Seq(at("2026-06-12T20:00"))
    def withSynopsis(s: String) = MovieRecord(tmdbId = Some(1),
      data = Map[Source, SourceData](Multikino -> slot(shows).copy(synopsis = Some(s))))

    projector.onMovieUpsert(stored(withSynopsis("A short blurb.")))
    projector.onMovieUpsert(stored(withSynopsis("A different, longer blurb entirely.")))
    m.metadataRecomputed shouldBe 2
    m.metadataReused     shouldBe 0
    rm.movieUpserts should have size 2  // synopsis is metadata → movie rewritten
  }

  "a film gaining a cinema in a NEW city" should "RECOMPUTE metadata (new city → new synopsisByCity)" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val shows = Seq(at("2026-06-12T20:00"))
    def wroclawSlot = SourceData(title = Some("Foo"), releaseYear = Some(2024),
      filmUrl = Some("https://mk/foo-wro"), showtimes = shows)

    projector.onMovieUpsert(stored(record(Some(8.0), shows)))  // Poznań only
    m.metadataRecomputed shouldBe 1
    // The film now also screens in Wrocław (MultikinoPasazGrunwaldzki) — the city set grows,
    // so the metadata (cities / synopsisByCity) genuinely changes and must NOT be reused.
    val twoCities = record(Some(8.0), shows).copy(
      data = Map[Source, SourceData](Multikino -> slot(shows), MultikinoPasazGrunwaldzki -> wroclawSlot))
    projector.onMovieUpsert(stored(twoCities))
    withClue("a new city is a metadata change → must recompute, never reuse: ") {
      m.metadataReused     shouldBe 0
      m.metadataRecomputed shouldBe 2
    }
  }

  // ── Venue reuse: a change at one venue rebuilds that venue's screenings row only ──
  // A wide US release carries thousands of venues, and building every one of their rows —
  // union, dedupe and sort each venue's showtimes — was most of the ~2s a scrape wave's
  // re-projection of such a film cost on the single apply thread, to write the ONE row
  // that moved. A row is a pure function of its card, venue and that venue's slots, so a
  // venue whose slots are unchanged since its row was written keeps it without a rebuild.
  // Two venues in each of three cities.
  private val venues = Cinema.all.distinct
    .flatMap(c => City.forCinema(c).map(_.slug -> c))
    .groupMap(_._1)(_._2).toSeq.sortBy(_._1).take(3).flatMap(_._2.take(2))

  private def venueSlot(title: String, showtimes: Seq[Showtime], url: String = "https://x/foo") =
    SourceData(title = Some(title), releaseYear = Some(2024), filmUrl = Some(url), showtimes = showtimes)

  "a showtime change at one venue of several" should "rebuild only that venue's screenings row" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    def row(extra: Seq[Showtime]) = stored(MovieRecord(tmdbId = Some(1), data = venues.take(3).zipWithIndex.map { (cinema, i) =>
      (cinema: Source) -> venueSlot("Foo", Seq(at("2026-06-12T20:00")) ++ (if (i == 1) extra else Nil)) }.toMap))

    projector.onMovieUpsert(row(Nil))
    m.venuesRebuilt shouldBe 3
    projector.onMovieUpsert(row(Seq(at("2026-06-13T18:00"))))

    withClue("only the venue whose showtimes moved is rebuilt: ") {
      m.venuesRebuilt shouldBe 4
      m.venuesReused  shouldBe 2
    }
    rm.screeningUpserts should have size 4
  }

  it should "rebuild every venue again once the memo no longer vouches for what was written" in {
    // Seeded from the read model after a restart, the memo knows each row's CONTENT but
    // not the slots it was built from — so nothing may be skipped on that say-so.
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val record = MovieRecord(tmdbId = Some(1), data = venues.take(3).map(c => (c: Source) -> venueSlot("Foo", Seq(at("2026-06-12T20:00")))).toMap)
    new ReadModelProjector(repository, rm, rm, clock = specClock).onMovieUpsert(stored(record))
    val m = new RecordingReadModelProjectionMetrics()
    val restarted = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    restarted.start()
    restarted.onMovieUpsert(stored(record))

    m.venuesRebuilt shouldBe 3
    rm.screeningUpserts should have size 3   // rebuilt, but identical, so nothing rewritten
    restarted.stop()
  }

  /** The same reuse, COUNTED across film widths — so a regression to rebuilding every
   *  venue on every re-projection fails on the count, not on a timing CI cannot hold. What
   *  a wide release costs must be the venues that moved, whatever else it screens at: one
   *  venue's showtimes → 1 rebuilt; a ratings-only change → 0 (a screenings row reads
   *  none of the metadata); the same row re-projected → 0. */
  "re-projecting a wide release" should "rebuild only the venues that moved, whatever the film's width" in {
    val wide = Cinema.all.distinct.filter(c => City.forCinema(c).isDefined)
    def rebuiltPerStep(venueCount: Int): Seq[(Int, Int)] = {
      val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
      val m = new RecordingReadModelProjectionMetrics()
      val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
      val cinemas = wide.take(venueCount)
      def row(rating: Double, moved: Boolean) = stored(MovieRecord(imdbRating = Some(rating), tmdbId = Some(1),
        data = cinemas.zipWithIndex.map { (cinema, i) =>
          (cinema: Source) -> venueSlot("Foo", Seq(at("2026-06-12T20:00")) ++ (if (moved && i == 0) Seq(at("2026-06-13T18:00")) else Nil))
        }.toMap))
      Seq(row(7.0, moved = false), row(7.0, moved = true), row(8.0, moved = true), row(8.0, moved = true)).map { r =>
        val (rebuiltBefore, reusedBefore) = (m.venuesRebuilt, m.venuesReused)
        projector.onMovieUpsert(r)
        (m.venuesRebuilt - rebuiltBefore, m.venuesReused - reusedBefore)
      }
    }
    Seq(50, 200).foreach { n =>
      withClue(s"a film at $n venues — (rebuilt, reused) for: first projection, one venue's showtimes, ratings only, no-op: ") {
        wide.size should be >= n
        rebuiltPerStep(n) shouldBe Seq((n, 0), (1, n - 1), (0, n), (0, n))
      }
    }
  }

  /** The equivalence the venue reuse rests on, over random edit sequences: whatever an
   *  incremental projector has been through, the read model it leaves is exactly what a
   *  FRESH projector writes for the final row — cards, screenings rows and their content.
   *  The edits cover every input a screenings row reads (showtimes, the link, a venue
   *  joining, emptying or leaving) and the ones that re-shape the row around it (a slot's
   *  title splitting it into a variant card and back, a rating change recomputing the
   *  metadata), interleaved with the prune that edits the memo from outside. */
  "an incrementally projected row" should "leave the same read model a fresh projection writes" in {
    val titles = Seq("Foo", "FOO", "Foo Bar")
    val times  = Seq("2026-06-12T20:00", "2026-06-12T22:30", "2026-06-13T18:00", "2026-06-14T11:15").map(at)
    (1 to 40).foreach { seed =>
      val random = new scala.util.Random(seed)
      val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
      val projector = new ReadModelProjector(repository, rm, rm, clock = specClock)
      var data   = venues.take(4).map(c => (c: Source) -> venueSlot("Foo", times.take(1))).toMap
      var rating = Some(7.0)
      (1 to 25).foreach { step =>
        val cinema: Source = venues(random.nextInt(venues.size))
        random.nextInt(7) match {
          case 0 => data = data.updated(cinema, venueSlot(titles(random.nextInt(titles.size)), random.shuffle(times).take(1 + random.nextInt(3))))
          case 1 => data.get(cinema).foreach(slot => data = data.updated(cinema, slot.copy(showtimes = random.shuffle(times).take(random.nextInt(4)))))
          case 2 => data.get(cinema).foreach(slot => data = data.updated(cinema, slot.copy(filmUrl = Some(s"https://x/$step"))))
          case 3 => data.get(cinema).foreach(slot => data = data.updated(cinema, slot.copy(title = Some(titles(random.nextInt(titles.size))))))
          case 4 => if (data.sizeIs > 1) data = data - cinema
          case 5 => rating = Some(random.nextInt(10).toDouble)
          case _ => projector.pruneOrphans()
        }
        val row = stored(MovieRecord(imdbRating = rating, tmdbId = Some(1), data = data))
        repository.upsert(row.title, row.year, row.record)
        projector.onMovieUpsert(row)

        val fresh = new InMemoryReadModelRepository()
        new ReadModelProjector(new InMemoryMovieRepository(normalizer = titleNormalizer), fresh, fresh, clock = specClock).onMovieUpsert(row)
        withClue(s"seed $seed, step $step: ") {
          rm.findAllMovies().sortBy(_._id)         shouldBe fresh.findAllMovies().sortBy(_._id)
          rm.findAllScreenings().sortBy(_._id)     shouldBe fresh.findAllScreenings().sortBy(_._id)
        }
      }
    }
  }

  "a film leaving a cinema" should "delete that cinema's screening document" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(stored(record(Some(8.0), Seq.empty)))  // cinema slot now has no showtimes

    rm.screeningDeletes should contain(s"$fid|poznan|Multikino Stary Browar")
  }

  // Boot-cost guard: the full reconcile is a `findAll()` + project-every-row scan.
  // Running it synchronously at `start()` stacked a second full scan onto the cache
  // hydrate + first scrape on a cold JVM (the boot CPU-credit drain). `start()` now
  // seeds state + installs the watch but defers the reconcile to the first scheduled
  // tick — so no source row is projected synchronously. (Before, the boot reconcile
  // projected "Foo" at start(), making movieUpserts size 1 and failing this.)
  "start" should "project only the ready rows that have no card, never the corpus" in {
    // A carded row is left to the diffing change-stream path; a row with no card at
    // all — the 2026-09-07 id-scheme rollout, or a restored database — is projected
    // before the first prune can delete whatever it had under an old id. In steady
    // state nothing is missing and boot projects nothing (the boot CPU-credit drain
    // a whole-corpus reconcile used to be).
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                       // Foo carded
    repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00")), tmdbId = 2))
    val before = rm.movieUpserts.size
    projector.start()
    // Bar healed (the helper's slot title is "Foo" for every row, so compare ids — a
    // key-addressed upsert files a new row under its legacy id), Foo untouched.
    rm.movieUpserts.drop(before).map(_._id) shouldBe Seq("bar|2024")
    rm.screeningUpserts.drop(before).map(_.filmId).distinct shouldBe Seq("bar|2024")
    projector.stop()

    val (steady, repository2, rm2) = fixture()
    repository2.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    steady.reconcile()
    val carded = rm2.movieUpserts.size
    steady.start()
    rm2.movieUpserts should have size carded   // nothing missing → nothing projected at boot
    steady.stop()
  }

  "the prune" should "name the rows it healed, capped per line" in {
    // "projected 3 ready row(s) missing a card or a venue" was all prod ever said, so a
    // heal could not be tied to the retirement that caused it (2026-09-23: 31 US heals in a
    // day, none attributable). The ids make each heal a lookup; the cap keeps a mass heal
    // (the 509-card id-scheme rollout) one readable line.
    val (projector, repository, _) = fixture()
    (1 to 25).foreach(i => repository.upsert(s"Film$i", Some(2024), record(Some(7.0), Seq(at("2026-06-12T20:00")), tmdbId = 100 + i)))
    val ids = repository.findAll().map(_.id.value)
    val lines = tools.LogCapture.capture(classOf[ReadModelProjector].getName)(projector.pruneOrphans())
      .map(_.getFormattedMessage).filter(_.contains("missing a card"))
    lines should have size 1
    val named = ids.filter(id => lines.head.contains(id))
    withClue(s"the heal line named ${named.size} of ${ids.size} rows: ${lines.head}") { named should have size 20 }
    lines.head should include ("(+5 more)")
  }

  // Every heal is a row the change-stream path failed to write, and on 2026-09-22 they ran
  // ~26 a day for days (a TMDB re-try making rows briefly unready) with nothing but a WARN line
  // to show for it. The count is what an alert can watch: each pass meters the rows it WROTE
  // for, by which pass it was, and a pass that found nothing missing meters nothing.
  "a heal" should "be metered by the pass that made it, one count per row it wrote for" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val m  = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00")), tmdbId = 2))
    projector.start()                                   // both uncarded → both healed at boot
    m.heals.toSeq shouldBe Seq(ReadModelProjectionMetrics.HealTrigger.Boot -> 2)

    projector.pruneOrphans()                            // nothing missing → no heal to meter
    m.heals.toSeq shouldBe Seq(ReadModelProjectionMetrics.HealTrigger.Boot -> 2)

    rm.deleteMovie("bar|2024")                          // a card the stream lost
    projector.pruneOrphans()
    m.heals.last shouldBe (ReadModelProjectionMetrics.HealTrigger.Sweep -> 1)
    projector.stop()
  }

  "reconcile" should "prune derived documents whose source film vanished" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    rm.movieUpserts should have size 1

    repository.delete("Foo", Some(2024))
    projector.reconcile()
    rm.movieDeletes     should contain(fid)
    rm.screeningDeletes should contain(s"$fid|poznan|Multikino Stary Browar")
  }

  // The change stream drops deletes, so a film re-keyed by `settle` (old `_id`
  // deleted, new one written) leaves the old read-model documents behind. They're
  // pruned only by `reconcile`. The trap: a worker that *wrote* the stale document
  // restarts before pruning it — its successor's in-memory `lastMovie` never
  // knew the document, so a `lastMovie`-based prune can't see it and the duplicate
  // card persists forever. `reconcile` must therefore diff the *actual read
  // model* against the live source, not this process's memory.
  // ── Reprojection / re-key metrics ───────────────────────────────────────────
  // The worker exposes these as kinowo_worker_readmodel_writes_total{target,op}
  // and kinowo_worker_readmodel_films_pruned_total so the rate of reprojection
  // churn — and the link-breaking film-prune events — is visible in Grafana.
  import ReadModelProjectionMetrics.{Op, ReconcileKind, Target}

  // ── Split sweep: cheap prune vs expensive full re-projection ─────────────────
  // The full re-projection (project EVERY row) was the ~1-core corpus burst that filled
  // the heap → GC thrash → credit starvation; it has been retired (the change stream
  // covers missed upserts). The remaining scheduled backstop is a cheap id-only prune
  // that removes deleted/re-keyed rows WITHOUT re-projecting anything.
  "pruneOrphans" should "prune a vanished film WITHOUT re-projecting live rows" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                     // full sweep: projects Foo
    rm.movieUpserts should have size 1
    // A live row's metadata changes: the CHEAP prune must NOT re-project it — that's
    // the whole point (no per-row projection = no CPU burst).
    repository.upsert("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
    projector.pruneOrphans()
    rm.movieUpserts should have size 1        // unchanged — prune did not reproject
    // …but a vanished film IS still pruned by the cheap sweep (the load-bearing job).
    repository.delete("Foo", Some(2024))
    projector.pruneOrphans()
    rm.movieDeletes should contain(fid)
  }

  // THE SELF-HEAL FOR A SILENT CHANGE STREAM. A cursor that is open and delivering nothing
  // reopens nothing; the prune sweep healed a MISSING card or venue but never re-projected a
  // CHANGED row, so the site served stale ratings and showtimes until a restart. The sweep
  // now re-projects every row written after the movies cursor's last delivery — bounded to
  // the rows that moved, never the corpus.
  "the prune sweep" should "re-project a row written after the movies cursor's last delivery" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val m          = new RecordingReadModelProjectionMetrics()
    val projector  = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))   // delivered at t0
    projector.reconcile()
    rm.movieUpserts should have size 1

    // The store changes after the last delivery and the cursor says nothing — the stall.
    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
    // Nothing subscribed to the cursor: nothing was promised, the sweep stays id-only.
    projector.pruneOrphans()
    rm.movieUpserts should have size 1
    m.caughtUp shouldBe empty                                              // not even metered: no cursor, no catch-up

    repository.changeStreamLiveness.watching(ChangeStreamLiveness.Movies)   // a cursor is open — and silent
    projector.pruneOrphans()

    withClue("the changed row must be re-projected by the sweep: ") {
      rm.movieUpserts should have size 2
      rm.movieUpserts.last.ratings.imdb shouldBe Some(9.9)
    }
    m.caughtUp shouldBe Seq(1)
  }

  // A cursor that stays silent does not move the delivery floor, so the sweep used to re-read
  // and re-project the SAME rows every 30 minutes — and on a cursor subscribed after the corpus
  // was written, the whole corpus, every sweep (the convergence legs' fixpoint tick, 2026-09-24).
  it should "catch a silent cursor's missed row up once, then only rows written after that" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val m          = new RecordingReadModelProjectionMetrics()
    val projector  = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    repository.changeStreamLiveness.watching(ChangeStreamLiveness.Movies)   // open, and never delivers:
    // every write below goes past the cursor, as a stalled one lets it
    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    clock.advanceSeconds(60)
    projector.pruneOrphans()
    m.caughtUp.last shouldBe 1

    clock.advanceSeconds(60)
    projector.pruneOrphans()
    withClue("nothing was written since the last catch-up, so there is nothing to catch up: ") {
      m.caughtUp.last shouldBe 0
    }

    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-12T20:00"))))
    clock.advanceSeconds(60)
    projector.pruneOrphans()
    m.caughtUp.last shouldBe 1
    rm.movieUpserts should have size 2                       // Foo once, then Bar — never Foo again
  }

  it should "re-read a caught-up row whose projection failed on the next sweep" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    var failing    = true
    val rm = new InMemoryReadModelRepository {
      override def upsertMovie(movie: ResolvedMovie): Unit =
        if (failing) throw new RuntimeException("read model unreachable") else super.upsertMovie(movie)
    }
    val m         = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    repository.changeStreamLiveness.watching(ChangeStreamLiveness.Movies)
    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    clock.advanceSeconds(60)
    projector.pruneOrphans()                                 // the write throws: Foo is still stale

    failing = false
    clock.advanceSeconds(60)
    projector.pruneOrphans()
    withClue("a row the catch-up failed to project must be read again: ") {
      rm.movieUpserts.map(_.title) should contain ("Foo")
    }
  }

  it should "not re-project a changed row the cursor did deliver" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    projector.start()                                        // subscribed: the next write is DELIVERED
    try {
      repository.upsert("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
      rm.movieUpserts should have size 2                     // projected off the stream
      val before = rm.movieUpserts.size
      projector.pruneOrphans()
      rm.movieUpserts should have size before                // the sweep read nothing to catch up
    } finally projector.stop()
  }

  // Only the PRUNE sweep is metered now — the reproject's did_work gate was retired, so
  // reconcile() (the seed/backfill path) records nothing; every sweep row is kind=prune.
  "the reconcile-sweep metric" should "meter only the prune sweep, never the reproject seed" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                                    // seed Foo — NOT metered
    m.sweeps shouldBe empty
    projector.pruneOrphans()                                 // nothing to prune (Foo live) → no-op
    m.sweeps.last shouldBe (ReconcileKind.Prune -> false)
    repository.delete("Foo", Some(2024))
    projector.pruneOrphans()                                 // prunes Foo → did work
    m.sweeps.last shouldBe (ReconcileKind.Prune -> true)
  }

  "the projector" should "meter the movie + screening upserts of a first projection" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    new ReadModelProjector(repository, rm, rm, m, clock = specClock)
      .onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    m.writes((Target.Movie, Op.Upsert))     shouldBe 1
    m.writes((Target.Screening, Op.Upsert)) shouldBe 1
    m.prunes                                shouldBe 0
  }

  "the project-duration metric" should "record one timed projectAll per ready row projected" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)

    // A ready row projected via the change-stream path → one timing recorded.
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    m.projectCalls shouldBe 1
    m.projectDurations.head should be >= 0.0

    // A row still enriching (no tmdbId → !readyToProject) is held back before
    // projectAll runs, so it must NOT be metered — the counter stays put.
    val notReady = MovieRecord(imdbRating = Some(7.0), data = Map[Source, SourceData](Multikino -> slot(Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(StoredMovieRecord.synthesised("Foo", Some(2024), notReady, services.movies.SingleCountryNormalizer.titleNormalizer))
    m.projectCalls shouldBe 1

    // A full reproject sweep projects the live row → one more timing.
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    m.projectCalls shouldBe 2
  }

  "the write-burst metric" should "time the write phase separately, and only for a row that reaches it" in {
    // Distinguishes "still computing" from "still writing" from "the event hadn't arrived
    // yet" for a slow multi-city projection — see `recordWriteBurst`'s doc.
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)

    // A ready row projected via the change-stream path reaches the write phase → timed.
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    m.writeBurstSeconds should have size 1
    m.writeBurstSeconds.head should be >= 0.0

    // A row still enriching is held back BEFORE the write phase (and before
    // recordProject too) — it must not buy a write-burst reading either.
    val notReady = MovieRecord(imdbRating = Some(7.0), data = Map[Source, SourceData](Multikino -> slot(Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(StoredMovieRecord.synthesised("Foo", Some(2024), notReady, services.movies.SingleCountryNormalizer.titleNormalizer))
    m.writeBurstSeconds should have size 1

    // A full reproject sweep reaches the write phase for the live row → one more timing.
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    m.writeBurstSeconds should have size 2
  }

  it should "attribute CPU from the thread CPU clock, not from wall-clock" in {
    // The panel that stacks projection against process CPU is only meaningful if this
    // number is CPU. Wall-clock is not: concurrent projections make it sum past one
    // core-second per second, and steal on a throttled box inflates it further — it
    // read 45.9cc against an 18.0cc process total on kinowo-worker-uk (2026-07-28).
    // Pin the clock to a known step so the recorded cost is exact, and assert it came
    // from THAT clock rather than from however long the projection happened to take.
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m,
      cpuClock = new SteppingCpuClock(stepNanos = 250000000L), clock = specClock) // 0.25s of CPU per reading

    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))

    m.projectCpuSeconds.head shouldBe 0.25
    // …and the wall-clock reading is still its own, genuinely-measured number. An
    // in-memory projection takes far less than the stubbed 0.25s of "CPU", so the two
    // series cannot be the same value — which is exactly the bug this guards.
    m.projectDurations.head should be >= 0.0
    m.projectDurations.head should be < 0.25
  }

  "a re-key that prunes the old film in reconcile" should "meter a film prune + its document deletes" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                       // projects the film
    repository.delete("Foo", Some(2024))         // source row gone (re-keyed/removed)
    projector.reconcile()                       // prunes its derived documents
    m.prunes                            shouldBe 1
    m.writes((Target.Movie, Op.Delete)) shouldBe 1
  }

  "reconcile after a restart" should "prune a stale film a prior process left in the read model" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm   = new InMemoryReadModelRepository()
    def yearKey(y: Int) = s"${titleNormalizer.sanitize("Foo")}|$y"
    // A film whose reported year was 2025 when an earlier projector ran.
    def recordYear(y: Int) =
      MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](Multikino ->
        SourceData(title = Some("Foo"), releaseYear = Some(y),
          filmUrl = Some("https://mk/foo"), showtimes = Seq(at("2026-06-12T20:00")))))

    val p1 = new ReadModelProjector(repository, rm, rm, clock = specClock)
    repository.upsert("Foo", Some(2025), recordYear(2025))
    p1.reconcile()
    rm.findAllMovies().map(_._id) should contain(yearKey(2025))
    p1.stop()  // the worker dies, taking its in-memory state with it

    // `settle` re-keys the source row onto the (now resolved) year — old gone,
    // new live.
    repository.delete("Foo", Some(2025))
    repository.upsert("Foo", Some(2026), recordYear(2026))

    // A fresh projector boots with an empty `lastMovie` and reconciles.
    val p2 = new ReadModelProjector(repository, rm, rm, clock = specClock)
    rm.movieDeletes.clear(); rm.screeningDeletes.clear()
    p2.reconcile()

    rm.findAllMovies().map(_._id)                  should contain only yearKey(2026)
    rm.findAllScreenings().map(_.filmId).distinct  should contain only yearKey(2026)
    rm.movieDeletes should contain(yearKey(2025))
    p2.stop()
  }

  // The periodic full reproject was retired: the resume-token change stream now catches
  // the upserts it used to, and its ~1-core whole-corpus burst was the CPU-credit drain.
  // `start()` must schedule ONLY the cheap orphan prune — never the reproject. Captured
  // via a fake scheduler: a live source row absent from the read model would be PROJECTED
  // by a scheduled reproject (movieUpserts size 1), but the prune re-projects nothing, so
  // running every scheduled task leaves the read model untouched.
  "start" should "project a split row one of whose variant cards is missing" in {
    // A row screened under two shown titles fans out into two cards. When only one
    // of them exists at boot — a restored database, or the id scheme moving under the
    // variant suffix — the row is healed whole: `filmIds` is asked per row, and a row
    // is healed when ANY of its ids has no card. The first version healed only rows
    // with NO card at all, and on 2026-09-07 that left every decorated listing (a
    // "35 lat po premierze …" banner, a "przedpremiera" screening) whose variant card
    // the prune had removed unserved for hours while the plain card survived:
    // Warszawa was 45 films short four hours after the rollout.
    val (projector, repository, rm) = fixture()
    val twoTitles = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino  -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Фу"), showtimes = Seq(at("2026-06-13T20:00")))))
    repository.upsert("Foo", Some(2024), twoTitles)
    val row = repository.findAll().head
    val ids = ReadModelProjection.filmIds(row, titleNormalizer)
    ids should have size 2

    projector.start()                                       // nothing carded → both cards written
    rm.movieUpserts.map(_._id).toSet shouldBe ids.toSet
    projector.stop()

    // One variant card gone: the row is projected whole, and the missing card comes back.
    val (again, _, _) = fixture()
    val partial = new InMemoryReadModelRepository()
    val healer  = new ReadModelProjector(repository, partial, partial, clock = specClock)
    partial.upsertMovie(rm.movieUpserts.find(_._id == ids.head).get)
    healer.start()
    partial.findAllMovieIds().toSet shouldBe ids.toSet
    partial.findAllScreenings().map(_.filmId).toSet shouldBe ids.toSet
    healer.stop(); again.stop()
  }

  // The same gap inside the scheduled sweep: the prune that removed a variant card as
  // an orphan under its old id must put it back under its new one in the SAME pass,
  // however healthy the row's plain card is — the change stream only re-creates the
  // rows a scrape happens to rewrite.
  "the orphan prune" should "restore a split row's missing variant card and its screenings" in {
    val (projector, repository, rm) = fixture()
    val twoTitles = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino  -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("35 lat po premierze: Foo"), showtimes = Seq(at("2026-06-13T20:00")))))
    repository.upsert("Foo", Some(2024), twoTitles)
    val row     = repository.findAll().head
    val ids     = ReadModelProjection.filmIds(row, titleNormalizer)
    val variant = ids.find(_.contains("~")).get
    projector.onMovieUpsert(row)
    rm.findAllMovieIds().toSet shouldBe ids.toSet
    rm.deleteMovie(variant)
    rm.findAllScreenings().filter(_.filmId == variant).foreach(s => rm.deleteScreening(s._id))
    rm.findAllMovieIds().toSet shouldBe (ids.toSet - variant)

    projector.pruneOrphans()

    rm.findAllMovieIds().toSet shouldBe ids.toSet
    rm.findAllScreenings().map(_.filmId).toSet shouldBe ids.toSet
    projector.stop()
  }

  // A venue the source lists but the read model lacks: a `movie_slots` row written after
  // the film's last projection (Palace Cinema Kent, 2026-09-07) — the projection needs
  // the slot to emit that venue, and nothing touched the row again. Both heals ask for
  // every screenings row a row's slots project to, not only for its cards.
  private def screenedInTwoCities(): (ReadModelProjector, InMemoryMovieRepository, InMemoryReadModelRepository, StoredMovieRecord, Seq[String]) = {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-13T20:00"))))))
    val row = repository.findAll().head
    val ids = ReadModelProjection.screeningIds(row, titleNormalizer)
    ids should have size 2
    (projector, repository, rm, row, ids)
  }

  "the orphan prune" should "restore a venue's screenings row the read model lacks while the card is healthy" in {
    val (projector, _, rm, row, ids) = screenedInTwoCities()
    projector.onMovieUpsert(row)
    rm.findAllScreenings().map(_._id).toSet shouldBe ids.toSet
    rm.deleteScreening(ids.last)                      // the venue's row vanishes; the card stays

    projector.pruneOrphans()

    rm.findAllScreenings().map(_._id).toSet shouldBe ids.toSet
    projector.stop()
  }

  "start" should "project a row one of whose venues has no screenings row" in {
    val (projector, repository, rm, row, ids) = screenedInTwoCities()
    projector.onMovieUpsert(row); projector.stop()
    val partial = new InMemoryReadModelRepository()
    rm.findAllMovies().foreach(partial.upsertMovie)
    rm.findAllScreenings().filter(_._id == ids.head).foreach(partial.upsertScreening)   // one venue short
    val healer = new ReadModelProjector(repository, partial, partial, clock = specClock)

    healer.start()

    partial.findAllScreenings().map(_._id).toSet shouldBe ids.toSet
    healer.stop()
  }

  // THE HEAL MUST CONVERGE. Its venue check reads slots only, so a slot with no showtimes
  // is indistinguishable from a venue whose row is missing and it asks about both — which
  // made every sweep re-project the same rows for ever (PL: ~333 of them, every 30 minutes,
  // 2026-09-08). A sweep that finds nothing to write must not ask again at the same row state.
  "the orphan prune" should "stop re-projecting a row whose only absent venue is a spent slot" in {
    // The SPLIT storage production runs, so the sweep's scan carries slots without showtimes.
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val m  = new RecordingReadModelProjectionMetrics()
    // One venue screening it, one venue whose showtimes are all gone: the second projects no
    // screenings row at all, so its id reads as absent on every slots-only pass.
    repository.upsert("Foo", Some(2024), MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = Nil))))
    val sweeper = new ReadModelProjector(repository, rm, rm, m, clock = clockSkippingSliceOf(repository.findAll().head.id))
    sweeper.onMovieUpsert(repository.findAll().head)

    sweeper.pruneOrphans()                      // the first sweep may legitimately look
    val looked = m.projectCalls
    sweeper.pruneOrphans()
    sweeper.pruneOrphans()

    // Counting PROJECTIONS, not writes: a heal that writes nothing is exactly the symptom,
    // so a write count cannot see the loop at all.
    withClue(s"the heal re-projected a row it had nothing to write for (${m.projectCalls - looked} times): ") {
      m.projectCalls shouldBe looked
    }
    sweeper.stop()
  }

  // …NOR AFTER A PROJECTION THAT CHANGED NOTHING. Every change-stream event for the row — a
  // showtime moving at its OTHER venue, the catch-up re-projecting a row the cursor was late
  // with — used to wipe the note, so the next sweep asked about the spent slot all over again.
  // Found by the convergence legs' fixpoint pass: 13 Polish rows (Janosik's spent slots)
  // re-projected on a sweep over a corpus nothing had written to.
  it should "keep its note about a spent slot across a projection that did not write that venue" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val m  = new RecordingReadModelProjectionMetrics()
    def film(times: Seq[Showtime]) = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = times),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = Nil)))
    repository.upsert("Foo", Some(2024), film(Seq(at("2026-06-12T20:00"))))
    val sweeper = new ReadModelProjector(repository, rm, rm, m, clock = clockSkippingSliceOf(repository.findAll().head.id))
    sweeper.onMovieUpsert(repository.findAll().head)
    sweeper.pruneOrphans()                      // the first sweep looks, and notes the phantom

    // The live venue's showtimes move: a projection, but nothing about the spent venue changed.
    repository.upsert("Foo", Some(2024), film(Seq(at("2026-06-12T20:00"), at("2026-06-13T20:00"))))
    sweeper.onMovieUpsert(repository.findAll().head)
    val looked = m.projectCalls
    sweeper.pruneOrphans()

    withClue(s"the heal re-projected the row for its spent slot again (${m.projectCalls - looked} times): ") {
      m.projectCalls shouldBe looked
    }
    sweeper.stop()
  }

  it should "heal a venue it had noted as spent once a projection has written it and it goes missing" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val sweeper = new ReadModelProjector(repository, rm, rm, new RecordingReadModelProjectionMetrics(), clock = specClock)
    def film(muranow: Seq[Showtime]) = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = muranow)))
    repository.upsert("Foo", Some(2024), film(Nil))
    sweeper.onMovieUpsert(repository.findAll().head)
    sweeper.pruneOrphans()                      // Muranów noted as a phantom

    // Muranów screens it after all — the projection writes its row, so the note is spent.
    repository.upsert("Foo", Some(2024), film(Seq(at("2026-06-14T20:00"))))
    sweeper.onMovieUpsert(repository.findAll().head)
    val muranow = rm.findAllScreenings().filter(_.cinema == KinoMuranow.displayName)
    muranow should not be empty
    muranow.foreach(s => rm.deleteScreening(s._id))   // …and then the read model loses it

    sweeper.pruneOrphans()
    withClue("a venue that has screenings and went missing must be healed, whatever was noted before: ") {
      rm.findAllScreenings().exists(_.cinema == KinoMuranow.displayName) shouldBe true
    }
    sweeper.stop()
  }

  // …AND IT MUST NOT ASK TWICE PER BOOT. The boot check asks the same slots-only question and
  // found the same spent slots, but kept no note of the answer, so the first sweep five minutes
  // later re-projected every one of them again: ~260 rows at boot and ~260-300 more at the first
  // sweep on every PL worker start (prod, 2026-09-19 and 09-24). Six rollouts in an hour on
  // 2026-09-19 put 0.78/s of projections no change stream asked for on the books, and
  // ReadModelProjectionTriggerUnaccounted fired.
  "the first prune after a boot" should "not re-project a spent-slot row the boot check already found nothing to write for" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    repository.upsert("Foo", Some(2024), MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = Nil))))
    val previous = new ReadModelProjector(repository, rm, rm, new RecordingReadModelProjectionMetrics(), clock = specClock)
    previous.onMovieUpsert(repository.findAll().head)
    previous.stop()
    val m      = new RecordingReadModelProjectionMetrics()
    val booted = new ReadModelProjector(repository, rm, rm, m, scheduler = new CapturingScheduler,
                                        clock = clockSkippingSliceOf(repository.findAll().head.id))

    booted.start()                              // the boot check may legitimately look once
    val looked = m.projectCalls
    booted.pruneOrphans()

    withClue(s"the first sweep re-projected a row the boot check had nothing to write for (${m.projectCalls - looked} times): ") {
      m.projectCalls shouldBe looked
    }
    booted.stop()
  }

  // The look itself is still a projection no change-stream event asked for, and
  // ReadModelProjectionTriggerUnaccounted counts every projection against the cursors' events.
  // Metered by pass, written or not, so the rule can tell a heal from an unmetered trigger.
  "a heal pass" should "meter every row it re-projected, including a look that wrote nothing" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    // Two rows each with a spent slot, so both read as short a venue; only Foo's card is gone.
    Seq("Foo" -> 1, "Bar" -> 2).foreach { case (title, tmdbId) =>
      repository.upsert(title, Some(2024), MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        Multikino   -> SourceData(title = Some(title), showtimes = Seq(at("2026-06-12T20:00"))),
        KinoMuranow -> SourceData(title = Some(title), showtimes = Nil))))
    }
    val previous = new ReadModelProjector(repository, rm, rm, new RecordingReadModelProjectionMetrics(), clock = specClock)
    repository.findAll().foreach(previous.onMovieUpsert)
    previous.stop()
    rm.findAllMovies().filter(_.title == "Foo").foreach(c => rm.deleteMovie(c._id))
    val m      = new RecordingReadModelProjectionMetrics()
    val booted = new ReadModelProjector(repository, rm, rm, m, scheduler = new CapturingScheduler, clock = specClock)

    booted.start()

    m.healChecks.toSeq shouldBe Seq(ReadModelProjectionMetrics.HealTrigger.Boot -> 2)   // both looked at
    m.heals.toSeq      shouldBe Seq(ReadModelProjectionMetrics.HealTrigger.Boot -> 1)   // one written for
    booted.stop()
  }

  // The meter exists to be SUBTRACTED from readmodel_project_calls, so it must count exactly the
  // projections a heal made -- not the rows the slots-only scan named. A row gone by the time the
  // heal reads it whole (deleted or re-keyed in between) is never projected, and counting it made
  // ReadModelProjectionTriggerUnaccounted subtract projections that never happened.
  it should "meter only the rows it actually projected, not one that vanished before it was read" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer) {
      override def findByIdChecked(id: services.movies.FilmId): (Option[StoredMovieRecord], Boolean) = {
        val found = super.findByIdChecked(id)
        if (found._1.exists(_.title == "Bar")) (None, true) else found
      }
    }
    val rm = new InMemoryReadModelRepository()
    val m  = new RecordingReadModelProjectionMetrics()
    Seq("Foo" -> 1, "Bar" -> 2).foreach { case (title, tmdbId) =>
      repository.upsert(title, Some(2024), MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some(title), showtimes = Seq(at("2026-06-12T20:00"))))))
    }
    val booted = new ReadModelProjector(repository, rm, rm, m, clock = specClock)

    booted.start()                                      // both uncarded; Bar vanishes before its read

    m.healChecks.toSeq shouldBe Seq(ReadModelProjectionMetrics.HealTrigger.Boot -> 1)
    m.healChecks.map(_._2).sum shouldBe m.projectCalls
    booted.stop()
  }

  // A HEAL IS A REPAIR OF WHAT WAS MISSING, NOT ANY WRITE. The heal reads its row whole, so it
  // also writes every change the change stream has not applied YET — and while the sweep holds
  // the projection lock the stream CANNOT apply, so a row with a spent slot (asked about on every
  // sweep after it changes) and an event queued behind the sweep was counted as healed. It is the
  // one path that fits what the US sweep "healed" on 2026-09-23 23:33Z and 00:03Z (Your Mother
  // Your Mother Your Mother, eight minutes after a scrape-prune at the Aero, Whalefall,
  // ff17516a55e08450): no card retired, no write failed, no restart, no stream error — and
  // ReadModelHealsRecurring paged for it.
  "the orphan prune" should "not count as a heal a row whose absent venue was a spent slot, even when it wrote a pending change" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val m  = new RecordingReadModelProjectionMetrics()
    val sweeper = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    def foo(showtime: String) = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at(showtime))),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = Nil)))
    repository.upsert("Foo", Some(2024), foo("2026-06-12T20:00"))
    sweeper.onMovieUpsert(repository.findAll().head)
    // A showtime change the stream has delivered but not applied: queued behind the sweep.
    repository.upsert("Foo", Some(2024), foo("2026-06-13T18:00"))

    sweeper.pruneOrphans()

    withClue("the sweep may apply the queued change, but it repaired nothing that was missing: ") {
      m.heals.filter(_._2 > 0) shouldBe empty
    }
    rm.findAllScreenings().flatMap(_.showtimes).map(_.dateTime.toString) shouldBe Seq("2026-06-13T18:00")
    sweeper.stop()
  }

  // A HEAL IS A MISS THE STREAM DID NOT REPAIR ITSELF. The case above left out the other half:
  // a scrape that lands a NEW venue (or a new row) seconds before the sweep, or during it, puts
  // exactly the id the sweep finds absent into an event queued behind the sweep's lock — and
  // the heal writing that id counted as a repair. Every sweep heal from 2026-09-24 21:08Z to
  // 09-25 02:37Z was that: US 21:08 (Mandaadi + Mirzapur, four Cinemark/Regal scrapes 1-5 s
  // before the sweep), 23:08 (Other Mommy, NCG Gallatin at the sweep's first second), 01:08
  // (The Social Reckoning + f10401cf8eb0f536, GQT Wabash Landing 1 s before), and UK 02:37
  // (The Social Reckoning + fc0b3a8fb0547b69, Showcase de Lux Leeds landing mid-sweep) — no
  // card retired, no re-read failed, and ReadModelHealsRecurring paged twice. Only a row the
  // stream does NOT apply once the sweep lets go is a miss.
  "the orphan prune" should "not count as a heal an absence the change stream applies once the sweep lets go" in {
    val (_, repository, rm) = fixture()
    val m = new RecordingReadModelProjectionMetrics()
    def foo(venues: (Source, String)*) = MovieRecord(tmdbId = Some(1), data = venues.map { case (cinema, showtime) =>
      cinema -> SourceData(title = Some("Foo"), showtimes = Seq(at(showtime))) }.toMap)
    repository.upsert("Foo", Some(2024), foo(Multikino -> "2026-06-12T20:00"))
    repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00")), tmdbId = 2))
    // The stream's apply thread: what it had queued when the sweep took the lock, run once it is free.
    val queued = scala.collection.mutable.Buffer.empty[StoredMovieRecord]
    lazy val projector: ReadModelProjector =
      new ReadModelProjector(repository, rm, rm, m, awaitStreamApplied = _ => queued.foreach(projector.onMovieUpsert), clock = specClock)
    repository.findAll().foreach(projector.onMovieUpsert)
    // A scrape lands Foo at a second venue; its event is delivered, and waits behind the sweep.
    repository.upsert("Foo", Some(2024), foo(Multikino -> "2026-06-12T20:00", KinoMuranow -> "2026-06-13T18:00"))
    queued ++= repository.findAll().filter(_.id.value == fid)
    rm.deleteMovie("bar|2024")                           // and a card the stream really did lose

    val lines = tools.LogCapture.capture(classOf[ReadModelProjector].getName)(projector.pruneOrphans())
      .map(_.getFormattedMessage).filter(_.contains("missing a card"))

    rm.findAllScreenings().map(_._id) should contain allElementsOf ReadModelProjection.screeningIds(queued.head, titleNormalizer)
    rm.findAllMovies().map(_._id) should contain ("bar|2024")
    withClue("only the row the stream never applied is a heal: ") {
      m.heals.toSeq shouldBe Seq(ReadModelProjectionMetrics.HealTrigger.Sweep -> 1)
    }
    lines should have size 1
    lines.head should include ("bar|2024")
    lines.head should not include (queued.head.id.value)
    projector.stop()
  }

  it should "ask again once the row itself changes" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val sweeper = new ReadModelProjector(repository, rm, rm, new RecordingReadModelProjectionMetrics(), clock = specClock)
    repository.upsert("Foo", Some(2024), MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = Nil))))
    sweeper.onMovieUpsert(repository.findAll().head)
    sweeper.pruneOrphans()
    rm.findAllMovies().foreach(c => rm.deleteMovie(c._id))     // the card goes missing after that
    val before = rm.movieUpserts.size

    sweeper.pruneOrphans()

    withClue("a genuinely missing card must still be healed: ") { rm.movieUpserts.size should be > before }
    sweeper.stop()
  }

  // A ROW THAT EXISTS AND IS WRONG is what every id-only backstop misses: the prune removes
  // a card whose row is gone, the heal writes one that is missing, and neither looks at what
  // a row SAYS. Three UK films held August showtimes into September because of it. The
  // rolling content check re-projects one slice of the corpus per sweep, so a drifted row is
  // corrected within a day even though nothing about it ever changes again.
  "the orphan prune" should "rewrite a stored projection whose source moved while nothing was listening" in {
    // THE PRODUCTION SHAPE, not a tampered read model: the source row changes and NO event is
    // delivered (`putEmbeddedOutOfBand` writes without ringing the change stream, which is what
    // a lost event leaves behind), and only THEN does a projector start. That ordering is the
    // whole difficulty — the row's `updatedAt` is older than the process, so the silent-cursor
    // catch-up cannot reach it either, and nothing about the film will ever change again.
    // Troy and 2046 at the Prince Charles and Glastonbury at the Southsea sat like this from
    // 2026-08-29 to 2026-09-08, serving August showtimes to real users.
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.onMovieUpsert(repository.findAll().head)
    rm.findAllScreenings().flatMap(_.showtimes).map(_.dateTime.toString) shouldBe Seq("2026-06-12T20:00")
    projector.stop()

    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(9.9), Seq(at("2026-07-20T18:00"))))
    // …and then the stream carries on delivering OTHER films, so the drifted row is older than
    // the cursor's last delivered event. This is what makes the case unreachable by every other
    // backstop and is exactly the production shape: the loss happened on 2026-08-29, the cursor
    // has delivered plenty since, and the silent-cursor catch-up only re-reads rows written
    // AFTER the last delivery. Without it the catch-up repairs the row and this spec would be
    // testing that instead.
    repository.upsert("Bar", Some(2024), record(Some(6.0), Seq(at("2026-06-14T20:00")), tmdbId = 2))

    val m       = new RecordingReadModelProjectionMetrics()
    val checker = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    checker.start()          // seeds its memo from the stale read model, exactly as a restart does
    withClue("the boot heal must not see this: every card and venue id is present, only the CONTENT is wrong: ") {
      rm.findAllScreenings().filter(_.filmId.startsWith("foo")).flatMap(_.showtimes).map(_.dateTime.toString) shouldBe Seq("2026-06-12T20:00")
    }

    (1 to 48).foreach(_ => checker.pruneOrphans())   // one slice per sweep; 48 covers the corpus

    withClue("the drifted screenings must be rewritten from the source: ") {
      rm.findAllScreenings().filter(_.filmId.startsWith("foo")).flatMap(_.showtimes).map(_.dateTime.toString) shouldBe Seq("2026-07-20T18:00")
    }
    rm.findAllMovies().find(_._id.startsWith("foo")).flatMap(_.ratings.imdb) shouldBe Some(9.9)
    m.driftWrites.sum should be > 0
    checker.stop()
  }

  // The slices must continue across restarts. A sweep counter that starts at zero in every
  // process checks slice 0 again after each deploy, so on a day of hourly deploys (2026-09-24:
  // six pl boots) the rest of the corpus was never re-checked — the bilety24 SVG posters a
  // fix had already stopped deriving stayed in the read model.
  it should "reach every slice across restarts, not re-check the first one after each boot" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.onMovieUpsert(repository.findAll().head)
    projector.stop()
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(9.9), Seq(at("2026-07-20T18:00"))))
    repository.upsert("Bar", Some(2024), record(Some(6.0), Seq(at("2026-06-14T20:00"))))

    val clock = new tools.MutableClock(java.time.Instant.parse("2026-09-24T00:00:00Z"))
    (1 to 48).foreach { _ =>
      val booted = new ReadModelProjector(repository, rm, rm, clock = clock)
      booted.start()
      booted.pruneOrphans()        // one sweep, then the next deploy
      booted.stop()
      clock.advance(java.time.Duration.ofMinutes(30))
    }

    rm.findAllMovies().find(_._id.startsWith("foo")).flatMap(_.ratings.imdb) shouldBe Some(9.9)
  }

  // The 2026-09-07 ReadModelFilmPruneBurst, replayed: live rows whose cards sit under ids
  // the source no longer produces (an id scheme change; a restored database), and then
  // the scheduled prune — with no boot heal in between. The prune must leave every live
  // film with a card, whatever id its old card carried.
  "the orphan prune" should "never leave a live film without a card, even when every card it has is stale" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00")), tmdbId = 2))
    val rows = repository.findAll()
    // Cards under the OLD scheme's ids for both rows, and nothing under the new ones.
    rows.foreach { row =>
      val (card, screenings) = ReadModelProjection.project(row, titleNormalizer)
      val old = s"${row.id.value}-old"
      rm.upsertMovie(card.copy(_id = old))
      screenings.foreach(s => rm.upsertScreening(s.copy(_id = s"$old|${s.city}|${s.cinema}", filmId = old)))
    }
    rm.findAllMovieIds().toSet shouldBe rows.map(r => s"${r.id.value}-old").toSet

    projector.pruneOrphans()                       // straight to the prune: no start(), no boot heal

    rm.findAllMovieIds().toSet shouldBe rows.flatMap(ReadModelProjection.filmIds(_, titleNormalizer)).toSet
    rm.findAllScreenings().map(_.filmId).toSet shouldBe rows.map(_.id.value).toSet
    projector.stop()
  }

  "a card read that did not complete" should "heal nothing and prune nothing" in {
    // "No cards" and "could not read the cards" are different facts: on the second, a
    // heal that trusted the empty answer would re-project every row (a boot burst), and
    // a prune would delete every card. Both stand down.
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val blind = new InMemoryReadModelRepository() {
      override def findAllMovieIdsChecked(): (Seq[String], Boolean) = (Seq.empty, false)
      override def findAllMovieIds(): Seq[String] = Seq.empty
    }
    val projector = new ReadModelProjector(repository, blind, blind, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    blind.upsertMovie(ReadModelProjection.project(repository.findAll().head, titleNormalizer)._1)
    val before = blind.movieUpserts.size

    projector.start()                                // boot heal: cannot see the cards → nothing
    projector.pruneOrphans()                         // sweep heal + prune: the same
    blind.movieUpserts should have size before
    blind.movieDeletes shouldBe empty
    projector.stop()
  }

  // …AND THE SAME FOR THE SCREENINGS. `findAllScreeningRefs` returns EMPTY when its keyset scan
  // gives up (MongoReadModelRepository.pagedIds), and the venue heals took that as "no venue has
  // a row": every ready row healed, each venue's memo dropped and its row rewritten — the whole
  // `web_screenings` collection (113k rows in the US) in one sweep, each counted as a heal.
  "a screenings read that did not complete" should "heal no venue" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val blind = new InMemoryReadModelRepository() {
      override def findAllScreeningRefsChecked(): (Seq[ScreeningRef], Boolean) = (Seq.empty, false)
      override def findAllScreeningRefs(): Seq[ScreeningRef] = Seq.empty
    }
    val m = new RecordingReadModelProjectionMetrics()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    val previous = new ReadModelProjector(repository, blind, blind, clock = specClock)
    previous.onMovieUpsert(repository.findAll().head)
    previous.stop()
    val before = blind.screeningUpserts.size
    val projector = new ReadModelProjector(repository, blind, blind, m, scheduler = new CapturingScheduler, clock = specClock)

    projector.start()                                // boot heal: cannot see the venues → nothing
    projector.pruneOrphans()                         // sweep heal: the same

    blind.screeningUpserts should have size before
    m.heals.filter(_._2 > 0) shouldBe empty
    projector.stop()
  }

  "a screenings write that throws" should "leave the card un-remembered so the next projection retries it" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val flaky = new InMemoryReadModelRepository() {
      var failOnce = true
      override def upsertScreening(s: CityScreening): Unit =
        if (failOnce) { failOnce = false; throw new RuntimeException("simulated screenings write failure") }
        else super.upsertScreening(s)
    }
    val projector = new ReadModelProjector(repository, flaky, flaky, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    val row = repository.findAll().head

    intercept[RuntimeException](projector.onMovieUpsert(row))
    flaky.screeningUpserts shouldBe empty

    projector.onMovieUpsert(row)                     // the same row again: the card's hash was not remembered
    flaky.screeningUpserts should not be empty
    projector.stop()
  }

  // A read-model write THROWS on failure now (so the projector forgets what it did not write),
  // and the sweep's prune deletes one card at a time. One refused delete must cost that card
  // alone: thrown out of the loop, it skipped every other orphan, the orphan screenings, the
  // content slice, the silent-stream catch-up and every metric the sweep records.
  "a card delete that throws in the prune" should "not stop the rest of the sweep" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val flaky = new InMemoryReadModelRepository() {
      override def deleteMovie(id: String): Unit =
        if (id == "bar|2024") throw new RuntimeException("simulated read-model delete failure")
        else super.deleteMovie(id)
    }
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, flaky, flaky, m, clock = specClock)
    repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00")), tmdbId = 2))
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    repository.delete("Bar", Some(2024))
    repository.delete("Foo", Some(2024))

    noException should be thrownBy projector.pruneOrphans()
    flaky.findAllMovieIds() shouldBe Seq("bar|2024")          // only the refused one is left
    m.sweeps.last shouldBe (ReconcileKind.Prune -> true)
    projector.stop()
  }

  // The card is deleted first; a screenings delete that then throws must not leave the memo
  // saying the card is still written — or the row coming back unchanged skips the card for good.
  "a card retired while a screenings delete throws" should "be written again when its row comes back" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    class FailingScreeningDeletes extends InMemoryReadModelRepository {
      @volatile var failing = false
      override def deleteScreening(id: String): Unit =
        if (failing) throw new RuntimeException("simulated screenings delete failure") else super.deleteScreening(id)
    }
    val flaky = new FailingScreeningDeletes
    val projector = new ReadModelProjector(repository, flaky, flaky, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    val row = repository.findAll().head
    projector.onMovieUpsert(row)
    flaky.failing = true
    intercept[RuntimeException](projector.onMovieDelete(row.id))
    flaky.findAllMovieIds() shouldBe empty                    // the card itself went
    flaky.failing = false

    projector.onMovieUpsert(row)                              // the same row, unchanged
    flaky.findAllMovieIds() shouldBe Seq(fid)
    projector.stop()
  }

  "start" should "schedule the orphan prune and the derivation pass's tick but NOT a periodic reproject" in {
    val fakeScheduler = new CapturingScheduler
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val projector = new ReadModelProjector(repository, rm, rm, scheduler = fakeScheduler, clock = specClock)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.start()
    val healedAtBoot = rm.movieUpserts.size       // Foo had no card, so the boot heal wrote it

    fakeScheduler.scheduled should have size 2   // the prune and the derivation tick, never the reproject
    fakeScheduler.runAll()                        // a scheduled reproject WOULD rewrite Foo here
    rm.movieUpserts     should have size healedAtBoot   // prune re-projects nothing
    projector.stop()
  }

  // ── One display-title partition per row per sweep ─────────────────────────────

  /** Counts how often ONE slot title is sanitized. The display-title partition
   *  (`ReadModelProjection.variants`) is the only projection step that sanitizes a
   *  slot's reported title, so the count is the number of times a row was partitioned —
   *  immune to the sanitizing the repository does on the row's own key. */
  private class SlotTitleCountingNormalizer(slotTitle: String) extends services.movies.TitleNormalizer(titleNormalizer.rules) {
    var hits = 0
    override def sanitize(title: String): String = {
      if (title == slotTitle) hits += 1
      super.sanitize(title)
    }
  }
  private def hitsOf(counting: SlotTitleCountingNormalizer)(work: => Any): Int = {
    val before = counting.hits
    work
    counting.hits - before
  }

  "a sweep" should "partition a row's slots by display title once, not once per question it asks" in {
    // The prune asks a row for its card ids AND its screening ids; the reproject asks
    // for its card ids AND projects it. Each answer used to re-partition the row's
    // slots by sanitized title, so one row was partitioned twice per sweep.
    val banner    = "35 lat po premierze: Foo"
    val counting  = new SlotTitleCountingNormalizer(banner)
    val repository = new InMemoryMovieRepository(normalizer = counting)
    val rm        = new InMemoryReadModelRepository()
    repository.upsert("Foo", Some(2024), MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some(banner), showtimes = Seq(at("2026-06-13T20:00"))))))
    val row = repository.findAll().head
    val projector = new ReadModelProjector(repository, rm, rm, clock = clockSkippingSliceOf(row.id))
    val onePartition  = hitsOf(counting)(ReadModelProjection.filmIds(row, counting))
    val oneProjection = hitsOf(counting)(ReadModelProjection.projectAll(row, counting))
    onePartition should be > 0                                              // the counter sees the partition at all

    // The scan itself re-derives each row's display title (`StoredMovieRecord.fromStorage`)
    // — the repository's cost, measured apart so a sweep is charged only for what the
    // projection asks on top of it.
    val slotsScan = hitsOf(counting)(repository.foreachRecordWithSlots(_ => ()))
    val wholeScan = hitsOf(counting)(repository.foreachRecord(_ => ()))

    hitsOf(counting)(projector.onMovieUpsert(row)) shouldBe oneProjection               // the change stream: project once
    hitsOf(counting)(projector.pruneOrphans())     shouldBe slotsScan + onePartition    // card ids + venue ids: one partition
    hitsOf(counting)(projector.reconcile())        shouldBe wholeScan + onePartition    // card ids + a metadata-reusing projection: one partition
    projector.stop()
  }

}
