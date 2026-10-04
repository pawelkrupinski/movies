package services.readmodel

import tools.SpecTimeouts

import models.{City, CityScreening, ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.Duration

/**
 * Unit cover for the read cache's read surface. `allScreenings()` exists for the
 * dev `/debug/readmodel` dump, which needs every cached screening across cities
 * (the per-city `screeningsForCity` is the request-time read key, not a dump).
 *
 * The `backstopTick` cases pin the CPU-saving contract: the periodic backstop
 * must NOT re-read the whole corpus while the change streams keep the model
 * current (that decode burst is what stalled requests on the single-vCPU web
 * box), yet must still fall back to a full reload when a stream dies or a count
 * drifts.
 */
class WebReadModelSpec extends AnyFlatSpec with Matchers {

  // `be >` / `sorted` on the validator stamps.
  private implicit val instantOrdering: Ordering[java.time.Instant] = _.compareTo(_)

  private def ratings = ResolvedRatings(None, None, None, "", None, "", None, "")
  private def movie(id: String) =
    ResolvedMovie(id, id, None, None, Nil, None, None, Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)
  private def screening(id: String, film: String, city: String) =
    CityScreening(id, film, city, "Cinema " + id, None, Nil)

  "allScreenings" should "return every cached screening flattened across all city buckets" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    repository.upsertScreening(screening("s2", "belle|2021", "krakow"))
    repository.upsertScreening(screening("s3", "belle|2021", "wroclaw"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    rm.allScreenings().map(_._id) should contain theSameElementsAs Seq("s1", "s2", "s3")
    // The per-city read key still partitions them — the dump is the union.
    rm.screeningsForCity("wroclaw").map(_._id) should contain theSameElementsAs Seq("s1", "s3")
  }

  it should "be empty when the cache holds no screenings" in {
    new WebReadModel(new InMemoryReadModelRepository, clock = _root_.tools.SpecClock.Pinned).allScreenings() shouldBe empty
  }

  // ── A renamed city keeps serving while the projection catches up ────────────
  //
  // `CityScreening._id` is `filmId|city|cinema` and `city` is the SLUG, so the
  // moment a city's slug changes every row already projected for it is filed
  // under a name nothing asks for. The rows are rewritten one film at a time as
  // each is projected again — a whole scrape cadence, 14h in the US — and
  // without this the city serves a near-empty page for that entire window. It
  // is what `/san-francisco/` → `/san-francisco-bay-area/` did: 6 films where
  // the metro has ~200.

  "A city that changed slug" should "serve the rows still projected under its former slug" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("dune|2021"))
    repository.upsertScreening(screening("s1", "dune|2021", "san-francisco"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    rm.screeningsForCity("san-francisco-bay-area").map(_._id) shouldBe Seq("s1")
  }

  it should "prefer the freshly projected row over the stale one for the same venue" in {
    // Mid-catch-up both exist: same film, same cinema, one row per slug. The
    // venue must appear ONCE, and with the row projected under the live slug.
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("dune|2021"))
    repository.upsertScreening(CityScreening("old", "dune|2021", "san-francisco", "Roxie", None, Nil))
    repository.upsertScreening(CityScreening("new", "dune|2021", "san-francisco-bay-area", "Roxie", None, Nil))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    rm.screeningsForCity("san-francisco-bay-area").map(_._id) shouldBe Seq("new")
  }

  // A re-cluster moves venues between pages without renaming any: Turek's
  // cinema was on Konin's page and is on "Turek i okolice" now; Szamotuły's was
  // on Poznań's. Rows projected before the move still carry the old slug until
  // their film is projected again — served from the new page straight away,
  // and never from the major city the venue left.
  "A venue that moved to another page" should "be served from its new page, and not from the one it left" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("dune|2021"))
    repository.upsertScreening(CityScreening("tur", "dune|2021", "konin", models.KinoTur.displayName, None, Nil))
    repository.upsertScreening(CityScreening("halszka", "dune|2021", "poznan", models.KinoHalszka.displayName, None, Nil))
    repository.upsertScreening(CityScreening("muza", "dune|2021", "poznan", models.KinoMuza.displayName, None, Nil))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    rm.screeningsForCity("turek").map(_._id) shouldBe Seq("tur")
    rm.screeningsForCity("konin") shouldBe empty
    rm.screeningsForCity("poznan").map(_._id) shouldBe Seq("muza")
    rm.screeningsForCity(City.forCinema(models.KinoHalszka).get.slug).map(_._id) shouldBe Seq("halszka")
  }

  "A city SPLIT out of a shared slug" should "take only its own venues from the shared bucket" in {
    // `alaska` was one city and is now nine metros, so — unlike a rename — its
    // rows hold every OTHER metro's venues too. Anchorage must not serve Juneau's
    // cinema, which is 1,400 km away with no road between them; and it must
    // still serve its own, or the split blanks the state for a whole 14 h
    // cadence.
    val anchorage = City.bySlug("anchorage").getOrElse(fail("no anchorage"))
    val juneau    = City.bySlug("juneau").getOrElse(fail("no juneau"))
    val mine      = anchorage.cinemaDisplayNames.head
    val theirs    = juneau.cinemaDisplayNames.head

    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("dune|2021"))
    repository.upsertScreening(CityScreening("mine", "dune|2021", "alaska", mine, None, Nil))
    repository.upsertScreening(CityScreening("theirs", "dune|2021", "alaska", theirs, None, Nil))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    rm.screeningsForCity("anchorage").map(_._id) shouldBe Seq("mine")
    rm.screeningsForCity("juneau").map(_._id) shouldBe Seq("theirs")
  }

  it should "leave a city that never changed slug reading only its own bucket" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("dune|2021"))
    repository.upsertScreening(screening("s1", "dune|2021", "san-francisco"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    rm.screeningsForCity("los-angeles") shouldBe empty
    // And the retired slug itself still resolves, for anything reaching it directly.
    rm.screeningsForCity("san-francisco").map(_._id) shouldBe Seq("s1")
  }

  // ── Boot: a write landing between the hydrate and the watches ───────────────
  //
  // 2026-09-23 17:26Z: web-pl booted, hydrated, and only then opened its change streams "from
  // now". Two Włodawa screenings the worker wrote in between reached neither — the hydrate had
  // already read past them, the streams had not started — and the site served 8 films where the
  // corpus had 10 until the 30-minute backstop saw the count drift (screenings mem=10600/db=10602,
  // 17:56:36Z), which paged ReadModelServingDiffersFromCorpus. A missed write that leaves the
  // counts equal (a changed showtime) the backstop never sees at all.

  /** A store that takes one more write the moment the boot hydrate has read the screenings —
   *  the worker writing while the web is between its reload and its watches. */
  private def writeDuringHydrate(write: InMemoryReadModelRepository => Unit): InMemoryReadModelRepository =
    new InMemoryReadModelRepository {
      private var pending = true
      override def findAllScreenings(): Seq[CityScreening] = {
        val snapshot = super.findAllScreenings()
        if (pending) { pending = false; write(this) }
        snapshot
      }
    }

  "start" should "serve a screening written after the hydrate read and before the watches opened" in {
    val repository = writeDuringHydrate { r =>
      r.upsertMovie(movie("late|2026"))
      r.upsertScreening(screening("late", "late|2026", "wlodawa"))
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wlodawa"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)

    rm.start()

    rm.screeningsForCity("wlodawa").map(_._id) should contain theSameElementsAs Seq("s1", "late")
    rm.movie("late|2026") shouldBe defined
    rm.stop()
  }

  it should "drop a screening deleted after the hydrate read and before the watches opened" in {
    val repository = writeDuringHydrate(_.deleteScreening("s1"))
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wlodawa"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)

    rm.start()

    rm.screeningsForCity("wlodawa") shouldBe empty
    rm.stop()
  }

  // ── Backstop: cheap drift check, not an unconditional full reload ────────────

  private def started(repository: InMemoryReadModelRepository): WebReadModel = {
    val rm = new WebReadModel(repository, driftSettle = WebReadModel.DriftSettle(Duration.Zero), clock = _root_.tools.SpecClock.Pinned)
    rm.start() // hydrates once + opens the watches; reset the counters so we only
    repository.findAllMoviesCalls.set(0)     // measure what the backstop tick itself does
    repository.findAllScreeningsCalls.set(0)
    rm
  }

  "backstopTick" should "skip the full reload while streams are live and counts match" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    rm.backstopTick()

    repository.findAllMoviesCalls.get()     shouldBe 0
    repository.findAllScreeningsCalls.get() shouldBe 0
    rm.stop()
  }

  it should "fall back to a full reload when a change stream has died" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    repository.failMovieStream()
    rm.backstopTick()

    repository.findAllMoviesCalls.get()     should be >= 1
    repository.findAllScreeningsCalls.get() should be >= 1
    rm.stop()
  }

  // A Mongo outage longer than the driver's one resume ENDS a change stream for good, and
  // nothing opened it again: every pod that lived through a blip served writes up to 30
  // minutes late (the backstop) for the rest of its life, and paid a full reload every tick.
  "coldRetryTick" should "reopen a change stream that died, catching up on what it missed" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    val rm = started(repository)

    repository.failMovieStream()
    repository.upsertMovie(movie("during|2026"))   // written while the stream was down
    rm.coldRetryTick()
    repository.upsertMovie(movie("after|2026"))    // written once it is back

    rm.movie("during|2026") shouldBe defined
    rm.movie("after|2026") shouldBe defined
    repository.movieWatchesOpened.get() shouldBe 2
    repository.screeningWatchesOpened.get() shouldBe 1   // the live one is left alone
    rm.stop()
  }

  // A reopen INTO the outage opens a stream that goes live once Mongo answers — but its catch-up
  // read failed, so what was written while the streams were down is in neither. Live streams and a
  // warm model made every later tick a no-op: those writes waited on the backstop's count drift.
  it should "read again once the streams are live, when the reopen's catch-up read failed" in {
    val repository = new UnreadableReadModelRepository
    repository.healReads()
    repository.upsertMovie(movie("belle|2021"))
    val rm = started(repository)

    repository.failingReads = true
    repository.failMovieStream()
    repository.upsertMovie(movie("during|2026"))
    rm.coldRetryTick()                 // reopened, but its read failed
    rm.movie("during|2026") shouldBe None
    repository.healReads()
    rm.coldRetryTick()

    rm.movie("during|2026") shouldBe defined
    rm.stop()
  }

  it should "back off reopening a stream that keeps dying, rather than reload every tick" in {
    val repository = new InMemoryReadModelRepository {
      override def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit, from: Option[StreamCheckpoint]): Option[StreamSubscription] = {
        val opened = super.watchMovies(onUpsert, onDelete, from)
        failMovieStream()   // Mongo still down: the stream dies as soon as it opens
        opened
      }
    }
    val rm = started(repository)

    (1 to 10).foreach(_ => rm.coldRetryTick())

    // Reopened on ticks 1, 3 and 7 — not on all ten.
    repository.movieWatchesOpened.get() shouldBe 1 + 3
    rm.stop()
  }

  it should "reload when a server-side count drifts from the in-memory model" in {
    // countScreenings reports one more than was streamed in — standing in for a
    // delivered event the applier dropped, which a count-blind backstop misses.
    val repository = new InMemoryReadModelRepository {
      override def countScreenings(): tools.ReadOutcome[Long] = super.countScreenings().map(_ + 1)
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    rm.backstopTick()

    repository.findAllScreeningsCalls.get() should be >= 1
    rm.stop()
  }

  // 2026-10-02: every drift reload web-us logged in a week was a count read mid-write -- the
  // database a screening or two ahead of a change event still on its way.
  it should "not reload over a mismatch that settles once the in-flight event lands" in {
    val counts = new java.util.concurrent.atomic.AtomicInteger(0)
    val repository = new InMemoryReadModelRepository {
      override def countScreenings(): tools.ReadOutcome[Long] =
        super.countScreenings().map(_ + (if (counts.getAndIncrement() == 0) 1 else 0))
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    rm.backstopTick()

    repository.findAllScreeningsCalls.get() shouldBe 0
    rm.stop()
  }

  // ── reload: one corpus on the heap, not two ─────────────────────────────────
  //
  // 2026-10-02: web-us heap-OOMed 34s into a drift reload. The reload buffered all of
  // `web_screenings` (~400 MB decoded for the US) and grouped it by city while the live
  // buckets still held the previous ~400 MB, which a 1 GiB heap cannot hold. Streamed a page
  // at a time and written over the live rows, the reload's transient is one page.

  // ── One object per distinct showtime instant ─────────────────────────────────
  //
  // A decoded row carries a fresh LocalDateTime (and its LocalDate and LocalTime) per
  // showtime, though a city's showtimes repeat a few thousand instants: New York's
  // 50,289 hold 6,793 distinct ones. Shared, a showtime costs ~211 bytes instead of ~311,
  // ~165 MB of web-us's resting heap.

  "a row entering the model" should "share one date-time object with every equal showtime" in {
    def at() = java.time.LocalDateTime.of(2026, 6, 10, 18, 30)     // a fresh object per call
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw").copy(showtimes = Seq(models.Showtime(at(), None))))
    repository.upsertScreening(screening("s2", "belle|2021", "krakow").copy(showtimes = Seq(models.Showtime(at(), None))))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()
    val Seq(a, b) = rm.allScreenings().map(_.showtimes.head.dateTime)
    a should be theSameInstanceAs b
  }

  // ── Booking URLs held split at the prefix their row shares ───────────────────
  //
  // A booking URL was ~150 bytes of web-us's heap per showtime (a String and its Some),
  // ~70% of what a showtime still cost after the instants were shared — yet a row's URLs
  // differ only in their last few characters. Held as the row's common prefix (one shared
  // String) plus each showtime's own remainder as bytes, New York's cost ~44 bytes each.

  "a row entering the model" should "hold its booking URLs split at the prefix they share, spelling each exactly" in {
    val at   = java.time.LocalDateTime.of(2026, 6, 10, 18, 30)
    val urls = Seq("https://kino.example/buy?show=101", "https://kino.example/buy?show=102", "https://kino.example/buy?show=2")
    val stored = screening("s1", "belle|2021", "wroclaw").copy(showtimes = urls.map(url => models.Showtime(at, Some(url))))
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(stored)
    repository.upsertScreening(screening("s2", "belle|2021", "krakow").copy(showtimes = urls.map(url => models.Showtime(at, Some(url)))))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    val Seq(held, other) = rm.allScreenings().sortBy(_._id)
    held shouldBe stored
    held.showtimes.flatMap(_.bookingUrl) shouldBe urls
    val prefixes = (held.showtimes ++ other.showtimes).map(_.urlSplitPrefix.get)
    prefixes.distinct shouldBe Seq("https://kino.example/buy?show=")
    prefixes.foreach(_ should be theSameInstanceAs prefixes.head)
  }

  it should "split a re-read row at its NEW prefix when the cinema's booking domain changed" in {
    val at = java.time.LocalDateTime.of(2026, 6, 10, 18, 30)
    def row(host: String) = screening("s1", "belle|2021", "wroclaw")
      .copy(showtimes = Seq(1, 2).map(n => models.Showtime(at, Some(s"https://$host/buy?show=$n"))))
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(row("old.example"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()
    repository.upsertScreening(row("tickets.new.example"))
    rm.reload()

    val Seq(held) = rm.allScreenings()
    held.showtimes.flatMap(_.bookingUrl) shouldBe Seq("https://tickets.new.example/buy?show=1", "https://tickets.new.example/buy?show=2")
    held.showtimes.map(_.urlSplitPrefix) shouldBe Seq.fill(2)(Some("https://tickets.new.example/buy?show="))
  }

  "reload" should "stream the screenings rather than buffer the whole collection" in {
    val buffered = new java.util.concurrent.atomic.AtomicInteger(0)
    val repository = new InMemoryReadModelRepository {
      override def findAllScreenings(): Seq[CityScreening] = { buffered.incrementAndGet(); super.findAllScreenings() }
      override def foreachScreening(f: CityScreening => Unit): tools.ScanOutcome = { super.findAllScreenings().foreach(f); tools.ScanOutcome.complete }
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    repository.upsertScreening(screening("s2", "belle|2021", "krakow"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)

    rm.reload()

    buffered.get() shouldBe 0
    rm.allScreenings().map(_._id) should contain theSameElementsAs Seq("s1", "s2")
  }

  it should "still evict the rows and cities a complete read no longer holds" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    repository.upsertScreening(screening("s2", "belle|2021", "wroclaw"))
    repository.upsertScreening(screening("s3", "belle|2021", "krakow"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    repository.deleteScreening("s2")
    repository.deleteScreening("s3")
    rm.reload()

    rm.screeningsForCity("wroclaw").map(_._id) shouldBe Seq("s1")
    rm.screeningsForCity("krakow") shouldBe empty
    rm.allScreenings().map(_._id) shouldBe Seq("s1")
  }

  // A drift reload runs with the change streams live, and its scan takes tens of seconds on the US
  // corpus. What the streams apply meanwhile is newer than the scan's read: a row inserted must not
  // be evicted for missing from the scan, a row deleted must not come back from it, and the film a
  // new row screens must still name that row's city, or its next metadata change leaves that
  // city's validator — and its cached page — where it was.
  it should "keep what the change streams apply while its scan runs" in {
    @volatile var during: () => Unit = () => ()
    val repository = new InMemoryReadModelRepository {
      override def foreachScreening(f: CityScreening => Unit): tools.ScanOutcome = {
        val read = super.findAllScreenings()
        val now = during; during = () => (); now()
        read.foreach(f); tools.ScanOutcome.complete
      }
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    repository.upsertScreening(screening("s2", "belle|2021", "wroclaw"))
    val rm = started(repository)
    during = () => { repository.upsertScreening(screening("new", "belle|2021", "krakow")); repository.deleteScreening("s2") }

    rm.reload()

    rm.allScreenings().map(_._id) should contain theSameElementsAs Seq("s1", "new")
    val krakowBefore = rm.lastModifiedFor("krakow")
    repository.upsertMovie(movie("belle|2021").copy(runtimeMinutes = Some(100)))
    rm.lastModifiedFor("krakow") should be > krakowBefore
    rm.stop()
  }

  // An incomplete keyset scan hands back only the pages it reached. Evicting against that
  // would drop every row past the failure from a model that was serving them correctly.
  it should "keep the rows it holds when the screenings read comes back incomplete" in {
    @volatile var screeningsFail = false
    // Both read shapes fail the way `MongoReadModelRepository`'s do: the buffered one empty,
    // the streamed one partway through and reporting it.
    val repository = new InMemoryReadModelRepository {
      override def findAllScreenings(): Seq[CityScreening] =
        if (screeningsFail) Seq.empty else super.findAllScreenings()
      override def foreachScreening(f: CityScreening => Unit): tools.ScanOutcome =
        if (!screeningsFail) super.foreachScreening(f)
        else { super.findAllScreenings().take(1).foreach(f); tools.ScanOutcome.of(whole = false, "screenings fail on purpose") }
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    repository.upsertScreening(screening("s2", "belle|2021", "krakow"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    screeningsFail = true
    rm.reload()

    rm.allScreenings().map(_._id) should contain theSameElementsAs Seq("s1", "s2")
  }

  // ── Cold retry: a failed boot read must not become an empty corpus ───────────
  //
  // The 2026-07-29 outage: prod Mongo was OOM-killed, the web tier restarted while it was
  // unreachable, and `start()`'s single hydrate came back empty. `reload`'s "empty result on
  // a warm cache is a Mongo hiccup" guard cannot apply at boot — the cache IS empty then — so
  // the failed read was accepted as the corpus and all 41 PL + 79 UK cities served zero films.
  // Nothing re-read until the 1800s backstop, so the board stayed blank until an unrelated
  // health-check restart happened to land on a recovered Mongo.

  "coldRetryTick" should "re-read while serving an empty corpus the database does not have" in {
    val repository = new UnreadableReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))

    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()
    // The failure this pins: the read failed, so there is nothing to serve — while the
    // database demonstrably holds a film.
    rm.allMovies() shouldBe empty
    repository.countMovies().shouldBe(tools.ReadOutcome.Answered(1L))

    // Mongo comes back. No restart, no 1800s backstop — the cold retry must notice that
    // it is serving nothing while the database holds films, and rehydrate.
    repository.healReads()
    rm.coldRetryTick()

    rm.allMovies().map(_._id) shouldBe Seq("belle|2021")
    rm.screeningsForCity("wroclaw").map(_._id) shouldBe Seq("s1")
  }

  // A boot whose films read but whose screenings did not serves every film with no showtimes,
  // and `movies` is not empty, so a cold retry gated on "no films" never looked again — the
  // site stayed showtime-less until the 30-minute backstop. Cold is "no complete read yet".
  it should "re-read when the boot read the films but not the screenings" in {
    @volatile var screeningsFail = true
    val repository = new InMemoryReadModelRepository {
      override def foreachScreening(f: CityScreening => Unit): tools.ScanOutcome =
        if (screeningsFail) tools.ScanOutcome.of(whole = false, "screenings fail on purpose") else super.foreachScreening(f)
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()
    rm.allMovies().map(_._id) shouldBe Seq("belle|2021")
    rm.hydrated shouldBe false

    screeningsFail = false
    rm.coldRetryTick()

    rm.screeningsForCity("wroclaw").map(_._id) shouldBe Seq("s1")
    rm.hydrated shouldBe true
  }

  // `hydrated` is what the web pod's `/ready` reports: a rolling deploy must not hand traffic
  // to a pod whose boot read failed, nor hold back one that read a genuinely empty corpus.
  "hydrated" should "stay false until a read of both collections completes" in {
    val repository = new UnreadableReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.hydrated shouldBe false
    rm.reload()
    rm.hydrated shouldBe false

    repository.healReads()
    rm.reload()
    rm.hydrated shouldBe true
  }

  it should "be true for a corpus that really is empty" in {
    val rm = new WebReadModel(new InMemoryReadModelRepository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()
    rm.hydrated shouldBe true
  }

  it should "stay true once reached, though a later read fails — a warm model keeps serving" in {
    val repository = new UnreadableReadModelRepository
    repository.healReads()
    repository.upsertMovie(movie("belle|2021"))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()
    repository.failingReads = true
    rm.reload()
    rm.hydrated shouldBe true
  }

  "coldRetryTick" should "cost nothing once the model is warm" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    val rm = started(repository)

    rm.coldRetryTick()

    // A warm model is the backstop's business; drift is not the cold retry's to chase.
    repository.findAllMoviesCalls.get().shouldBe(0)
    rm.stop()
  }

  it should "not re-read when the database really is empty" in {
    val repository = new InMemoryReadModelRepository
    val rm = started(repository)

    rm.coldRetryTick()

    repository.findAllMoviesCalls.get().shouldBe(0)
    rm.stop()
  }

  // ── Per-city cache validators ───────────────────────────────────────────────
  //
  // `lastModified` is the MODEL-wide stamp: it moves when anything anywhere
  // changes. Used as the conditional-GET validator it meant a Warsaw showtime
  // invalidated London's ETag, so every city's payload appeared to change every
  // couple of minutes and no 304 -- client or edge -- survived long.
  // `lastModifiedFor(city)` is the narrower question the conditional actually
  // asks: did the bytes THAT CITY renders change?
  //
  // The one thing that stops this being a plain per-city bucket stamp is
  // `FilmSlugs`: film addresses are assigned over the WHOLE corpus, so a film
  // appearing in Warsaw can take the bare slug off a film playing in London and
  // change London's rendered links. Those changes -- and only those -- have to
  // move every city, which is what the "slug corpus" cases below pin.

  private def titled(id: String, title: String, year: Option[Int] = None) =
    ResolvedMovie(id, title, None, None, Nil, None, year, Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)

  private def twoCityModel(): (InMemoryReadModelRepository, WebReadModel) = {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)))
    repository.upsertMovie(titled("dune|2021", "Dune", Some(2021)))
    repository.upsertScreening(screening("s-waw", "belle|2021", "warszawa"))
    repository.upsertScreening(screening("s-lon", "dune|2021", "london"))
    (repository, started(repository))
  }

  "lastModifiedFor" should "leave one city's validator alone when another city's showtimes change" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")

    repository.upsertScreening(CityScreening("s-waw-2", "belle|2021", "warszawa", "Muranow", None, Nil))

    rm.lastModifiedFor("warszawa") should be > londonBefore
    rm.lastModifiedFor("london") shouldBe londonBefore
    // The model-wide stamp still moves -- the sitemap and the filmSlugs memo want it.
    rm.lastModified should be > londonBefore
    rm.stop()
  }

  // A city's FIRST stamp after a reload used to be the bare clock reading, never compared with
  // the floor the reload had just advanced — so a reading not past that floor (a clock that
  // stepped back, or one too coarse to have moved) left the city's validator where it was:
  // a cached page and every client's 304 kept naming bytes the city no longer renders.
  it should "move a city's validator on its first change after a reload, whatever the clock reads" in {
    val clock      = java.time.Clock.fixed(java.time.Instant.parse("2026-06-10T10:00:00Z"), java.time.ZoneOffset.UTC)
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)))
    val rm = new WebReadModel(repository, driftSettle = WebReadModel.DriftSettle(Duration.Zero), clock = clock)
    rm.start()
    val before = rm.lastModifiedFor("warszawa")

    repository.upsertScreening(screening("s-waw", "belle|2021", "warszawa"))

    rm.lastModifiedFor("warszawa") should be > before
    rm.stop()
  }

  // A reload re-derives every city and drops their stamps for the floor — which it advanced
  // only past ITSELF. A city stamped after the floor then fell back below where it stood, and a
  // validator that moves backwards is a cached copy that looks newer than the reload's data.
  it should "never move a city's validator backwards across a reload" in {
    val clock      = java.time.Clock.fixed(java.time.Instant.parse("2026-06-10T10:00:00Z"), java.time.ZoneOffset.UTC)
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)))
    val rm = new WebReadModel(repository, driftSettle = WebReadModel.DriftSettle(Duration.Zero), clock = clock)
    rm.start()
    repository.upsertScreening(screening("s-waw", "belle|2021", "warszawa"))
    val before = rm.lastModifiedFor("warszawa")

    rm.reload()

    rm.lastModifiedFor("warszawa") should be > before
    rm.stop()
  }

  // The same, under a request reading the validator WHILE a reload drops the city stamps: it read
  // the floor before the reload advanced it and the city's stamp after the reload dropped it, and
  // answered the old floor — a validator older than one already handed out, which the encoded-
  // response cache still held a body for, rendered before the city's change. A race loop: the
  // window is two reads wide, so the loop runs until it is seen or the budget is spent.
  it should "never move a city's validator backwards for a request racing a reload" in {
    val clock      = java.time.Clock.fixed(java.time.Instant.parse("2026-06-10T10:00:00Z"), java.time.ZoneOffset.UTC)
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)))
    val rm = new WebReadModel(repository, driftSettle = WebReadModel.DriftSettle(Duration.Zero), clock = clock)
    rm.start()
    @volatile var racing = true
    val backwards = new java.util.concurrent.atomic.AtomicReference[Option[(java.time.Instant, java.time.Instant)]](None)
    val requests = new Thread(() => {
      var seen = rm.lastModifiedFor("warszawa")
      while (racing && backwards.get.isEmpty) {
        val now = rm.lastModifiedFor("warszawa")
        if (now.isBefore(seen)) backwards.set(Some(seen -> now)) else seen = now
      }
    })
    requests.setDaemon(true)
    requests.start()
    val deadline = System.nanoTime() + 2_000_000_000L
    var n = 0
    try while (System.nanoTime() < deadline && backwards.get.isEmpty) {
      repository.upsertScreening(screening(s"s-waw-$n", "belle|2021", "warszawa"))
      rm.reload()
      n += 1
    } finally { racing = false; requests.join(SpecTimeouts.Io.toMillis); rm.stop() }
    backwards.get shouldBe None
  }

  it should "move a city's validator when a screening is deleted from it, and no other city's" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")

    repository.deleteScreening("s-waw")

    rm.lastModifiedFor("warszawa") should be > londonBefore
    rm.lastModifiedFor("london") shouldBe londonBefore
    rm.stop()
  }

  it should "move only the cities screening a film when that film's metadata changes" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")
    val warsawBefore = rm.lastModifiedFor("warszawa")

    // A rating refresh on the film only Warsaw is screening: same title, same
    // year, so film addresses are untouched and London's bytes cannot have moved.
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)).copy(weightedRating = 7.5))

    rm.lastModifiedFor("warszawa") should be > warsawBefore
    rm.lastModifiedFor("london") shouldBe londonBefore
    rm.stop()
  }

  it should "move EVERY city when a title change reshuffles the corpus-wide film addresses" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")

    // Warsaw's film is retitled. `FilmSlugs` assigns addresses over the whole
    // corpus, so this can take a bare slug off London's film -- London's links
    // may now differ and its validator MUST move.
    repository.upsertMovie(titled("belle|2021", "Belle Renamed", Some(2021)))

    rm.lastModifiedFor("london") should be > londonBefore
    rm.stop()
  }

  it should "move EVERY city when a film enters the corpus" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")

    repository.upsertMovie(titled("dune|1984", "Dune", Some(1984)))

    rm.lastModifiedFor("london") should be > londonBefore
    rm.stop()
  }

  it should "move EVERY city when a film leaves the corpus" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")

    repository.deleteMovie("belle|2021")

    rm.lastModifiedFor("london") should be > londonBefore
    rm.stop()
  }

  it should "move every city on a full reload" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")

    rm.reload()

    rm.lastModifiedFor("london") should be > londonBefore
    rm.stop()
  }

  it should "answer for a city that has never been touched" in {
    val (_, rm) = twoCityModel()
    // No screenings, no stamp of its own -- it still needs a usable validator,
    // and the model-wide floor is the honest one.
    rm.lastModifiedFor("poznan") shouldBe rm.lastModifiedFor("krakow")
    rm.stop()
  }

  it should "move a renamed city's validator when a row lands under its former slug" in {
    // Mid-catch-up the projector still writes rows under the OLD slug, and
    // `screeningsForCity` serves them. A validator that ignored the former slug
    // would hand out a 304 for a page whose contents had just changed.
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("dune|2021", "Dune", Some(2021)))
    val rm = started(repository)
    val before = rm.lastModifiedFor("san-francisco-bay-area")

    repository.upsertScreening(screening("s1", "dune|2021", "san-francisco"))

    rm.lastModifiedFor("san-francisco-bay-area") should be > before
    rm.stop()
  }

  it should "advance strictly, so two changes inside one clock tick are still distinguishable" in {
    // The stamp is a wall clock, and a coarse one can hand out the same Instant
    // twice. A validator that repeated would serve a 304 for changed bytes, so
    // the stamp is monotonic by construction rather than by luck.
    val (repository, rm) = twoCityModel()
    val stamps = (1 to 50).map { n =>
      repository.upsertScreening(CityScreening(s"s-waw-$n", "belle|2021", "warszawa", s"Kino $n", None, Nil))
      rm.lastModifiedFor("warszawa")
    }
    stamps shouldBe stamps.sorted
    stamps.distinct.size shouldBe stamps.size
    rm.stop()
  }

  // ── A rewrite that changes nothing invalidates nothing ──────────────────────
  //
  // The change stream delivers DOCUMENT WRITES, not content changes. A re-key or
  // a venue re-projection rewrites every row it touches — `replaceFilm` once
  // rewrote all 298 rows for a single venue — and each of those arrives here as
  // an upsert. Bumping on the write rather than on a real difference threw away
  // a city's cached page, its gzipped body, and every client's 304 for a
  // document byte-identical to the one already held.
  //
  // These rows are pure case classes with no timestamp, so structural equality
  // is exactly the question "would any client see different bytes?".

  it should "leave a city's validator alone when an upsert rewrites an identical row" in {
    val (repository, rm) = twoCityModel()
    val before = rm.lastModifiedFor("warszawa")
    val modelWide = rm.lastModified

    repository.upsertScreening(screening("s-waw", "belle|2021", "warszawa"))

    rm.lastModifiedFor("warszawa") shouldBe before
    rm.lastModified shouldBe modelWide
    rm.stop()
  }

  it should "still move it when the rewrite genuinely changes the row" in {
    val (repository, rm) = twoCityModel()
    val before = rm.lastModifiedFor("warszawa")

    // Same _id, different content — a real showtime edit.
    repository.upsertScreening(CityScreening("s-waw", "belle|2021", "warszawa", "Muranow",
      Some("https://example.test/belle"), Nil))

    rm.lastModifiedFor("warszawa") should be > before
    rm.stop()
  }

  it should "move nothing when an upsert rewrites an identical movie document" in {
    val (repository, rm) = twoCityModel()
    val warsawBefore = rm.lastModifiedFor("warszawa")
    val londonBefore = rm.lastModifiedFor("london")
    val modelWide    = rm.lastModified

    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)))

    rm.lastModifiedFor("warszawa") shouldBe warsawBefore
    rm.lastModifiedFor("london")   shouldBe londonBefore
    rm.lastModified                shouldBe modelWide
    rm.stop()
  }

  it should "still move the screening cities when a movie rewrite changes a field" in {
    // The guard must not swallow a real metadata change.
    val (repository, rm) = twoCityModel()
    val before = rm.lastModifiedFor("warszawa")

    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)).copy(weightedRating = 8.1))

    rm.lastModifiedFor("warszawa") should be > before
    rm.stop()
  }

  // ── The stamp must not go backwards under concurrent appliers ───────────────
  //
  // The two change streams deliver on DIFFERENT threads, and the backstop
  // scheduler and /rehydrate touch the model too. A stamp advanced with
  // `x = advance(x)` is a read-modify-write, and @volatile buys visibility, not
  // atomicity — so an interleaving can lose an update and move the stamp
  // BACKWARDS. That is not cosmetic: once it regresses, a later advance can
  // re-issue a value some client already holds, which is a 304 for changed
  // bytes. This drives the appliers from many threads at once and fails on the
  // first observed decrease.

  it should "never let the validator go backwards while both streams apply concurrently" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)))
    repository.upsertScreening(screening("s0", "belle|2021", "warszawa"))
    val rm = started(repository)

    val threads = 8
    val perThread = 400
    val regressions = new java.util.concurrent.atomic.AtomicInteger(0)
    val stop = new java.util.concurrent.atomic.AtomicBoolean(false)

    // A sampler is the cleanest detector: it only ever reads, so any decrease it
    // sees is the model's own doing.
    val sampler = new Thread(() => {
      var previous = rm.lastModified
      while (!stop.get()) {
        val now = rm.lastModified
        if (now.isBefore(previous)) regressions.incrementAndGet()
        previous = now
      }
    })
    sampler.start()

    val workers = (1 to threads).map { t =>
      val th = new Thread(() => {
        var i = 0
        while (i < perThread) {
          // Alternate the two streams and mix floor-bumping with city-scoped
          // changes, so both mutated fields are contended.
          if ((i + t) % 2 == 0)
            repository.upsertMovie(titled(s"film-$t-$i|2021", s"Film $t $i", Some(2021)))
          else
            repository.upsertScreening(screening(s"s-$t-$i", "belle|2021", "warszawa"))
          i += 1
        }
      })
      th.start(); th
    }
    workers.foreach(_.join())
    stop.set(true); sampler.join()

    regressions.get() shouldBe 0
    rm.stop()
  }

  // ── A per-city synopsis change is a per-city change ─────────────────────────
  //
  // `ResolvedMovie.synopsisByCity` holds one blurb per city, and
  // `synopsisFor(city)` reads that city's entry before falling back to the
  // city-independent `synopsis`. So a cinema blurb landing for Warsaw changes
  // WARSAW's bytes and nobody else's — yet a movie upsert bumped every city
  // screening the film, because the document as a whole had changed.

  private def screenedInBoth(): (InMemoryReadModelRepository, WebReadModel) = {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)))
    repository.upsertScreening(screening("s-waw", "belle|2021", "warszawa"))
    repository.upsertScreening(screening("s-poz", "belle|2021", "poznan"))
    (repository, started(repository))
  }

  it should "bump only the city whose synopsis override changed" in {
    val (repository, rm) = screenedInBoth()
    val poznanBefore = rm.lastModifiedFor("poznan")
    val warsawBefore = rm.lastModifiedFor("warszawa")

    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021))
      .copy(synopsisByCity = Map("warszawa" -> "Muranow's own blurb")))

    rm.lastModifiedFor("warszawa") should be > warsawBefore
    rm.lastModifiedFor("poznan")   shouldBe poznanBefore
    rm.stop()
  }

  it should "bump a city whose synopsis override was REMOVED, since it falls back now" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021))
      .copy(synopsisByCity = Map("warszawa" -> "blurb", "poznan" -> "other")))
    repository.upsertScreening(screening("s-waw", "belle|2021", "warszawa"))
    repository.upsertScreening(screening("s-poz", "belle|2021", "poznan"))
    val rm = started(repository)
    val poznanBefore = rm.lastModifiedFor("poznan")
    val warsawBefore = rm.lastModifiedFor("warszawa")

    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021))
      .copy(synopsisByCity = Map("poznan" -> "other")))

    rm.lastModifiedFor("warszawa") should be > warsawBefore
    rm.lastModifiedFor("poznan")   shouldBe poznanBefore
    rm.stop()
  }

  it should "still bump EVERY screening city when the fallback synopsis changes" in {
    // Cities with no override of their own render `synopsis`, so a change to it
    // reaches all of them. The narrowing must not swallow this.
    val (repository, rm) = screenedInBoth()
    val poznanBefore = rm.lastModifiedFor("poznan")

    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021))
      .copy(synopsis = Some("a new city-independent blurb")))

    rm.lastModifiedFor("poznan") should be > poznanBefore
    rm.stop()
  }

  it should "still bump every screening city when a non-synopsis field changes" in {
    val (repository, rm) = screenedInBoth()
    val poznanBefore = rm.lastModifiedFor("poznan")

    repository.upsertMovie(titled("belle|2021", "Belle", Some(2021)).copy(weightedRating = 9.2))

    rm.lastModifiedFor("poznan") should be > poznanBefore
    rm.stop()
  }

  // ── The slug map is versioned by the SLUG corpus, not by every change ───────
  //
  // `FilmSlugs` walks and sorts every film to assign addresses. It is a pure
  // function of the (id, title, releaseYear) projection — which is exactly what
  // `_globalFloor` tracks — so keying its memo on the model-wide stamp meant a
  // showtime edit anywhere threw the whole map away and the next listing render
  // rebuilt it over the entire corpus.

  it should "reuse the slug map across a change that cannot re-address anything" in {
    val (repository, rm) = twoCityModel()
    val first = rm.filmSlugs

    // A showtime edit: no film entered, left, or changed title/year.
    repository.upsertScreening(CityScreening("s-waw-2", "belle|2021", "warszawa", "Atlantic", None, Nil))

    rm.filmSlugs should be theSameInstanceAs first
    rm.stop()
  }

  it should "rebuild the slug map when a film's title changes" in {
    val (repository, rm) = twoCityModel()
    val first = rm.filmSlugs

    repository.upsertMovie(titled("belle|2021", "Belle Reissued", Some(2021)))

    rm.filmSlugs should not be theSameInstanceAs(first)
    rm.stop()
  }

  it should "rebuild the slug map when a film enters the corpus" in {
    val (repository, rm) = twoCityModel()
    val first = rm.filmSlugs

    repository.upsertMovie(titled("dune|1984", "Dune", Some(1984)))

    rm.filmSlugs should not be theSameInstanceAs(first)
    rm.stop()
  }

  // ── A delete of something we never held changed nothing ─────────────────────

  it should "not invalidate every city when a delete names a film it does not hold" in {
    // Deletes here are mostly RE-KEYS, so a delete for an id this model never
    // saw is ordinary traffic — and it frees no slug, because none was taken.
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")
    val modelWide    = rm.lastModified

    repository.deleteMovie("never-held|1999")

    rm.lastModifiedFor("london") shouldBe londonBefore
    rm.lastModified              shouldBe modelWide
    rm.stop()
  }

  it should "still invalidate every city when a delete removes a film it held" in {
    val (repository, rm) = twoCityModel()
    val londonBefore = rm.lastModifiedFor("london")

    repository.deleteMovie("belle|2021")

    rm.lastModifiedFor("london") should be > londonBefore
    rm.stop()
  }

  "screeningsOfFilms" should "answer exactly the city's rows of those films, former-slug fill included" in {
    val repository = new InMemoryReadModelRepository
    Seq("dune|2021", "alien|1979").foreach(id => repository.upsertMovie(movie(id)))
    repository.upsertScreening(CityScreening("old", "dune|2021", "san-francisco", "Roxie", None, Nil))
    repository.upsertScreening(CityScreening("new", "dune|2021", "san-francisco-bay-area", "Roxie", None, Nil))
    repository.upsertScreening(CityScreening("fill", "dune|2021", "san-francisco", "Castro", None, Nil))
    repository.upsertScreening(CityScreening("other", "alien|1979", "san-francisco-bay-area", "Roxie", None, Nil))
    repository.upsertScreening(CityScreening("other-old", "alien|1979", "san-francisco", "Castro", None, Nil))
    val rm = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    rm.reload()

    for (films <- Seq(Set("dune|2021"), Set("alien|1979"), Set("dune|2021", "alien|1979"), Set("nope")))
      rm.screeningsOfFilms("san-francisco-bay-area", films).map(_._id) should contain theSameElementsAs
        rm.screeningsForCity("san-francisco-bay-area").filter(s => films(s.filmId)).map(_._id)
    rm.screeningsOfFilms("san-francisco-bay-area", Set("dune|2021")).map(_._id) should contain theSameElementsAs Seq("new", "fill")
  }
}
