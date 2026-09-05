package services.readmodel

import models.{City, CityScreening, ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
    val rm = new WebReadModel(repository)
    rm.reload()

    rm.allScreenings().map(_._id) should contain theSameElementsAs Seq("s1", "s2", "s3")
    // The per-city read key still partitions them — the dump is the union.
    rm.screeningsForCity("wroclaw").map(_._id) should contain theSameElementsAs Seq("s1", "s3")
  }

  it should "be empty when the cache holds no screenings" in {
    new WebReadModel(new InMemoryReadModelRepository).allScreenings() shouldBe empty
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
    val rm = new WebReadModel(repository)
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
    val rm = new WebReadModel(repository)
    rm.reload()

    rm.screeningsForCity("san-francisco-bay-area").map(_._id) shouldBe Seq("new")
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
    val rm = new WebReadModel(repository)
    rm.reload()

    rm.screeningsForCity("anchorage").map(_._id) shouldBe Seq("mine")
    rm.screeningsForCity("juneau").map(_._id) shouldBe Seq("theirs")
  }

  it should "leave a city that never changed slug reading only its own bucket" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("dune|2021"))
    repository.upsertScreening(screening("s1", "dune|2021", "san-francisco"))
    val rm = new WebReadModel(repository)
    rm.reload()

    rm.screeningsForCity("los-angeles") shouldBe empty
    // And the retired slug itself still resolves, for anything reaching it directly.
    rm.screeningsForCity("san-francisco").map(_._id) shouldBe Seq("s1")
  }

  // ── Backstop: cheap drift check, not an unconditional full reload ────────────

  private def started(repository: InMemoryReadModelRepository): WebReadModel = {
    val rm = new WebReadModel(repository)
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

  it should "reload when a server-side count drifts from the in-memory model" in {
    // countScreenings reports one more than was streamed in — standing in for a
    // delivered event the applier dropped, which a count-blind backstop misses.
    val repository = new InMemoryReadModelRepository {
      override def countScreenings(): Long = super.countScreenings() + 1
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    rm.backstopTick()

    repository.findAllScreeningsCalls.get() should be >= 1
    rm.stop()
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

    val rm = new WebReadModel(repository)
    rm.reload()
    // The failure this pins: the read failed, so there is nothing to serve — while the
    // database demonstrably holds a film.
    rm.allMovies() shouldBe empty
    repository.countMovies().shouldBe(1L)

    // Mongo comes back. No restart, no 1800s backstop — the cold retry must notice that
    // it is serving nothing while the database holds films, and rehydrate.
    repository.healReads()
    rm.coldRetryTick()

    rm.allMovies().map(_._id) shouldBe Seq("belle|2021")
    rm.screeningsForCity("wroclaw").map(_._id) shouldBe Seq("s1")
  }

  it should "cost nothing once the model is warm" in {
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
}
