package services.movies

import clients.tools.FakeHttpFetch
import controllers.{FilmSchedule, MovieControllerService}
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.MovieDetailsComplete
import tools.{FixtureTestWiring, HttpFetch}

import java.time.LocalDateTime
import java.util.concurrent.CompletableFuture
import scala.collection.mutable

/**
 * Determinism guard: the merged `MovieRecord` we persist and the
 * `FilmSchedule` row `MovieControllerService.toSchedules` renders must NOT
 * depend on the order things happen during a scrape tick. Several point fixes
 * in the code already exist *because* ordering bit us — `runOneScrapeTick`
 * splits record-from-publish so enrichment can't race a later cinema's merge,
 * and `MovieController.toSchedules` adds a `displayName` tiebreaker to kill the
 * "Kino Malta vs Kino Meduza" snapshot flake. This spec proves the invariant
 * by actively *forcing* the disorder rather than trusting it away.
 *
 * For each of a handful of multi-cinema films it replays the same scrape input
 * `IterationsPerMovie` times, each time randomizing every ordering we control:
 *   - the order cinema scrapes arrive (and the within-cinema movie order),
 *   - the order `MovieDetailsComplete` events are published,
 *   - the timing each enrichment fetch (TMDB / IMDb / Metacritic / Rotten
 *     Tomatoes / Filmweb) *completes* — a [[JitterHttpFetch]] sleeps a
 *     deterministic-per-(url,seed) few ms before each call, so the four rating
 *     stages on their separate worker pools finish in a perturbed order every
 *     run.
 * After each replay it drains the cascade (so everything has settled) and
 * captures both the persisted record and the rendered row. Iteration 0 is the
 * reference; every later iteration must match it. Equality is case-class
 * structural `==` (these models carry no timestamps/random ids, so `==` is
 * exact; the `data` map compares order-independently by construction).
 *
 * Two things are asserted FULLY identical across orders — no normalisation,
 * no tolerated fields:
 *   - the persisted `StoredMovieRecord` (its `(cleanTitle, year)` storage key
 *     AND the whole `MovieRecord`: enrichment fields + every per-cinema
 *     `SourceData` slot), and
 *   - the controller-level rendered rows (`FilmSchedule` from
 *     `MovieControllerService.toSchedules`, across every city).
 *
 * Getting there required making the pipeline order-independent end to end: the
 * canonical `(cleanTitle, year)` key is chosen by `canonicalRank` not arrival
 * order (`MovieCache.recordCinemaScrape`); every enrichment stage addresses the
 * row through `canonicalKeyFor` so a write never lands on a stale key; and a
 * cinema's multiple same-film rows (per-screening pages) are folded with a
 * deterministic representative + unioned showtimes. The spec was run across the
 * WHOLE corpus (every multi-cinema film) until zero divergences remained; the
 * committed config samples a generous subset for CI speed.
 *
 * The scrape is limited to ONE film at a time: only that film's cinema slots
 * are fed into a fresh cache, so the run is cheap (no full-corpus fetch beyond
 * the one-time harvest) and the captured record/row contain exactly that film.
 */
class ScrapeOrderDeterminismSpec extends AnyFlatSpec with Matchers {

  private val Fixture          = "08-06-2026"
  private val Now              = LocalDateTime.of(2026, 6, 8, 0, 0)
  // The one-time harvest dominates wall-clock; each replay is one film's merge
  // + enrichment + drain (~100ms). `MustTest` films are always exercised, then
  // the busiest-N auto-selection fills out the rest; iterations kept modest so
  // the (now larger) film set still finishes in ~15s.
  private val MoviesToTest     = 40
  private val IterationsPerMovie = 5
  // Per-fetch sleep ceiling (ms). Small enough that the perturbation adds only
  // a few ms per replay, large enough that concurrent rating stages reorder.
  private val MaxJitterMillis  = 3

  // Films that surfaced an order-dependence (or were near-misses) while this
  // spec was built — pinned so they're always replayed even when they're not in
  // the busiest-N: Tom i Jerry (enrichment-race director gate), Zawodowcy (the
  // anchor + MC/RT URL race), Straszny film + its Ukrainian dub (same-tmdbId
  // sibling fold), Mortal Kombat II / Diabeł u Prady 2 (multi-year canonical-key
  // merge), Bez wyjścia (multi-cinema yearless/year mix).
  private val MustTest = Seq(
    "Tom i Jerry", "Zawodowcy", "Straszny film", "Mortal Kombat",
    "Diabeł ubiera się u Prady", "Bez wyjścia",
    // Films that diverged run-to-run under the full parallel scrape while
    // hardening the concurrent path — pinned so the (cheaper, single-film)
    // order-shuffle spec reproduces the same discrepancy deterministically.
    "Drzewo magii", "All You Need Is Kill", "Erupcja", "Sny o słoniach",
    "Milcząca przyjaciółka"
  )

  /** A scraped film grouped across the cinemas that report it, keyed by the
   *  canonical cleanTitle `recordCinemaScrape` assigns. `byCinema` is what we
   *  replay (in shuffled order) every iteration. */
  private case class FilmGroup(cleanTitle: String, year: Option[Int], byCinema: Map[Cinema, Seq[CinemaMovie]]) {
    def cinemaCount: Int = byCinema.size
  }

  /** Parse every cinema fixture ONCE and group each scraped `CinemaMovie` under
   *  the canonical cleanTitle the real merge would assign it (read straight off
   *  `recordCinemaScrape`'s returned `CacheKey`, so we don't re-implement the
   *  normalisation/redirect rules). This is the only place we pay the
   *  full-corpus parse cost. */
  private lazy val harvest: Seq[FilmGroup] = {
    // Enumeration only — take the DIRECT path (staging = None) so
    // `recordCinemaScrape` returns every film's canonical key. With staging on it
    // would divert newcomers and return nothing to harvest from.
    val w = new FixtureTestWiring(Fixture) {
      override lazy val movieCache = new CaffeineMovieCache(movieRepo, eventBus, staging = None)
    }
    val rows = mutable.ListBuffer.empty[(String, Option[Int], Cinema, CinemaMovie)]
    w.cinemaScrapers.foreach { scraper =>
      try {
        val touched = w.movieCache.recordCinemaScrape(scraper.cinema, scraper.fetch())
        touched.foreach { case (cm, key, _) => rows += ((key.cleanTitle, key.year, scraper.cinema, cm)) }
      } catch { case _: Exception => () } // mirror cinemaScrapeRunner.run / runOneScrapeTick
    }
    rows.groupBy(_._1).toSeq.map { case (cleanTitle, rs) =>
      FilmGroup(cleanTitle, rs.head._2, rs.groupBy(_._3).view.mapValues(_.map(_._4).toSeq).toMap)
    }
  }

  /** The films worth testing: every `MustTest` film present in the corpus
   *  (always, even single-cinema), plus the busiest-N multi-cinema films (where
   *  cross-cinema merge order matters and the full TMDB→IMDb→ratings cascade
   *  runs). Deduplicated by cleanTitle, deterministic given the fixture. */
  private lazy val targets: Seq[FilmGroup] = {
    def matches(g: FilmGroup, needle: String): Boolean =
      g.cleanTitle.toLowerCase(java.util.Locale.ROOT).contains(needle.toLowerCase(java.util.Locale.ROOT))
    val curated = MustTest.flatMap(n => harvest.filter(matches(_, n))).sortBy(_.cleanTitle)
    val auto    = harvest.filter(_.cinemaCount >= 2).sortBy(g => (-g.cinemaCount, g.cleanTitle)).take(MoviesToTest)
    (curated ++ auto).distinctBy(_.cleanTitle)
  }

  /** Replay one film with a given RNG seed: fresh wiring (jittered enrichment
   *  fetch), shuffled cinema arrival + within-cinema order, shuffled event
   *  publish order, drain. Returns the persisted record(s) and the rendered
   *  rows across every city. */
  private def replay(group: FilmGroup, seed: Long): (Seq[StoredMovieRecord], Seq[FilmSchedule]) = {
    val rnd = new scala.util.Random(seed)
    val w = new FixtureTestWiring(Fixture) {
      // Only this wiring's ENRICHMENT goes through httoFetch here (we never call
      // scraper.fetch() in a replay — cinema data is the pre-parsed harvest), so
      // the jitter perturbs exactly the TMDB/IMDb/MC/RT/Filmweb arrival timing.
      override lazy val httoFetch: HttpFetch = new JitterHttpFetch(new FakeHttpFetch(fixture), seed, MaxJitterMillis)
    }

    val created = mutable.ListBuffer.empty[MovieDetailsComplete]
    rnd.shuffle(group.byCinema.toSeq).foreach { case (cinema, movies) =>
      val touched = w.movieCache.recordCinemaScrape(cinema, rnd.shuffle(movies))
      touched.foreach { case (cm, key, isNew) =>
        if (isNew)
          created += MovieDetailsComplete(
            key.cleanTitle, key.year, cm.movie.originalTitle,
            if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None
          )
      }
    }
    rnd.shuffle(created.toList).foreach(w.eventBus.publish)
    w.drainServices()
    // Newcomers were diverted to staging by recordCinemaScrape; promote + fold
    // them into `movies` (the prod scheduler's job) before reading the corpus.
    w.drainStaging()

    val record = w.movieRepo.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
    // Project the settled corpus into the read model and serve from it, exactly
    // as the web does. The cache holds only this one film, so toSchedules over
    // every city yields just this film's rows (one per city it screens in).
    // Sorted by city slug for a stable, order-independent capture.
    w.readModelProjector.reconcile()
    w.webReadModel.reload()
    val svc = new MovieControllerService(w.webReadModel)
    val rows = City.all.sortBy(_.slug).flatMap(c => svc.toSchedules(c, Now))
    (record, rows)
  }

  "the scrape + enrichment pipeline" should
    "produce an identical persisted record and rendered row regardless of scrape/enrichment order" in {
    targets should not be empty

    val started = System.nanoTime()
    val divergences = mutable.ListBuffer.empty[String]
    targets.zipWithIndex.foreach { case (group, mi) =>
      val (record0, rows0) = replay(group, seed(mi, 0))
      if (record0.isEmpty) divergences += s"NO RECORD for '${group.cleanTitle}' (${group.cinemaCount} cinemas)"
      (1 until IterationsPerMovie).foreach { i =>
        val (recordI, rowsI) = replay(group, seed(mi, i))
        if (recordI != record0)
          divergences += s"RECORD '${group.cleanTitle}' iter $i:\n${recordDiff(record0, recordI)}"
        if (rowsI != rows0)
          divergences += s"ROW '${group.cleanTitle}' iter $i (cinemas=${group.cinemaCount})"
      }
    }
    val elapsedMs = (System.nanoTime() - started) / 1000000
    info(s"${targets.size} films × $IterationsPerMovie iterations = ${targets.size * IterationsPerMovie} replays in ${elapsedMs}ms")
    withClue(s"${divergences.size} divergence(s):\n${divergences.take(60).mkString("\n")}\n") {
      divergences.toList shouldBe empty
    }
  }

  // ── Whole-corpus variant ───────────────────────────────────────────────────
  // The single-film replays above can't see a CROSS-film order dependence (a
  // cinema's prune sweeping another film's slot, an enrichment rekey colliding
  // across films, the convergence sweep). This variant feeds EVERY harvested
  // film's slots into one cache, per cinema, in shuffled cinema order — the same
  // disorder the real parallel cinema scrape produces — and asserts
  // the whole persisted corpus + rendered rows are identical across orders.
  private val CorpusIterations = 3

  /** The harvest re-grouped by CINEMA: every `CinemaMovie` a cinema reports
   *  across all films, so a replay records each cinema once (all its films at
   *  once) exactly as `recordCinemaScrape` is called in production. */
  private lazy val harvestByCinema: Seq[(Cinema, Seq[CinemaMovie])] =
    harvest.flatMap(_.byCinema.toSeq)
      .groupBy(_._1).view.mapValues(_.flatMap(_._2)).toSeq

  /** Replay the WHOLE corpus once: shuffled cinema arrival order, shuffled event
   *  publish order, jittered enrichment timing, then drain + the deterministic
   *  convergence sweep `bootStartup` runs. Returns the full persisted corpus and
   *  the rendered rows across every city. */
  private def replayCorpus(seed: Long): (Seq[StoredMovieRecord], Seq[FilmSchedule]) = {
    val rnd = new scala.util.Random(seed)
    val jitter = new JitterHttpFetch(new FakeHttpFetch(Fixture), seed, MaxJitterMillis)
    val w = new FixtureTestWiring(Fixture) {
      override lazy val httoFetch: HttpFetch = jitter
    }
    val created = mutable.ListBuffer.empty[MovieDetailsComplete]
    rnd.shuffle(harvestByCinema).foreach { case (cinema, movies) =>
      val touched = w.movieCache.recordCinemaScrape(cinema, rnd.shuffle(movies))
      touched.foreach { case (cm, key, isNew) =>
        if (isNew)
          created += MovieDetailsComplete(
            key.cleanTitle, key.year, cm.movie.originalTitle,
            if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None
          )
      }
    }
    // Fill each deferred cinema's per-film detail BEFORE publishing, exactly as
    // production's `runOneScrapeTick` does — so a detail-page year/director is on
    // the row when the TMDB stage resolves. Without this a year-from-detail film
    // (recorded bare/yearless) resolves on an order-pinned key and folds
    // order-dependently (the `Zaproszenie` same-title divergence).
    w.enrichDetailsSync()
    rnd.shuffle(created.toList).foreach(w.eventBus.publish)
    w.drainServices()
    // The async drain above is where the concurrent enrichment pools complete in
    // a perturbed order (what the jitter forces). The following `converge()`
    // re-enriches all ~950 films SERIALLY, but in a SHUFFLED order (seeded by
    // this replay) — mirroring prod's arbitrary `retryUnresolvedTmdb` sweep, so a
    // cross-film re-enrich/settle order can't change the result. Single-threaded,
    // so the per-fetch jitter only burns wall-clock here — switch it off.
    jitter.enabled = false
    w.converge(Some(rnd))
    val record = w.movieRepo.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
    // Project the converged corpus into the read model and warm it — the same
    // reconcile + reload the worker runs at boot — so we render through the
    // web's read seam (webReadModel), not the raw worker cache.
    w.readModelProjector.reconcile()
    w.webReadModel.reload()
    val svc = new MovieControllerService(w.webReadModel)
    val rows = City.all.sortBy(_.slug).flatMap(c => svc.toSchedules(c, Now))
    (record, rows)
  }

  // Once IGNORED for a deferDetail-determinism divergence: removing
  // KINOWO_DEFERRED_DETAIL made scrapers scrape BARE, so a *year-from-detail*
  // film (e.g. `Zaproszenie`, one of several same-title TMDB entries) was
  // recorded yearless and TMDB resolved it on an order-pinned key — folding it
  // order-dependently. Root cause was the harness, not production: `replayCorpus`
  // skipped the detail-enrich step that production's `runOneScrapeTick` runs
  // BEFORE publishing, so the detail-page year/director wasn't on the row when
  // the TMDB stage resolved. `replayCorpus` now calls `enrichDetailsSync()` first
  // (as production does), and the whole corpus is byte-identical across orders.
  "the whole-corpus scrape" should
    "persist an identical record set + rendered rows regardless of cross-film scrape/enrichment order" in {
    harvestByCinema should not be empty
    val started = System.nanoTime()
    val (record0, rows0) = replayCorpus(900000L)
    val divergences = mutable.ListBuffer.empty[String]
    (1 until CorpusIterations).foreach { i =>
      val (recordI, rowsI) = replayCorpus(900000L + i)
      if (recordI != record0) divergences += s"RECORD iter $i:\n${recordDiff(record0, recordI)}"
      if (rowsI != rows0) {
        val d = rows0.zipAll(rowsI, null, null).collect { case (x, y) if x != y => s"  0=${String.valueOf(x).take(600)}\n  i=${String.valueOf(y).take(600)}" }
        divergences += s"ROW iter $i differs (${rows0.size} vs ${rowsI.size}):\n${d.take(2).mkString("\n")}"
      }
    }
    val elapsedMs = (System.nanoTime() - started) / 1000000
    info(s"$CorpusIterations whole-corpus replays (${record0.size} films) in ${elapsedMs}ms")
    withClue(s"${divergences.size} divergence(s):\n${divergences.take(40).mkString("\n")}\n") {
      divergences.toList shouldBe empty
    }
  }

  // ── Settle-pass ±1-year film-identity collapse ─────────────────────────────
  // The harvested replays above can't construct the exact adjacency cases the
  // settle pass's `canonicalizeBySanitize` sub-clustering must get right —
  // distinct-tmdbId remakes sharing a title, a production-year cinema row one
  // year off a TMDB-resolved sibling, an unbounded run of adjacent years. These
  // build each scenario as a synthetic single-title corpus and assert the
  // settled outcome is identical across EVERY insertion permutation — the same
  // "force the disorder" guarantee, scoped to the partition logic. A wrong
  // (order-dependent) partition would chain years differently per order, or fold
  // two films together only when one spelling arrives first.

  private def resolvedRow(title: String, cinema: Cinema, year: Int, tmdbId: Int): (CacheKey, MovieRecord) =
    (CacheKey(title, Some(year)),   // mirrors MovieCache.keyOf — the title's own form, no searchTitle
      MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        (Tmdb: Source)   -> SourceData(title = Some(title), releaseYear = Some(year)),
        (cinema: Source) -> SourceData(title = Some(title), releaseYear = Some(year)))))

  private def cinemaRow(title: String, cinema: Cinema, year: Option[Int]): (CacheKey, MovieRecord) =
    (CacheKey(title, year),         // mirrors MovieCache.keyOf — the title's own form, no searchTitle
      MovieRecord(data = Map[Source, SourceData](
        (cinema: Source) -> SourceData(title = Some(title), releaseYear = year))))

  /** Settle a synthetic corpus across EVERY insertion order and return the
   *  distinct settled row-sets. Order-independence ⇒ exactly one. */
  private def settledAcrossOrders(rows: Seq[(CacheKey, MovieRecord)]): Set[Set[(String, Option[Int])]] =
    rows.permutations.map { ordered =>
      val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
      ordered.foreach { case (k, e) => cache.put(k, e) }
      cache.canonicalizeBySanitize()
      cache.snapshot().map(r => (r.title, r.year)).toSet
    }.toSet

  "canonicalizeBySanitize" should "collapse a film's ±1-year variants identically regardless of order" in {
    // One TMDB-resolved row (2026) + a production-year cinema row (2025) one year
    // off → one row at TMDB's year, whatever order they arrive.
    val outcomes = settledAcrossOrders(Seq(
      resolvedRow("Erupcja", Helios,   2026, 555),
      cinemaRow  ("Erupcja", KinoMuza, Some(2025))))
    outcomes shouldBe Set(Set(("Erupcja", Some(2026))))
  }

  it should "keep distinct-tmdbId same-title remakes as two rows regardless of order" in {
    val outcomes = settledAcrossOrders(Seq(
      resolvedRow("Diuna", Helios,   1984, 100),
      resolvedRow("Diuna", KinoMuza, 2021, 200)))
    outcomes shouldBe Set(Set(("Diuna", Some(1984)), ("Diuna", Some(2021))))
  }

  it should "window a {2024,2025,2026} no-tmdbId run into the same two clusters regardless of order" in {
    val outcomes = settledAcrossOrders(Seq(
      cinemaRow("Festiwal", Helios,                Some(2024)),
      cinemaRow("Festiwal", KinoMuza,              Some(2025)),
      cinemaRow("Festiwal", CinemaCityPoznanPlaza, Some(2026))))
    // Greedy from the lowest year: {2024,2025} then {2026}. NEVER one 3-year blob
    // and NEVER {2025,2026} — both of which an order-dependent chain could give.
    outcomes shouldBe Set(Set(("Festiwal", Some(2024)), ("Festiwal", Some(2026))))
  }

  // Distinct, reproducible seed per (film, iteration) so a divergence replays.
  private def seed(movieIdx: Int, iter: Int): Long = movieIdx.toLong * 1000L + iter

  /** Concise field-level diff of two persisted record sets — pinpoints the
   *  exact (record, source, field) that diverged instead of dumping the whole
   *  (huge) record, so the failure clue is actionable. */
  private def recordDiff(a: Seq[StoredMovieRecord], b: Seq[StoredMovieRecord]): String = {
    def short(x: Any): String = { val t = x.toString; if (t.length > 140) s"${t.take(140)}…(len ${t.length})" else t }
    val ka = a.map(r => (r.title, r.year)).toSet
    val kb = b.map(r => (r.title, r.year)).toSet
    if (ka != kb) return s"record keys differ: only0=${ka -- kb} only1=${kb -- ka}"
    val byKey = b.map(r => (r.title, r.year) -> r.record).toMap
    a.flatMap { ra =>
      val ea = ra.record; val eb = byKey((ra.title, ra.year))
      val scalars = Seq[(String, Any, Any)](
        ("tmdbId", ea.tmdbId, eb.tmdbId), ("imdbId", ea.imdbId, eb.imdbId),
        ("imdbRating", ea.imdbRating, eb.imdbRating), ("metascore", ea.metascore, eb.metascore),
        ("rottenTomatoes", ea.rottenTomatoes, eb.rottenTomatoes), ("filmwebRating", ea.filmwebRating, eb.filmwebRating),
        ("filmwebUrl", ea.filmwebUrl, eb.filmwebUrl), ("metacriticUrl", ea.metacriticUrl, eb.metacriticUrl),
        ("rottenTomatoesUrl", ea.rottenTomatoesUrl, eb.rottenTomatoesUrl)
      ).collect { case (n, x, y) if x != y => s"  scalar $n: 0=${short(x)} 1=${short(y)}" }
      val srcDiffs = (ea.data.keySet ++ eb.data.keySet).toSeq.sortBy(_.displayName).flatMap { src =>
        (ea.data.get(src), eb.data.get(src)) match {
          case (Some(da), Some(db)) if da != db =>
            val fields = Seq[(String, Any, Any)](
              ("title", da.title, db.title),
              ("synopsis.len", da.synopsis.map(_.length), db.synopsis.map(_.length)),
              ("cast", da.cast, db.cast), ("director", da.director, db.director),
              ("posterUrl", da.posterUrl, db.posterUrl), ("releaseYear", da.releaseYear, db.releaseYear),
              ("runtimeMinutes", da.runtimeMinutes, db.runtimeMinutes), ("countries", da.countries, db.countries),
              ("genres", da.genres, db.genres), ("filmUrl", da.filmUrl, db.filmUrl),
              ("showtimes.size", da.showtimes.size, db.showtimes.size), ("showtimes", da.showtimes, db.showtimes)
            ).collect { case (n, x, y) if x != y => s"      $n: 0=${short(x)} 1=${short(y)}" }
            Some(s"  source '${src.displayName}' differs:\n${fields.mkString("\n")}")
          case (Some(_), None) => Some(s"  source '${src.displayName}': only in iter0")
          case (None, Some(_)) => Some(s"  source '${src.displayName}': only in iter1")
          case _               => None
        }
      }
      if (scalars.isEmpty && srcDiffs.isEmpty) Nil
      else Seq(s"record '${ra.title}' (${ra.year}):") ++ scalars ++ srcDiffs
    }.mkString("\n")
  }

  private def renderRows(rows: Seq[FilmSchedule]): String =
    rows.map { s =>
      val showings = s.showings.map { case (d, cs) =>
        s"$d:" + cs.map(c => s"${c.cinema.displayName}(${c.showtimes.size})").mkString(",")
      }.mkString(";")
      s"${s.movie.title}/${s.movie.releaseYear} | poster=${s.posterUrl} | cast=${s.cast.mkString(",")} | " +
        s"dir=${s.director.mkString(",")} | $showings"
    }.mkString("\n")
}

/** Sleeps a deterministic-per-(url,seed) 0..maxDelayMillis before each fetch so
 *  the concurrent enrichment stages complete in a perturbed order. Deterministic
 *  in the delay (a fixed function of the URL + seed), so a failing seed replays
 *  the same timing profile; the actual completion *order* still depends on the
 *  pools, which is exactly the nondeterminism under test. */
private class JitterHttpFetch(delegate: HttpFetch, seed: Long, maxDelayMillis: Int) extends HttpFetch {
  // Toggled off for serial, single-threaded re-enrichment phases (e.g.
  // `converge()`), where a per-fetch sleep perturbs nothing and only burns
  // wall-clock. Stays on through the concurrent scrape/drain where it matters.
  @volatile var enabled: Boolean = true
  private def jitter(url: String): Unit = {
    if (!enabled) return
    val h = (url.hashCode.toLong * 0x9E3779B97F4A7C15L) ^ seed
    val d = Math.floorMod(h, (maxDelayMillis + 1).toLong)
    if (d > 0) Thread.sleep(d)
  }
  override def get(url: String): String = { jitter(url); delegate.get(url) }
  override def getBytes(url: String): Array[Byte] = { jitter(url); delegate.getBytes(url) }
  override def get(url: String, headers: Map[String, String]): String = { jitter(url); delegate.get(url, headers) }
  override def post(url: String, body: String, contentType: String): String = { jitter(url); delegate.post(url, body, contentType) }
  override def getAsync(url: String): CompletableFuture[String] = CompletableFuture.supplyAsync(() => get(url))
}
