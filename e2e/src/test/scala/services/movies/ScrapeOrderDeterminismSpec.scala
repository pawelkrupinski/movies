package services.movies

import clients.tools.FakeHttpFetch
import controllers.{FilmSchedule, MovieControllerService}
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.MovieRecordCreated
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
 *   - the order `MovieRecordCreated` events are published,
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
 * Two things are asserted identical across orders:
 *   - the persisted `MovieRecord` VALUES (enrichment fields + every per-cinema
 *     `SourceData` slot), and
 *   - the controller-level rendered rows (`FilmSchedule` from
 *     `MovieControllerService.toSchedules`, across every city).
 *
 * Records are matched by NORMALISED title (`TitleNormalizer.sanitize`), NOT by
 * the raw `(cleanTitle, year)` storage key. The key's casing and year are still
 * order-dependent (the first cinema to create the row pins them; for a film
 * that resolves on TMDB the year is later canonicalised by the TMDB stage, but
 * a fixture-unenriched row keeps the first arriver's). That residual is
 * STORAGE-ONLY and display-neutral — `displayTitle`/derived `releaseYear` drive
 * the rendered output, so the rows below are asserted in full — and a clean
 * canonicalisation of the key needs a keying refactor + fixture re-record;
 * tracked as a follow-up, deliberately out of scope here.
 *
 * The scrape is limited to ONE film at a time: only that film's cinema slots
 * are fed into a fresh cache, so the run is cheap (no full-corpus fetch beyond
 * the one-time harvest) and the captured record/row contain exactly that film.
 */
class ScrapeOrderDeterminismSpec extends AnyFlatSpec with Matchers {

  private val Fixture          = "08-06-2026"
  private val Now              = LocalDateTime.of(2026, 6, 8, 0, 0)
  // Tuned to stay under ~10s wall-clock (the one-time harvest dominates; each
  // replay is one film's merge + enrichment + drain). 8×8 = 64 replays lands
  // ~9s; 10×10 ran ~12s. Raise if the reported timing leaves headroom.
  private val MoviesToTest     = 8
  private val IterationsPerMovie = 8
  // Per-fetch sleep ceiling (ms). Small enough that the perturbation adds only
  // a few ms per replay, large enough that concurrent rating stages reorder.
  private val MaxJitterMillis  = 3

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
    val w = new FixtureTestWiring(Fixture)
    val rows = mutable.ListBuffer.empty[(String, Option[Int], Cinema, CinemaMovie)]
    w.cinemaScrapers.foreach { scraper =>
      try {
        val touched = w.movieCache.recordCinemaScrape(scraper.cinema, scraper.fetch())
        touched.foreach { case (cm, key, _) => rows += ((key.cleanTitle, key.year, scraper.cinema, cm)) }
      } catch { case _: Exception => () } // mirror ShowtimeCache.refreshOne / runOneScrapeTick
    }
    rows.groupBy(_._1).toSeq.map { case (cleanTitle, rs) =>
      FilmGroup(cleanTitle, rs.head._2, rs.groupBy(_._3).view.mapValues(_.map(_._4).toSeq).toMap)
    }
  }

  /** The films worth testing: those reported by ≥2 cinemas (so cross-cinema
   *  merge order actually matters), the busiest first (these are the mainstream
   *  titles that also enrich fully, exercising the TMDB→IMDb→ratings cascade).
   *  Deterministic given the fixture. */
  private lazy val targets: Seq[FilmGroup] =
    harvest.filter(_.cinemaCount >= 2)
      .sortBy(g => (-g.cinemaCount, g.cleanTitle))
      .take(MoviesToTest)

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

    val created = mutable.ListBuffer.empty[MovieRecordCreated]
    rnd.shuffle(group.byCinema.toSeq).foreach { case (cinema, movies) =>
      val touched = w.movieCache.recordCinemaScrape(cinema, rnd.shuffle(movies))
      touched.foreach { case (cm, key, isNew) =>
        if (isNew)
          created += MovieRecordCreated(
            key.cleanTitle, key.year, cm.movie.originalTitle,
            if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None
          )
      }
    }
    rnd.shuffle(created.toList).foreach(w.eventBus.publish)
    w.drainServices()

    val record = w.movieRepo.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
    // The cache holds only this one film, so toSchedules over every city yields
    // just this film's rows (one per city it screens in). Sorted by city slug
    // for a stable, order-independent capture.
    val svc = new MovieControllerService(w.movieCache)
    val rows = City.all.sortBy(_.slug).flatMap(c => svc.toSchedules(c, Now))
    (record, rows)
  }

  "the scrape + enrichment pipeline" should
    "produce an identical persisted record and rendered row regardless of scrape/enrichment order" in {
    targets should not be empty

    val started = System.nanoTime()
    targets.zipWithIndex.foreach { case (group, mi) =>
      val (record0, rows0) = replay(group, seed(mi, 0))
      withClue(s"film '${group.cleanTitle}' (${group.cinemaCount} cinemas) yielded no record\n") {
        record0 should not be empty
      }
      (1 until IterationsPerMovie).foreach { i =>
        val s = seed(mi, i)
        val (recordI, rowsI) = replay(group, s)
        withClue(
          s"RECORD diverged for '${group.cleanTitle}' on iteration $i (seed=$s):\n${recordDiff(record0, recordI)}\n"
        ) { byTitle(recordI) shouldBe byTitle(record0) }
        withClue(
          s"ROW diverged for '${group.cleanTitle}' on iteration $i (seed=$s):\n" +
            s"--- iteration 0 ---\n${renderRows(rows0)}\n--- iteration $i ---\n${renderRows(rowsI)}\n"
        ) { rowsI shouldBe rows0 }
      }
    }
    val elapsedMs = (System.nanoTime() - started) / 1000000
    info(s"${targets.size} films × $IterationsPerMovie iterations = ${targets.size * IterationsPerMovie} replays in ${elapsedMs}ms")
  }

  // Distinct, reproducible seed per (film, iteration) so a divergence replays.
  private def seed(movieIdx: Int, iter: Int): Long = movieIdx.toLong * 1000L + iter

  // The persisted record VALUES keyed by normalised title — drops the
  // order-dependent storage key (casing/year, see class doc) so the comparison
  // asserts the enrichment + per-cinema slots, which must be order-invariant.
  private def byTitle(rs: Seq[StoredMovieRecord]): Map[String, models.MovieRecord] =
    rs.map(r => TitleNormalizer.sanitize(r.title) -> r.record).toMap

  /** Concise field-level diff of two persisted record sets — pinpoints the
   *  exact (record, source, field) that diverged instead of dumping the whole
   *  (huge) record, so the failure clue is actionable. */
  private def recordDiff(a: Seq[StoredMovieRecord], b: Seq[StoredMovieRecord]): String = {
    def short(x: Any): String = { val t = x.toString; if (t.length > 140) s"${t.take(140)}…(len ${t.length})" else t }
    val ka = a.map(r => TitleNormalizer.sanitize(r.title)).toSet
    val kb = b.map(r => TitleNormalizer.sanitize(r.title)).toSet
    if (ka != kb) return s"record set differs (by normalised title): only0=${ka -- kb} only1=${kb -- ka}"
    val byKey = b.map(r => TitleNormalizer.sanitize(r.title) -> r.record).toMap
    a.flatMap { ra =>
      val ea = ra.record; val eb = byKey(TitleNormalizer.sanitize(ra.title))
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
      s"${s.movie.title} | poster=${s.posterUrl} | cast=${s.cast.mkString(",")} | " +
        s"dir=${s.director.mkString(",")} | $showings"
    }.mkString("\n")
}

/** Sleeps a deterministic-per-(url,seed) 0..maxDelayMillis before each fetch so
 *  the concurrent enrichment stages complete in a perturbed order. Deterministic
 *  in the delay (a fixed function of the URL + seed), so a failing seed replays
 *  the same timing profile; the actual completion *order* still depends on the
 *  pools, which is exactly the nondeterminism under test. */
private class JitterHttpFetch(delegate: HttpFetch, seed: Long, maxDelayMillis: Int) extends HttpFetch {
  private def jitter(url: String): Unit = {
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
