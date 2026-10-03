package controllers

import models._
import play.api.mvc._
import play.api.Mode
import services.movies.TitleNormalizer
import services.readmodel.WebReadModel

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

/**
 * The dev-only `/debug*` pages: the corpus table, its per-row detail, the
 * staging queue, the read-model dump, the rating cadence, the per-row re-enrich
 * button and the card / film tuning pages — plus `rehydrate`, the one endpoint
 * here that runs in prod (admin-gated).
 *
 * ⚠️ THE ONLY CONTROLLER IN THE WEB TIER THAT READS THE SOURCE CORPUS. Every
 * page here pulls `movies` / `pending_movies` / the task queue from Mongo on
 * demand and blocks on it (`Await`) — full-collection scans over the local→prod
 * tunnel, ~6 s apiece. That is fine for an operator's tab and would not be for a
 * public route, which is why none of this lives beside the listing handlers:
 * `MovieController` renders from the projected read model alone and never
 * learns a `MovieRepository` exists.
 */
class DebugController(cc: ControllerComponents,
                      // Every collaborator the /debug pages read (corpus, staging,
                      // queue, cadence, read-model dump), keyed by country. In
                      // prod a single stack — this deployment's country; locally
                      // in Dev one per switchable country, so /debug can switch
                      // which country's db it shows via `?country=xx` same-origin
                      // instead of hopping to the other country's prod host
                      // (which 404s /debug).
                      debugCountries: DebugCountries,
                      // What `rehydrate` reloads.
                      readModel: WebReadModel,
                      // Gate for the state-mutating /…/debug/rehydrate trigger
                      // (the other /debug pages are dev-only; rehydrate runs in
                      // every mode, so it needs the admin gate instead).
                      adminAction: AdminAction,
                      environment: Mode,
                      // `cinema displayName -> public source-page URL`, the same
                      // links /uptime shows, sourced from the UptimeMonitor tag
                      // snapshot. Evaluated per request so it tracks live retags;
                      // used by the /debug table to link cinema names.
                      cinemaSourceUrls: () => Map[String, String] = () => Map.empty,
                      // The ONE country this deployment serves — which city slugs
                      // the tuning pages resolve, and whose title rules order the
                      // staging rows by the same anchor the worker wrote. Injected
                      // rather than read from `Country.fromEnv` at each use so a
                      // spec can exercise another country's host without mutating
                      // the process-global env that parallel suites share.
                      servingCountry: models.Country,
                      // What "now" is for the pages' own age readings (the mirror badge, cadence).
                      clock: java.time.Clock = java.time.Clock.systemUTC(),
                      // The serving country's title rules — the wiring's one instance.
                      normalizer: TitleNormalizer,
                     )(implicit messages: play.api.i18n.Messages) extends AbstractController(cc) {


  private def withCity(slug: String)(f: City => Result): Result = ServedCity.resolve(slug, servingCountry)(f)

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

  def debug(): Action[AnyContent] = Action { request =>
    devOnly {
      // The debug table is the global corpus; the only thing the view needs a
      // city for is the /movie fallback link on a row with no live showtimes
      // anywhere — give it the default city for that edge case.
      implicit val c: City = City.all.head
      // Which country's corpus to show (the boot country unless a Dev-only
      // ?country= switch selected another). `stack` binds every debug read below
      // to that country's Mongo db.
      val country = debugCountries.resolve(request)
      val stack   = debugCountries.stackFor(country)
      // The corpus listing is a SNAPSHOT (see `RefreshingSnapshot`): re-reading
      // `movies` + every `movie_slots` row per load cost 4–10 s a country switch
      // even off the local mirror. `findAllForListing` drops each row's per-cinema
      // `showtimes` server-side — the table renders only metadata + counts; the
      // showtimes are fetched per-row on expand via `/debug/details`.
      //
      // Staging + the queue stay per-request reads: both are small and bounded,
      // and the queue is what the staging rows are ORDERED by (the page renders only
      // the first `StagingRowLimit`, so the most imminent rows must sort to the top
      // server-side; the client poll then repaints the live badge in place, but does
      // not reorder). Fired concurrently with the listing, which only waits on its
      // very first read.
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val listingFuture = Future(stack.listing())
      val stagingFuture = Future(stack.stagingRepository.findAll())
      val queueFuture   = Future(stack.taskQueue.monitor(DebugController.DebugQueueActiveLimit))
      val (listing, (staging, queue)) =
        Await.result(listingFuture.zip(stagingFuture.zip(queueFuture)), 70.seconds)
      val staged = staging.sortBy(r => (r.title.toLowerCase, r.cinema.displayName))
      Ok(views.html.debug(
        listing.value.table,
        // The SELECTED country's rules, not the deployment's: /debug can switch
        // countries, and a row's display title must read as its own corpus keyed it.
        stack.movieRepository.normalizer,
        DebugController.orderStagingByQueue(staged, queue.active, normalizer),
        current = country, sameOrigin = debugCountries.switchable, mirror = mirrorAge(listing)))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** How far behind the mirror the data ON SCREEN is, for the debug navbar's badge:
   *  the mirror's newest stamp as of the snapshot's read, against now — so a
   *  snapshot taken a minute ago over a live mirror reads "1m behind", which is what
   *  it is. `None` in prod, where the pages read the source and there is no copy to
   *  be behind. */
  private def mirrorAge(snapshot: DebugSnapshot[?]): Option[services.MirrorFreshness.Age] =
    services.MirrorFreshness.describe(snapshot.mirrorNewest, clock.instant())

  /** Dev-only: the per-(rating source, film) adaptive refresh cadence. Films are
   *  grouped by their current refresh interval, slowest (most backed-off / stable)
   *  first, with the last two displayed-value changes shown on hover. Reads the
   *  worker-written `rating_cadence` collection + names the films from the same
   *  corpus-listing snapshot `/debug` renders. */
  def cadence(): Action[AnyContent] = Action { request =>
    devOnly {
      val country = debugCountries.resolve(request)
      val stack   = debugCountries.stackFor(country)
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val recordsFuture = Future(stack.ratingCadence())
      val listingFuture = Future(stack.listing())
      val (records, listing) = Await.result(recordsFuture.zip(listingFuture), 70.seconds)
      implicit val c: City = City.all.head   // only for the shared debug navbar's city link
      Ok(views.html.cadence(services.cadence.CadenceReport.build(records.value, listing.value.titleByTmdb.get), clock.instant(),
        // The OLDER of the two snapshots' stamps: the page is as behind as its stalest half.
        current = country, sameOrigin = debugCountries.switchable,
        mirror = mirrorAge(Seq(records, listing).minBy(_.mirrorNewest.getOrElse(java.time.Instant.MAX)))))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** Dev-only: the heavy per-source breakdown for ONE corpus row, fetched lazily
   *  by the /debug table when a row is expanded. Rendering every row's breakdown
   *  inline (each iterates `Cinema.all` × day × showtime) built one giant `Html`
   *  string that OOM'd the view on the full corpus; serving them per-row on
   *  demand keeps the initial /debug render to the light data rows only. The `id`
   *  is the row's Mongo `_id` (its `FilmId`), the same value the table rows are
   *  keyed on. */
  def debugDetails(id: String): Action[AnyContent] = Action { request =>
    devOnly {
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val stack = debugCountries.stackFor(debugCountries.resolve(request))
      stack.movieRepository.findById(services.movies.FilmId(id)) match {
        case Some(row) =>
          // The per-source enrichment log, joined on the tmdbId-keyed rating key.
          // Two bounded `_id in [...]` lookups (4 keys each), not the readers'
          // full-collection reads — this runs per row-expand. Issued CONCURRENTLY:
          // the reads are independent, and against a remote Mongo the round-trip
          // is the whole cost (see `buildFrom`).
          scala.util.Try(services.attempts.FilmAttemptReport.buildFrom(
            row.record.tmdbId, stack.attemptReader, stack.ratingCadenceReader)) match {
            case scala.util.Success(statuses) =>
              Ok(views.html.debugDetails(row.title, row.year, row.record,
                stack.movieRepository.normalizer, cinemaSourceUrls(), statuses))
            // Not an empty report: that reads as "never attempted".
            case scala.util.Failure(e) =>
              ServiceUnavailable(s"could not read the enrichment log for this row: ${e.getMessage}")
          }
        case None      => NotFound("no such row")
      }
    }
  }

  /** Dev-only: the active tasks in the durable queue (worked-on first, then the
   *  waiting block oldest-first), so the /debug staging table's queue columns can
   *  show, per row, whether an enrichment task already exists and its place in the
   *  queue. The page polls this; it's a bounded, index-backed `monitor` read (the
   *  same one `/tasks/data` serves), so the cost scales with viewers-while-open,
   *  not queue churn. Only the fields the page matches on are shipped — type,
   *  dedup key, state; a waiting task's place is already encoded by its list
   *  position. */
  def debugQueue(): Action[AnyContent] = Action { request =>
    devOnly {
      val snap = debugCountries.stackFor(debugCountries.resolve(request)).taskQueue.monitor(DebugController.DebugQueueActiveLimit)
      Ok(play.api.libs.json.Json.obj(
        "active" -> snap.active.map { t =>
          play.api.libs.json.Json.obj(
            "taskType" -> t.taskType,
            "dedupKey" -> t.dedupKey,
            "state"    -> t.state
          )
        }
      ))
    }
  }

  /** Dev-only: dump the read cache the web actually serves from — `web_movies` plus
   *  per-film `web_screenings` counts — so you can see exactly what a request would
   *  resolve against (vs `/debug`, which shows the source `movies` corpus). A row's
   *  screenings come from [[debugReadModelScreenings]] on expand: inlining every
   *  showtime of the US read model (~100k screening docs) OOM'd the dev server. */
  def debugReadModel(): Action[AnyContent] = Action { request =>
    devOnly {
      implicit val c: City = City.all.head
      val country = debugCountries.resolve(request)
      val dump    = debugCountries.stackFor(country).readModel()
      Ok(views.html.debugReadModel(dump.value,
        current = country, sameOrigin = debugCountries.switchable, mirror = mirrorAge(dump)))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** Dev-only: ONE read-model film's screening docs, every field of every showtime —
   *  fetched lazily by the `/debug/readmodel` table when a row is expanded. */
  def debugReadModelScreenings(id: String): Action[AnyContent] = Action { request =>
    devOnly {
      debugCountries.stackFor(debugCountries.resolve(request)).readModelScreeningsFor(id) match {
        case Some(screenings) => Ok(views.html.debugReadModelScreenings(id, screenings))
        // Not an empty list: that reads as "this film has no screenings".
        case None             => ServiceUnavailable(s"could not read the screenings of $id")
      }
    }
  }

  /** Dev-only: force a TMDB re-enrich of one film from the /debug row button.
   *  Enqueues a `ResolveTmdb` task the worker's `ResolveTmdbHandler` consumes;
   *  that re-resolves the row and writes the TMDB-side fields, and the worker's
   *  `EnrichmentReaper` then re-runs every rating refresher for the row on its
   *  next pass. Idempotent per (title, year): a repeat
   *  click while one is queued returns `duplicate`. Returns JSON for the page's
   *  fetch. */
  def reenrich(title: String, year: Option[Int]): Action[AnyContent] = Action { request =>
    devOnly {
      if (title.isEmpty) BadRequest(play.api.libs.json.Json.obj("error" -> "missing title"))
      else {
        val result = debugCountries.stackFor(debugCountries.resolve(request)).taskQueue.enqueue(
          services.tasks.TaskType.ResolveTmdb,
          services.tasks.EnrichTaskKeys.resolveTmdbDedup(title, year),
          // `force` so the operator's explicit re-enrich re-resolves even an
          // already-resolved row (the normal flow's guard would otherwise skip it).
          services.tasks.EnrichTaskKeys.resolveTmdbPayload(title, year, mode = services.tasks.ResolveMode.Force)
        )
        Ok(play.api.libs.json.Json.obj(
          "title"     -> title,
          "year"      -> year,
          "enqueued"  -> (result == services.tasks.EnqueueResult.Added),
          "duplicate" -> (result == services.tasks.EnqueueResult.Duplicate)
        ))
      }
    }
  }

  /** Dev-only visual-tuning page. Renders the real `_movieCard` partial(s)
   *  inside a `.tune-scope` wrapper plus a slider panel that drives the CSS
   *  custom properties the production card styles read. Self-contained: the
   *  sample films are built in-process so the page works regardless of cache
   *  state. */
  def tune(city: String): Action[AnyContent] = Action {
    withCity(city) { implicit c =>
      devOnly {
        Ok(views.html.tune(DebugController.tuneSampleFilms))
      }
    }
  }

  /** Dev-only tuning page for the film-detail view — live sliders over the real
   *  `_filmDetailContent` for the title / meta / Seanse typography. */
  def tuneFilm(city: String): Action[AnyContent] = Action {
    withCity(city) { implicit c =>
      devOnly {
        Ok(views.html.tuneFilm(DebugController.tuneSampleFilm))
      }
    }
  }

  /** Reload the in-memory read-model caches from Mongo. Available in every mode
   * (unlike the rest of the debug endpoints, which are dev-only) so a production
   * instance whose caches drifted from the derived collections can be reconciled
   * without a redeploy — but since it runs in prod and mutates state, it's gated
   * by [[AdminAction]] (login session + ADMIN_ALLOWLIST) rather than left open. */
  def rehydrate(city: String): Action[AnyContent] = adminAction {
    withCity(city) { _ =>
      val count = readModel.reload()
      Ok(s"rehydrated $count rows\n").as("text/plain; charset=utf-8")
    }
  }
}

object DebugController {

  /** Cap on the active tasks `/debug/queue` returns per poll — high enough to
   *  cover a backed-up enrichment queue so a pending movie's place is still
   *  resolvable, without an unbounded scan. */
  private val DebugQueueActiveLimit = 1000

  /** How many staging rows `/debug` renders. The header still shows the full
   *  `pending_movies` count; only the table is capped (and the page's live
   *  count-tracking JS caps appends to the same number). */
  val StagingRowLimit = 20

  /**
   * Order staging rows by their place in the durable queue — the same ranking
   * the /debug "Queue #" badge shows, so the rows that sort to the top (and thus
   * survive the `StagingRowLimit` cap) are the ones the worker is about to touch:
   *   1. a row with a worked-on `staging-*` task (▶ running) sorts first;
   *   2. then by best waiting place (1-based, oldest-first among waiting tasks);
   *   3. then queued-but-past-the-snapshot, then no-task last.
   * Ties keep the incoming order (the caller pre-sorts by title, cinema).
   *
   * `active`'s waiting tasks must be oldest-first, as `TaskQueue.monitor` returns
   * them (in one block, after the worked-on rows). This mirrors
   * the page's `waitingPlaces`/`badgeFor` JS (debug.scala.html) — keep the two in
   * sync so the server order and the live badge agree.
   */
  def orderStagingByQueue(
    staging: Seq[services.staging.StagingRecord],
    active:  Seq[services.tasks.TaskSummary],
    normalizer: TitleNormalizer,
  ): Seq[services.staging.StagingRecord] = {
    import services.tasks.TaskState
    // 1-based place of each waiting dedupKey among the waiting tasks (first seen).
    val waitingPlaces = {
      val b = scala.collection.mutable.LinkedHashMap.empty[String, Int]
      var i = 0
      active.foreach { t =>
        if (t.state == TaskState.Waiting) { i += 1; b.getOrElseUpdate(t.dedupKey, i) }
      }
      b.toMap
    }
    // Active `staging-*` tasks grouped by the film anchor their dedupKey embeds
    // (the segment after the `staging-*` prefix). Mirrors the JS `stagingTasksFor`.
    val byAnchor: Map[String, Seq[services.tasks.TaskSummary]] =
      active.flatMap { t =>
        if (t.taskType.startsWith("Staging")) t.dedupKey.split('|').lift(1).map(_ -> t) else None
      }.groupMap(_._1)(_._2)
    def rank(anchor: String): Double = byAnchor.get(anchor) match {
      case None | Some(Nil)                                        => Double.PositiveInfinity // no task
      case Some(ts) if ts.exists(_.state == TaskState.WorkedOn)    => 0d                       // ▶ running
      case Some(ts) =>
        val places = ts.flatMap(t => waitingPlaces.get(t.dedupKey))
        if (places.isEmpty) 1e9d else places.min.toDouble                                      // waiting / queued-past-snapshot
    }
    // sortBy is stable, so equal-rank rows keep the caller's (title, cinema) order.
    staging.sortBy(r => rank(normalizer.sanitize(r.title)))
  }

  /** Deterministic sample cards for the `/debug/tune` page — built in process
   *  so the tuning page renders the real `_movieCard` partial without depending
   *  on live cache contents. The set is a deliberate spread of edge cases so
   *  every pill row, rating variant, and vertical gap is on screen at once:
   *
   *   1. `rich`        — all four ratings (RT fresh), two cinemas, two days.
   *   2. `manyTimes`   — long wrapping title + 3 genres, one cinema with many
   *                      showtimes whose format tokens all differ, so the pills
   *                      wrap across several rows with wide format badges.
   *   3. `rotten`      — RT below 60 (the `.rotten` red variant) + a low
   *                      single-digit IMDb, so the rotten styling and the
   *                      narrowest rating values show.
   *   4. `extremes`    — the widest possible values: IMDb 10.0, Metacritic 100,
   *                      RT 100%, Filmweb 10.0 — stress-tests pill width.
   *   5. `metaOnly`    — only the Metacritic bare-number pill, alone on its row.
   *   6. `noRatings`   — no enrichment at all, so the ratings row is absent and
   *                      the meta→date gap collapses to just the title gap.
   *   7. `seniorClub`  — a programme-prefixed long title (the separate-row case)
   *                      with a single no-booking showtime (the `<span>` badge
   *                      variant, not the `<a>` one).
   *   8. `sparse`      — one rating, one cinema, one showtime: the loosest case.
   */
  private[controllers] def tuneSampleFilms: Seq[FilmSchedule] = {
    val base = LocalDate.of(2026, 6, 4)
    def at(d: LocalDate, h: Int, m: Int): LocalDateTime = d.atTime(h, m)

    def slot(d: LocalDate, h: Int, m: Int, fmt: List[String], booking: Boolean = true): Showtime =
      Showtime(
        at(d, h, m),
        bookingUrl = if (booking) Some("https://example.test/book") else None,
        room       = Some("Sala 1"),
        format     = fmt
      )

    // Build a resolved-movie sample directly (the web no longer holds
    // MovieRecords). Rating hrefs are placeholders — this page tunes layout, not
    // links — and `weightedRating` uses the production formula so the grid's
    // data-rating sort behaves as in prod.
    def res(
      title:     String,
      genres:    Seq[String],
      runtime:   Option[Int],
      year:      Option[Int],
      imdb:      Option[Double] = None,
      metascore: Option[Int]    = None,
      rt:        Option[Int]    = None,
      filmweb:   Option[Double] = None
    ): ResolvedMovie = {
      val weighted = {
        val ns = Seq(imdb, filmweb, metascore.map(_ / 10.0), rt.map(_ / 10.0)).flatten
        if (ns.isEmpty) 0.0 else ns.sum / ns.size
      }
      ResolvedMovie(
        _id = title, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
        runtimeMinutes = runtime, releaseYear = year, genres = genres, countries = Seq.empty,
        directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
        ratings = ResolvedRatings(
          imdb = imdb, imdbUrl = imdb.map(_ => "https://www.imdb.com/"),
          metascore = metascore, metacriticUrl = "https://www.metacritic.com/",
          rottenTomatoes = rt, rottenTomatoesUrl = "https://www.rottentomatoes.com/",
          filmweb = filmweb, filmwebUrl = "https://www.filmweb.pl/"
        ),
        weightedRating = weighted
      )
    }

    def film(resolved: ResolvedMovie, showings: Seq[(LocalDate, Seq[CinemaShowtimes])]): FilmSchedule =
      FilmSchedule(
        movie          = Movie(resolved.title, runtimeMinutes = resolved.runtimeMinutes, releaseYear = resolved.releaseYear, genres = resolved.genres),
        posterUrl      = resolved.posterUrl,
        synopsis       = resolved.synopsis,
        cast           = resolved.cast,
        director       = resolved.directors,
        cinemaFilmUrls = Seq.empty,
        showings       = showings,
        resolved       = resolved,
        slug           = FilmHref.slugOf(resolved.title),
        asOf           = base
      )

    val rich = film(
      res("Incepcja", Seq("Sci-Fi", "Akcja"), Some(148), Some(2010), imdb = Some(8.8), metascore = Some(74), rt = Some(87), filmweb = Some(7.6)),
      Seq(
        base -> Seq(
          CinemaShowtimes(Multikino, Seq(slot(base, 17, 30, List("2D", "NAP")), slot(base, 20, 15, List("2D")))),
          CinemaShowtimes(Helios,    Seq(slot(base, 18, 0, List("IMAX", "2D"))))
        ),
        base.plusDays(1) -> Seq(
          CinemaShowtimes(Multikino, Seq(slot(base.plusDays(1), 19, 45, List("2D", "DUB"))))
        )
      )
    )

    // One cinema, eight showtimes, every slot a different format token set so
    // none is stripped as "common" — the badges wrap to several rows and the
    // wide tokens (4DX, VOSE, ATMOS) stress the pill's max width.
    val manyTimes = film(
      res("Spider-Man: Poprzez multiwersum (wersja rozszerzona)", Seq("Animacja", "Akcja", "Przygodowy"), Some(140), Some(2023), imdb = Some(8.6), metascore = Some(86), rt = Some(95), filmweb = Some(7.9)),
      Seq(base -> Seq(CinemaShowtimes(CinemaCityKinepolis, Seq(
        slot(base, 10, 0,  List("2D", "DUB")),
        slot(base, 12, 30, List("3D", "DUB")),
        slot(base, 14, 15, List("IMAX", "NAP")),
        slot(base, 16, 0,  List("4DX")),
        slot(base, 18, 20, List("VOSE")),
        slot(base, 20, 0,  List("ATMOS", "NAP")),
        slot(base, 21, 30, List("2D", "NAP", "ATMOS")),
        slot(base, 23, 0,  List("3D"))
      ))))
    )

    val rotten = film(
      res("Morbius", Seq("Akcja", "Horror"), Some(104), Some(2022), imdb = Some(4.3), metascore = Some(35), rt = Some(15), filmweb = Some(4.1)),
      Seq(base -> Seq(CinemaShowtimes(Helios, Seq(slot(base, 19, 0, List("2D", "NAP"))))))
    )

    val extremes = film(
      res("Ojciec chrzestny", Seq("Dramat", "Kryminał"), Some(175), Some(1972), imdb = Some(10.0), metascore = Some(100), rt = Some(100), filmweb = Some(10.0)),
      Seq(base -> Seq(CinemaShowtimes(KinoPalacowe, Seq(slot(base, 16, 45, List("2D", "NAP"))))))
    )

    val metaOnly = film(
      res("Aftersun", Seq("Dramat"), Some(102), Some(2022), metascore = Some(95)),
      Seq(base -> Seq(CinemaShowtimes(KinoMuza, Seq(slot(base, 20, 30, List("NAP"))))))
    )

    val noRatings = film(
      res("Pokaz przedpremierowy: Niezatytułowany film", Seq("Dramat"), None, Some(2026)),
      Seq(base -> Seq(CinemaShowtimes(Rialto, Seq(slot(base, 18, 15, List("NAP"))))))
    )

    val seniorClub = film(
      res("Kino Seniora: Niebo nad Berlinem", Seq("Dramat", "Fantasy"), Some(128), Some(1987), imdb = Some(8.0), filmweb = Some(7.8)),
      Seq(base -> Seq(CinemaShowtimes(KinoApollo, Seq(slot(base, 12, 0, List("NAP"), booking = false)))))
    )

    val sparse = film(
      res("Cicha noc", Seq("Dramat"), Some(98), Some(2017), filmweb = Some(7.1)),
      Seq(base -> Seq(CinemaShowtimes(KinoMuza, Seq(slot(base, 21, 0, List("2D"))))))
    )

    Seq(rich, manyTimes, rotten, extremes, metaOnly, noRatings, seniorClub, sparse)
  }

  /** One fully-populated film (synopsis + cast + director, which the listing
   *  samples leave empty) for the `/debug/tune/movie` page, so every meta block
   *  renders and its fonts are tunable. Built off the rich sample's ratings +
   *  multi-cinema showings tree. */
  private[controllers] def tuneSampleFilm: FilmSchedule =
    tuneSampleFilms.head.copy(
      synopsis       = Some(
        "Dom Cobb to wytrawny złodziej, najlepszy w niebezpiecznej sztuce ekstrakcji — " +
        "wykradania cennych sekretów z głębi podświadomości podczas snu. Tym razem dostaje " +
        "szansę na odkupienie: zadanie odwrotne, zaszczepienie idei zamiast jej kradzieży. " +
        "Tekst celowo długi, by dało się dostroić rozmiar i odstępy opisu na ekranie filmu."
      ),
      cast           = Seq("Leonardo DiCaprio", "Joseph Gordon-Levitt", "Elliot Page", "Tom Hardy", "Ken Watanabe"),
      director       = Seq("Christopher Nolan"),
      cinemaFilmUrls = Seq(Multikino -> "https://example.test/incepcja")
    )
}
