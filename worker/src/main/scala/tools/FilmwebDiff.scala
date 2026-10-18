package tools

import models._
import services.cinemas.CinemaScraperCatalog
import services.cinemas.common.CinemaScraper
import services.cinemas.pl.FilmwebCinemaIdResolver.{Fuzzy, Override, OverrideSuppressed, Resolution, Source, Unmatched}
import services.cinemas.pl.{FilmwebCinemaIdResolver, FilmwebShowtimesClient}

import play.api.libs.json.Json

import java.io.PrintWriter
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
 * Diagnostic: compare OUR scrapers' screenings against Filmweb's, per cinema.
 *
 * Filmweb ids are resolved at RUNTIME via [[FilmwebCinemaIdResolver]] (fetch each
 * venue's town's cinema listing, fuzzy-match each `Cinema.displayName` to a Filmweb
 * cinema → its id; a small override map wins first). So a cinema added to the
 * model/catalog auto-joins the diff with no hand-edited id table; a cinema with
 * no Filmweb listing is reported `NO_FILMWEB_ID`, not an error.
 *
 * For every resolved cinema, fetch both sides — our real `CinemaScraper`, and a
 * `FilmwebShowtimesClient` pointed at Filmweb's JSON seances API — restrict both
 * to the `[today, today+daysAhead]` window, and report per cinema: total
 * showtimes each side, the shared / ours-only / fw-only `LocalDateTime` multisets
 * (all films), and a per-film (normalised title) breakdown. The point is to
 * surface where our scrape is thin (Filmweb has screenings we miss) or stale (we
 * list screenings Filmweb dropped), so the gaps can be chased one cinema at a time.
 *
 * Run by hand or daily in CI (`.github/workflows/filmweb-diff.yml`):
 *   sbt "worker/runMain tools.FilmwebDiff 3 out.txt out.csv [city-slug …]"
 * args: [daysAhead] [out.txt] [out.csv] [out.json] [city-slug …] — order-independent for the
 * paths (a `*.txt` is the text report, a `*.csv` the summary, a `*.json` the deterministic
 * report; each defaulted when omitted); bare city slugs
 * restrict to those cities; anything else stops it with exit 2 before any fetch
 * (`Options.parse`). Past that it always exits 0 (per-cinema network errors are
 * Try-wrapped and reported, never fatal) so a scheduled job's artifact uploads.
 *
 * `def main`, not `extends App`: App-body static-init throws (and pages Sentry
 * through the Play logback appender) on the network timeouts this tool provokes.
 *
 * Filmweb soft-blocks aggressive callers by holding sockets open, so cinemas
 * are processed SEQUENTIALLY with a small inter-cinema sleep (each client's own
 * ParallelDetailFetch gives enough per-cinema concurrency); a run of consecutive
 * Filmweb failures backs off harder.
 */
object FilmwebDiff {

  // How many OUR-side scrapes run at once. Each hits a different cinema host and
  // fans its own detail fetches out internally, so a handful of concurrent
  // scrapers keeps the GitHub runner's cores busy without burying any single
  // upstream. The Filmweb side is NOT covered by this — it stays sequential.
  private val OurFetchWorkers = 6

  // Multiset ops over LocalDateTime live in FilmwebDiffJson, shared so the
  // text/CSV path and the JSON path agree on shared/ours-only/fw-only counts.
  import FilmwebDiffJson.{multisetIntersect, multisetDiff}

  sealed trait Verdict
  case object SAME             extends Verdict
  case object OURS_EXTRA       extends Verdict
  case object FW_EXTRA         extends Verdict
  case object BOTH_DIFFER      extends Verdict
  case object OUR_FETCH_FAILED extends Verdict
  case object FW_FETCH_FAILED  extends Verdict
  case object NO_FILMWEB_ID    extends Verdict

  /** One cinema's comparison result. Times are multisets (Seq, not Set) so a
   *  duplicate slot on one side counts as a difference. `oursByFilm`/`fwByFilm`
   *  carry the same normalised-title → screening-time maps the JSON renderer
   *  needs, so text, CSV, and JSON all agree. */
  private case class CinemaDiff(
    cinema:     Cinema,
    filmwebId:  Option[Int],
    source:     Source,
    oursCount:  Int,
    fwCount:    Int,
    shared:     Int,
    oursOnly:   Int,
    fwOnly:     Int,
    verdict:    Verdict,
    detail:     String,
    oursByFilm: Map[String, Seq[LocalDateTime]],
    fwByFilm:   Map[String, Seq[LocalDateTime]]
  )

  /** The run asked: the days ahead, the three report paths, and the cities it is restricted to (none: all). */
  final case class Options(daysAhead: Int = 3, txt: String = "/temporary/filmweb-diff-output.txt",
                           csv: String = "/temporary/filmweb-diff-summary.csv", json: String = "/temporary/filmweb-diff-output.json",
                           cities: Set[String] = Set.empty)

  /** `[daysAhead] [out.txt] [out.csv] [out.json] [city-slug …]`, the paths classified by extension in any order — or
   *  why not. (A worker `main` tool: testkit's CommandLine is not on its classpath.) The old reading dropped whatever
   *  it could not classify: a mistyped city slug diffed every city, a days count that was no number diffed 3 days, and
   *  a path given first was taken for the days and lost. */
  def parse(args: Seq[String], knownSlugs: Set[String]): Either[String, Options] = {
    val (days, rest) = args.headOption.flatMap(_.toIntOption) match {
      case Some(n) if n >= 0 => (n, args.drop(1))
      case _                 => (Options().daysAhead, args)
    }
    rest.foldLeft(Right(Options(daysAhead = days)): Either[String, Options]) { (read, arg) =>
      read.flatMap { o =>
        def path(taken: Boolean, set: Options => Options, kind: String) =
          if (taken) Left(s"a second $kind report path: $arg") else Right(set(o))
        val defaults = Options()
        if (arg.endsWith(".txt"))       path(o.txt != defaults.txt, _.copy(txt = arg), "text")
        else if (arg.endsWith(".csv"))  path(o.csv != defaults.csv, _.copy(csv = arg), "CSV")
        else if (arg.endsWith(".json")) path(o.json != defaults.json, _.copy(json = arg), "JSON")
        else if (knownSlugs(arg))       Right(o.copy(cities = o.cities + arg))
        else Left(s"'$arg' is no report path (*.txt, *.csv, *.json), Polish city slug, or leading days count")
      }
    }
  }

  /** The text report as it is printed: every line echoed to stdout and kept for the `*.txt` file. */
  private final class Report {
    private val out = new StringBuilder
    def emit(line: String): Unit = { println(line); out.append(line).append('\n'); () }
    def text: String = out.toString
  }

  /** One run's window — `[today, windowEnd]`, Poland's `now` — and what it reads both sides with. */
  private final case class Run(options: Options, clock: java.time.Clock, now: LocalDateTime, http: HttpFetch,
                               titles: services.movies.TitleNormalizer, catalog: CinemaScraperCatalog,
                               commit: String, report: Report) {
    def today: LocalDate     = now.toLocalDate
    def daysAhead: Int       = options.daysAhead
    def windowEnd: LocalDate = today.plusDays(daysAhead.toLong)
    def emit(line: String): Unit = report.emit(line)
  }

  def main(args: Array[String]): Unit = {
    val knownSlugs = models.Country.Poland.cities.map(_.slug).toSet   // Filmweb is Poland's
    val options    = parse(args.toSeq, knownSlugs).fold(why => { System.err.println(s"FilmwebDiff: $why"); sys.exit(2) }, identity)
    val run        = setUp(options)
    import run.emit

    emit(s"FilmwebDiff — window [${run.today} .. ${run.windowEnd}] (${run.daysAhead} days ahead)")
    if (options.cities.nonEmpty) emit(s"Cities: ${options.cities.toSeq.sorted.mkString(", ")}")
    emit("")

    // ── RESOLUTION ───────────────────────────────────────────────────────────
    // Resolve every (in-scope) cinema's Filmweb id live, then report how each
    // resolved. Unmatched cinemas are listed, not errors.
    val resolutions = new FilmwebCinemaIdResolver(run.http).resolveAll(options.cities)
    emit("══════════════════════════════ RESOLUTION ═══════════════════════════")
    emit(resolutionReport(resolutions))
    emit("")

    val diffs = compared(run, resolutions)
    // Cinemas with NO Filmweb id never reach the comparison loop, but they DO
    // belong in the JSON (counts 0, verdict NO_FILMWEB_ID, empty discrepancies)
    // so the set of cinemas is stable across days. Not added to the text/CSV —
    // those already list them in the RESOLUTION section.
    val unresolvedDiffs: Seq[CinemaDiff] =
      resolutions.filterNot(_.resolved).map { r =>
        CinemaDiff(r.cinema, None, r.source, 0, 0, 0, 0, 0, NO_FILMWEB_ID, "", Map.empty, Map.empty)
      }
    emitSummaryAndDetail(run, diffs)
    writeReports(run, diffs, unresolvedDiffs)
  }

  /** The tool's composition root: the JVM's egress settings, its one clock, and the catalog over the
   *  residential proxy. */
  private def setUp(options: Options): Run = {
    // Before any handshake — see IssuerCertificateFetching.
    // Before any request: Multikino / biletyna / ck105 tunnel through the residential proxy,
    // and the JDK reads this once — see ProxyTunnelAuthentication.
    ProxyTunnelAuthentication.BasicAllowed.applyToJvm()
    IssuerCertificateFetching.Enabled.applyToJvm()
    val process = settings.ProcessConfiguration.resolve()
    val clock   = java.time.Clock.systemUTC()   // the tool's composition root: its one clock, handed down
    val now     = new models.VenueClock(clock).now(models.TimeZones.Poland)
    val http    = new RealHttpFetch()
    // Filmweb is Polish-only, so a Filmweb title is a Polish title by definition.
    val titles  = services.movies.TitleNormalizer.forCountry(models.Country.Poland)
    // Multikino, biletyna and ck105 refuse a datacenter runner: through the residential
    // proxy, as the worker reaches Multikino and biletyna, when the Decodo credentials are set.
    val shards  = modules.wiring.EgressWiring.residentialShards(ResidentialProxy.fromConfiguration(process), TlsTrust.newContext())
    val catalog = new CinemaScraperCatalog(http, venueClock = models.VenueClock.fixedOn(now.toLocalDate), clock = clock,
      titles = titles, proxyShards = shards)
    Run(options, clock, now, http, titles, catalog, process.githubCommit.value, new Report)
  }

  /** Both sides of every cinema with a resolved Filmweb id, in city order, diffed. */
  private def compared(run: Run, resolutions: Seq[Resolution]): Seq[CinemaDiff] = {
    import run.emit
    val resolutionByCinema: Map[Cinema, Resolution] = resolutions.map(r => r.cinema -> r).toMap
    val fwIdByCinema: Map[Cinema, Int] = resolutions.collect { case Resolution(c, Some(id), _) => c -> id }.toMap
    val ourScrapers: Seq[CinemaScraper] = run.catalog.all.filter(s => fwIdByCinema.contains(s.cinema))
    emit(s"Cinemas compared: ${ourScrapers.size}")
    emit("")

    // OUR side fans out across many independent hosts (bok.waw.pl, bilety.kinomikro.pl,
    // helios.pl, …), so it parallelises freely — each scraper already throttles
    // its own per-cinema detail fetches via ParallelDetailFetch. Pre-fetch all
    // of them on a bounded pool so the slow scrapers (Bok now walks a week of
    // day pages; Helios/Multikino are heavy) overlap instead of summing. The
    // FILMWEB side stays strictly sequential below — Filmweb soft-blocks
    // aggressive callers by holding sockets open, so hammering its seances API
    // in parallel is exactly what we must not do.
    emit(s"Pre-fetching our side for ${ourScrapers.size} cinema(s) (≤$OurFetchWorkers at once)…")
    val ourResults: Map[Cinema, Try[Seq[CinemaMovie]]] = fetchOursInParallel(ourScrapers)

    var consecutiveFwFailures = 0
    ourScrapers.zipWithIndex.map { case (scraper, i) =>
      if (i > 0) Thread.sleep(if (consecutiveFwFailures >= 3) 8000L else 1500L)
      val cinema = scraper.cinema
      emit(s"… ${cinema.displayName}")
      val fwTry = Try(new FilmwebShowtimesClient(run.http, fwIdByCinema(cinema), cinema, run.daysAhead, run.today).fetch())
      consecutiveFwFailures = if (fwTry.isFailure) consecutiveFwFailures + 1 else 0
      val r = resolutionByCinema(cinema)
      diffFor(cinema, r.filmwebId, r.source, ourResults(cinema), fwTry, run.now, run.windowEnd, run.titles)
    }
  }

  private def emitSummaryAndDetail(run: Run, diffs: Seq[CinemaDiff]): Unit = {
    import run.emit
    emit("")
    emit("══════════════════════════════ SUMMARY ══════════════════════════════")
    emit(summaryTable(diffs))
    emit("")
    emit("══════════════════════════════ DETAIL ═══════════════════════════════")
    diffs.foreach { d =>
      emit("")
      emit(s"══ ${d.cinema.displayName}  [${verdictLabel(d.verdict)}] ══")
      emit(d.detail)
    }
  }

  /** The `*.txt` text report, the `*.csv` summary and the deterministic `*.json` — which includes the
   *  unresolved cinemas too, so its `cinemas` set is stable. */
  private def writeReports(run: Run, diffs: Seq[CinemaDiff], unresolvedDiffs: Seq[CinemaDiff]): Unit = {
    val options = run.options
    writeReport(run, options.txt, run.report.text, s"\nText report written to ${options.txt}", "\n")
    writeReport(run, options.csv, csvReport(run.today, diffs), s"CSV summary written to ${options.csv}")
    val meta = FilmwebDiffJson.Meta(
      date        = run.today.toString,
      generatedAt = run.clock.instant().toString,
      windowDays  = run.daysAhead,
      commit      = run.commit
    )
    val json = Json.prettyPrint(FilmwebDiffJson.render((diffs ++ unresolvedDiffs).map(cinemaResult), meta))
    writeReport(run, options.json, json + "\n", s"JSON report written to ${options.json}")
  }

  private def writeReport(run: Run, path: String, content: String, written: String, lead: String = ""): Unit =
    writeFile(path, content) match {
      case Success(_) => run.emit(written)
      case Failure(e) => run.emit(s"${lead}Could not write $path: ${e.getMessage}")
    }

  /** Map a per-cinema [[CinemaDiff]] to the JSON renderer's input. */
  private def cinemaResult(d: CinemaDiff): FilmwebDiffJson.CinemaResult =
    FilmwebDiffJson.CinemaResult(
      city        = City.forCinema(d.cinema).map(_.slug).getOrElse(""),
      cinema      = d.cinema.displayName,
      filmwebId   = d.filmwebId,
      resolvedVia = resolvedVia(d.source),
      verdict     = verdictLabel(d.verdict),
      oursByFilm  = d.oursByFilm,
      fwByFilm    = d.fwByFilm
    )

  private def resolvedVia(source: Source): String = source match {
    case Override       => "override"
    case Fuzzy(_, _)    => "fuzzy"
    case _              => "none"
  }

  /** Run every scraper's `fetch()` on a bounded pool, each result `Try`-wrapped
   *  (a thrown scraper can't sink the batch) and keyed back to its own cinema.
   *  The keying is by `scraper.cinema`, so parallelism can't misattribute one
   *  cinema's films to another. `package`-visible for direct testing. */
  private[tools] def fetchOursInParallel(scrapers: Seq[CinemaScraper]): Map[Cinema, Try[Seq[CinemaMovie]]] = {
    if (scrapers.isEmpty) return Map.empty
    given pool: scala.concurrent.ExecutionContextExecutorService =
      DaemonExecutors.boundedEC("fwdiff-ours", OurFetchWorkers)
    try {
      val futures = scrapers.map(scraper => Future(scraper.cinema -> Try(scraper.fetch())))
      Await.result(Future.sequence(futures), Duration.Inf).toMap
    } finally pool.shutdown()
  }

  private def writeFile(path: String, content: String): Try[Unit] = Try {
    val pw = new PrintWriter(path)
    try pw.write(content) finally pw.close()
  }

  /** The RESOLUTION section: each cinema → its resolved id and how (override /
   *  fuzzy), then a clear list of cinemas with NO Filmweb id. */
  private def resolutionReport(resolutions: Seq[Resolution]): String = {
    val (resolved, unresolved) = resolutions.partition(_.resolved)
    val nameW = resolutions.map(_.cinema.displayName.length).maxOption.getOrElse(20)
    val sb = new StringBuilder
    resolved.foreach { r =>
      val how = r.source match {
        case Override               => "override"
        case Fuzzy(name, score)     => f"fuzzy  ($score%.2f ← “$name”)"
        case _                      => ""
      }
      sb.append(f"  ${r.cinema.displayName.padTo(nameW, ' ')}  -> ${r.filmwebId.get}%5d  $how%n")
    }
    if (unresolved.nonEmpty) {
      sb.append(s"\n  NO_FILMWEB_ID (${unresolved.size}) — no Filmweb listing / data, excluded from diff:\n")
      unresolved.foreach { r =>
        val why = r.source match {
          case OverrideSuppressed => "override: no usable Filmweb data"
          case Unmatched          => "no fuzzy match in its town's listing"
          case _                  => ""
        }
        sb.append(s"    - ${r.cinema.displayName}  ($why)\n")
      }
    }
    sb.toString.stripLineEnd
  }

  /** Machine-readable per-cinema summary: one row per compared cinema. */
  private def csvReport(today: LocalDate, diffs: Seq[CinemaDiff]): String = {
    val header = "date,city,cinema,ours,fw,shared,ours_only,fw_only,verdict"
    val rows = diffs.map { d =>
      val city = City.forCinema(d.cinema).map(_.slug).getOrElse("")
      List(today.toString, city, csvField(d.cinema.displayName),
        d.oursCount, d.fwCount, d.shared, d.oursOnly, d.fwOnly, verdictLabel(d.verdict))
        .mkString(",")
    }
    (header +: rows).mkString("\n") + "\n"
  }

  /** RFC-4180-ish quoting: wrap in quotes and double internal quotes when the
   *  field carries a comma/quote/newline. Cinema names can contain commas. */
  private def csvField(s: String): String =
    if (s.exists(c => c == ',' || c == '"' || c == '\n')) "\"" + s.replace("\"", "\"\"") + "\"" else s

  /** Restrict a side's showtimes to the comparison window and key by film.
   *
   *  The lower bound is `Showtime.isUpcoming(now)` — the SAME grace-period rule
   *  the web's `toSchedules` and the worker's source-films gauge use to decide
   *  "is this still showing" — applied identically to BOTH sides, not just a
   *  bare `today` cutoff. Without this, a showtime earlier today that already
   *  elapsed drops out of OUR live scrape (a source's feed stops listing a
   *  screening once it starts) well before Filmweb's cached listing catches up,
   *  and every one of those showed up as a phantom `fw-only` gap — entirely an
   *  artifact of when in the day the diff happens to run, worst right before
   *  midnight (confirmed: a 2026-09-13 23:33 manual run over-reported ~10 such
   *  gaps at Kino Mikro alone that the routine 06:00 run never sees). */
  private[tools] def withinWindow(
    movies: Seq[CinemaMovie], now: LocalDateTime, windowEnd: LocalDate, titles: services.movies.TitleNormalizer
  ): Map[String, Seq[LocalDateTime]] = {
    val endOfDay = windowEnd.plusDays(1).atStartOfDay() // exclusive: whole windowEnd day
    movies
      .map(m => FilmwebDiffTitleNormalizer.normalize(m.movie.title, titles) -> m.showtimes
        .filter(s => s.isUpcoming(now) && s.dateTime.isBefore(endOfDay))
        .map(_.dateTime))
      .filter(_._2.nonEmpty)
      .groupMapReduce(_._1)(_._2)(_ ++ _)
  }

  private def diffFor(
    cinema:    Cinema,
    filmwebId: Option[Int],
    source:    Source,
    oursTry:   Try[Seq[CinemaMovie]],
    fwTry:     Try[Seq[CinemaMovie]],
    now:       LocalDateTime,
    windowEnd: LocalDate,
    titles:    services.movies.TitleNormalizer
  ): CinemaDiff = (oursTry, fwTry) match {
    case (Failure(e), _) =>
      CinemaDiff(cinema, filmwebId, source, 0, 0, 0, 0, 0, OUR_FETCH_FAILED,
        s"  OUR fetch failed: ${message(e)}", Map.empty, Map.empty)
    case (_, Failure(e)) =>
      CinemaDiff(cinema, filmwebId, source, 0, 0, 0, 0, 0, FW_FETCH_FAILED,
        s"  Filmweb fetch failed: ${message(e)}", Map.empty, Map.empty)
    case (Success(ours), Success(fw)) =>
      val oursByFilm = withinWindow(ours, now, windowEnd, titles)
      val fwByFilm   = withinWindow(fw, now, windowEnd, titles)

      val oursTimes = oursByFilm.values.flatten.toSeq
      val fwTimes   = fwByFilm.values.flatten.toSeq
      val shared    = multisetIntersect(oursTimes, fwTimes)
      val oursOnly  = multisetDiff(oursTimes, fwTimes)
      val fwOnly    = multisetDiff(fwTimes, oursTimes)

      val verdict =
        if (oursOnly.isEmpty && fwOnly.isEmpty) SAME
        else if (fwOnly.isEmpty) OURS_EXTRA
        else if (oursOnly.isEmpty) FW_EXTRA
        else BOTH_DIFFER

      CinemaDiff(
        cinema, filmwebId, source, oursTimes.size, fwTimes.size, shared.size, oursOnly.size, fwOnly.size,
        verdict, perFilmDetail(oursByFilm, fwByFilm), oursByFilm, fwByFilm
      )
  }

  /** Per-film breakdown: matched films with their time deltas, then films only
   *  on one side. */
  private def perFilmDetail(
    oursByFilm: Map[String, Seq[LocalDateTime]],
    fwByFilm:   Map[String, Seq[LocalDateTime]]
  ): String = {
    val sb = new StringBuilder
    val matched = (oursByFilm.keySet & fwByFilm.keySet).toSeq.sorted
    val oursOnlyFilms = (oursByFilm.keySet -- fwByFilm.keySet).toSeq.sorted
    val fwOnlyFilms   = (fwByFilm.keySet -- oursByFilm.keySet).toSeq.sorted

    matched.foreach { title =>
      val o = oursByFilm(title); val f = fwByFilm(title)
      val oOnly = multisetDiff(o, f); val fOnly = multisetDiff(f, o)
      if (oOnly.isEmpty && fOnly.isEmpty)
        sb.append(s"  = $title  (${o.size} slots, identical)\n")
      else {
        sb.append(s"  ~ $title  ours=${o.size} fw=${f.size}\n")
        if (oOnly.nonEmpty) sb.append(s"      ours-only: ${fmtTimes(oOnly)}\n")
        if (fOnly.nonEmpty) sb.append(s"      fw-only:   ${fmtTimes(fOnly)}\n")
      }
    }
    oursOnlyFilms.foreach(t => sb.append(s"  - [ours only] $t  (${oursByFilm(t).size} slots)\n"))
    fwOnlyFilms.foreach(t => sb.append(s"  + [fw only]   $t  (${fwByFilm(t).size} slots)\n"))
    if (sb.isEmpty) "  (no screenings in window on either side)" else sb.toString.stripLineEnd
  }

  private def fmtTimes(times: Seq[LocalDateTime]): String =
    times.sorted.map(t => t.toLocalDate.toString.drop(5) + " " + "%02d:%02d".format(t.getHour, t.getMinute))
      .mkString(", ")

  private def summaryTable(diffs: Seq[CinemaDiff]): String = {
    val nameW = (diffs.map(_.cinema.displayName.length).maxOption.getOrElse(20)).max("cinema".length)
    val header = f"  ${"cinema".padTo(nameW, ' ')}  ${"ours#"}%6s  ${"fw#"}%6s  ${"shared"}%6s  ${"ours-only"}%9s  ${"fw-only"}%7s  verdict"
    val rows = diffs.map { d =>
      f"  ${d.cinema.displayName.padTo(nameW, ' ')}  ${d.oursCount}%6d  ${d.fwCount}%6d  ${d.shared}%6d  ${d.oursOnly}%9d  ${d.fwOnly}%7d  ${verdictLabel(d.verdict)}"
    }
    (header +: rows).mkString("\n")
  }

  private def verdictLabel(v: Verdict): String = v match {
    case SAME             => "SAME"
    case OURS_EXTRA       => "OURS_EXTRA"
    case FW_EXTRA         => "FW_EXTRA"
    case BOTH_DIFFER      => "BOTH_DIFFER"
    case OUR_FETCH_FAILED => "OUR_FETCH_FAILED"
    case FW_FETCH_FAILED  => "FW_FETCH_FAILED"
    case NO_FILMWEB_ID    => "NO_FILMWEB_ID"
  }

  private def message(e: Throwable): String =
    Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
}
