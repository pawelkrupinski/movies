package tools

import models._
import services.cinemas.CinemaScraperCatalog
import services.cinemas.common.CinemaScraper
import services.cinemas.pl.FilmwebCinemaIdResolver.{Fuzzy, Override, OverrideSuppressed, Resolution, Source, Unmatched}
import services.cinemas.pl.{FilmwebCinemaIdResolver, FilmwebShowtimesClient}

import play.api.libs.json.Json

import java.io.PrintWriter
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
 * Diagnostic: compare OUR scrapers' screenings against Filmweb's, per cinema.
 *
 * Filmweb ids are resolved at RUNTIME via [[FilmwebCinemaIdResolver]] (fetch the
 * per-city showtimes listing, fuzzy-match each `Cinema.displayName` to a Filmweb
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
 * args: [daysAhead] [out.txt] [out.csv] [city-slug …] — order-independent for the
 * paths (a `*.txt` is the text report, a `*.csv` the summary); bare city slugs
 * restrict to those cities. Always exits 0 (per-cinema network errors are
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

  def main(args: Array[String]): Unit = {
    val daysAhead = args.headOption.flatMap(a => Try(a.toInt).toOption).getOrElse(3)
    val today     = LocalDate.now(ZoneId.of("Europe/Warsaw"))
    val windowEnd = today.plusDays(daysAhead.toLong)

    // Path args: a `*.txt` → text report, a `*.csv` → summary CSV, a `*.json` →
    // the deterministic JSON report. Bare slugs (matching a known city) restrict
    // the diff to those cities. Order-independent: classified by extension, so the
    // documented `<days> <txt> <csv> <json>` order works and omitting the json
    // path just defaults it (doesn't break older 2-3 arg invocations).
    val knownSlugs  = City.all.map(_.slug).toSet
    val pathArgs    = args.tail.filter(a => a.endsWith(".txt") || a.endsWith(".csv") || a.endsWith(".json") || a.startsWith("/"))
    val cityFilter  = args.tail.filter(knownSlugs).toSet
    val txtPath     = pathArgs.find(_.endsWith(".txt")).getOrElse("/temporary/filmweb-diff-output.txt")
    val csvPath     = pathArgs.find(_.endsWith(".csv")).getOrElse("/temporary/filmweb-diff-summary.csv")
    val jsonPath    = pathArgs.find(_.endsWith(".json")).getOrElse("/temporary/filmweb-diff-output.json")

    val http     = new RealHttpFetch()
    val catalog  = new CinemaScraperCatalog(http, today = today)
    val resolver = new FilmwebCinemaIdResolver(http)

    val out = new StringBuilder
    def emit(line: String): Unit = { println(line); out.append(line).append('\n'); () }

    emit(s"FilmwebDiff — window [$today .. $windowEnd] ($daysAhead days ahead)")
    if (cityFilter.nonEmpty) emit(s"Cities: ${cityFilter.toSeq.sorted.mkString(", ")}")
    emit("")

    // ── RESOLUTION ───────────────────────────────────────────────────────────
    // Resolve every (in-scope) cinema's Filmweb id live, then report how each
    // resolved. Unmatched cinemas are listed, not errors.
    val resolutions = resolver.resolveAll(cityFilter)
    emit("══════════════════════════════ RESOLUTION ═══════════════════════════")
    emit(resolutionReport(resolutions))
    emit("")

    val resolutionByCinema: Map[Cinema, Resolution] =
      resolutions.map(r => r.cinema -> r).toMap
    val fwIdByCinema: Map[Cinema, Int] =
      resolutions.collect { case Resolution(c, Some(id), _) => c -> id }.toMap

    // Our scrapers for the cinemas with a resolved Filmweb id, in city order.
    val ourScrapers: Seq[CinemaScraper] =
      catalog.all.filter(s => fwIdByCinema.contains(s.cinema))

    emit(s"Cinemas compared: ${ourScrapers.size}")
    emit("")

    // OUR side fans out across many independent hosts (bok.waw.pl, kinomikro.pl,
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
    val comparedDiffs: Seq[CinemaDiff] = ourScrapers.zipWithIndex.map { case (scraper, i) =>
      if (i > 0) Thread.sleep(if (consecutiveFwFailures >= 3) 8000L else 1500L)
      val cinema = scraper.cinema
      emit(s"… ${cinema.displayName}")

      val oursTry = ourResults(cinema)
      val fwTry   = Try(new FilmwebShowtimesClient(http, fwIdByCinema(cinema), cinema, daysAhead, today).fetch())

      fwTry match {
        case Failure(_) => consecutiveFwFailures += 1
        case Success(_) => consecutiveFwFailures = 0
      }

      val r = resolutionByCinema(cinema)
      diffFor(cinema, r.filmwebId, r.source, oursTry, fwTry, today, windowEnd)
    }

    // Cinemas with NO Filmweb id never reach the comparison loop, but they DO
    // belong in the JSON (counts 0, verdict NO_FILMWEB_ID, empty discrepancies)
    // so the set of cinemas is stable across days. Not added to the text/CSV —
    // those already list them in the RESOLUTION section.
    val unresolvedDiffs: Seq[CinemaDiff] =
      resolutions.filterNot(_.resolved).map { r =>
        CinemaDiff(r.cinema, None, r.source, 0, 0, 0, 0, 0, NO_FILMWEB_ID, "", Map.empty, Map.empty)
      }

    val diffs = comparedDiffs

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

    writeFile(txtPath, out.toString) match {
      case Success(_) => emit(s"\nText report written to $txtPath")
      case Failure(e) => emit(s"\nCould not write $txtPath: ${e.getMessage}")
    }
    writeFile(csvPath, csvReport(today, diffs)) match {
      case Success(_) => emit(s"CSV summary written to $csvPath")
      case Failure(e) => emit(s"Could not write $csvPath: ${e.getMessage}")
    }

    // JSON includes the unresolved cinemas too, so its `cinemas` set is stable.
    val meta = FilmwebDiffJson.Meta(
      date        = today.toString,
      generatedAt = Instant.now().toString,
      windowDays  = daysAhead,
      commit      = sys.env.getOrElse("GITHUB_SHA", "unknown")
    )
    val json = Json.prettyPrint(FilmwebDiffJson.render((diffs ++ unresolvedDiffs).map(cinemaResult), meta))
    writeFile(jsonPath, json + "\n") match {
      case Success(_) => emit(s"JSON report written to $jsonPath")
      case Failure(e) => emit(s"Could not write $jsonPath: ${e.getMessage}")
    }
  }

  /** Map a per-cinema [[CinemaDiff]] to the JSON renderer's input. */
  private def cinemaResult(d: CinemaDiff): FilmwebDiffJson.CinemaResult =
    FilmwebDiffJson.CinemaResult(
      city        = cityOf(d.cinema).map(_.slug).getOrElse(""),
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
          case Unmatched          => "no fuzzy match in city listing"
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
      val city = cityOf(d.cinema).map(_.slug).getOrElse("")
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

  private def cityOf(cinema: Cinema): Option[City] =
    City.all.find(_.cinemas.contains(cinema))

  /** Restrict a side's showtimes to the comparison window and key by film. */
  private def withinWindow(
    movies: Seq[CinemaMovie], today: LocalDate, windowEnd: LocalDate
  ): Map[String, Seq[LocalDateTime]] = {
    val startOfDay = today.atStartOfDay()
    val endOfDay   = windowEnd.plusDays(1).atStartOfDay() // exclusive: whole windowEnd day
    movies
      .map(m => FilmwebDiffTitleNormalizer.normalize(m.movie.title) -> m.showtimes.map(_.dateTime)
        .filter(dt => !dt.isBefore(startOfDay) && dt.isBefore(endOfDay)))
      .filter(_._2.nonEmpty)
      .groupMapReduce(_._1)(_._2)(_ ++ _)
  }

  private def diffFor(
    cinema:    Cinema,
    filmwebId: Option[Int],
    source:    Source,
    oursTry:   Try[Seq[CinemaMovie]],
    fwTry:     Try[Seq[CinemaMovie]],
    today:     LocalDate,
    windowEnd: LocalDate
  ): CinemaDiff = (oursTry, fwTry) match {
    case (Failure(e), _) =>
      CinemaDiff(cinema, filmwebId, source, 0, 0, 0, 0, 0, OUR_FETCH_FAILED,
        s"  OUR fetch failed: ${message(e)}", Map.empty, Map.empty)
    case (_, Failure(e)) =>
      CinemaDiff(cinema, filmwebId, source, 0, 0, 0, 0, 0, FW_FETCH_FAILED,
        s"  Filmweb fetch failed: ${message(e)}", Map.empty, Map.empty)
    case (Success(ours), Success(fw)) =>
      val oursByFilm = withinWindow(ours, today, windowEnd)
      val fwByFilm   = withinWindow(fw, today, windowEnd)

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
