package tools

import models._
import services.cinemas.{CinemaScraperCatalog, CinemaScraper, FilmwebShowtimesClient}

import java.io.PrintWriter
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.util.{Failure, Success, Try}

/**
 * Diagnostic: compare OUR scrapers' screenings against Filmweb's, per cinema.
 *
 * For every modelled cinema that Filmweb also lists (`filmwebCinemaIds`), fetch
 * both sides — our real `CinemaScraper`, and a `FilmwebShowtimesClient` pointed
 * at Filmweb's JSON seances API — restrict both to the `[today, today+daysAhead]`
 * window, and report per cinema: total showtimes each side, the shared /
 * ours-only / fw-only `LocalDateTime` multisets (all films), and a per-film
 * (normalised title) breakdown. The point is to surface where our scrape is
 * thin (Filmweb has screenings we miss) or stale (we list screenings Filmweb
 * dropped), so the gaps can be chased one cinema at a time.
 *
 * NOT a test or a scheduled job — a one-shot, run by hand:
 *   sbt "worker/runMain tools.FilmwebDiff 3"
 * args: [daysAhead] [city-slug …]   (output also written to a /tmp file)
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

  /**
   * Our modelled cinema → Filmweb's internal cinema id (the `/showtimes/<City>/
   * <Name>-<id>` URL id, verified to equal the `/api/v1/cinema/<id>/seances`
   * id). Built from Filmweb's per-city showtimes listings (2026-06, bounded:
   * one page per city); ids spot-checked against the seances API.
   *
   * Cinemas Filmweb does NOT list — excluded from the diff:
   *   - Warszawa: Cinema City Galeria Północna, KINOMUZEUM, Kino Kępa,
   *     Służewski Dom Kultury (none appear on /showtimes/Warszawa).
   *   - Trójmiasto: Kino IKM (no IKM/Instytut entry on /showtimes/Gdańsk).
   * Fuzzy name matches (id verified, display name differs):
   *   - KinoApollo ← "Kino Teatr Apollo" (3025); the only Apollo in Poznań.
   *   - MultikinoReduta ← "Multikino Atrium Reduta" (2119)
   *   - MultikinoWolaPark ← "Multikino Wola" (1380)
   *   - KinoAmondo ← "Amondo Kino" (2077)
   *   - HeliosRiviera ← Gdynia "Helios" (1775)
   */
  val filmwebCinemaIds: Map[Cinema, Int] = Map(
    // ── Poznań ──
    Multikino             -> 633,
    CinemaCityKinepolis   -> 624,
    CinemaCityPoznanPlaza -> 568,
    KinoBulgarska         -> 1618,
    CharlieMonroe         -> 1499,
    Helios                -> 1943,
    KinoPalacowe          -> 1854,
    KinoMuza              -> 75,
    Rialto                -> 78,
    KinoApollo            -> 3025, // "Kino Teatr Apollo"
    // ── Wrocław ──
    CinemaCityWroclavia   -> 2019,
    CinemaCityKorona      -> 169,
    MultikinoPasazGrunwaldzki -> 638,
    HeliosMagnolia        -> 1133,
    HeliosAlejaBielany    -> 1911,
    KinoNoweHoryzonty     -> 1737,
    DolnoslaskieCentrumFilmowe -> 1475,
    // ── Warszawa ──  (Galeria Północna, KINOMUZEUM, Kino Kępa, SDK absent)
    CinemaCityArkadia     -> 556,
    CinemaCityBemowo      -> 199,
    CinemaCityJanki       -> 175,
    CinemaCityMokotow     -> 196,
    CinemaCityPromenada   -> 144,
    CinemaCitySadyba      -> 172,
    MultikinoZloteTarasy  -> 656,
    MultikinoMlociny      -> 2139,
    MultikinoReduta       -> 2119, // "Multikino Atrium Reduta"
    MultikinoTargowek     -> 1378,
    MultikinoWolaPark     -> 1380, // "Multikino Wola"
    HeliosBlueCity        -> 2126,
    KinoMuranow           -> 41,
    KinoLuna              -> 40,
    KinoElektronik        -> 1881,
    KinoIluzjon           -> 35,
    KinoGram              -> 2400,
    KinoKultura           -> 38,
    KinoAmondo            -> 2077, // "Amondo Kino"
    KinoNaBoku            -> 1846,
    KinoGlebocka66        -> 2401,
    KinoSwit              -> 52,
    StacjaFalenica        -> 1443,
    KinoAtlantic          -> 29,
    Kinoteka              -> 55,
    Ujazdowski            -> 151,
    KinoCytadela          -> 3247,
    // ── Kraków ──
    CinemaCityBonarka     -> 1432,
    CinemaCityKazimierz   -> 564,
    CinemaCityZakopianka  -> 395,
    MultikinoKrakow       -> 281,
    KinoMikro             -> 24,
    MikroBronowice        -> 1785,
    KinoSfinks            -> 88,
    // ── Trójmiasto ──
    MultikinoGdansk       -> 218,
    HeliosMetropolia      -> 1942,
    HeliosForum           -> 2097,
    HeliosRiviera         -> 1775, // Gdynia "Helios"
    KinoSpektrum          -> 3051,
    KinoKameralne         -> 2122,
    KinoMuzeumGdansk      -> 2042, // "Kino Muzeum"
    KinoZak               -> 60,
    KinoPort              -> 1735,
  )

  private val ArabicToRoman: Map[String, String] = Map(
    "1" -> "I", "2" -> "II", "3" -> "III", "4" -> "IV", "5" -> "V",
    "6" -> "VI", "7" -> "VII", "8" -> "VIII", "9" -> "IX", "10" -> "X",
    "11" -> "XI", "12" -> "XII", "13" -> "XIII", "14" -> "XIV", "15" -> "XV",
    "16" -> "XVI", "17" -> "XVII", "18" -> "XVIII", "19" -> "XIX", "20" -> "XX"
  )

  /** Loose title key for cross-source film matching: trim, Arabic→Roman for
   *  sequel numerals (Filmweb writes "Avatar 3", we write "Avatar III" etc.),
   *  lowercase. Same idea as the old `app/scripts/FilmwebDiff`. */
  private def normalize(title: String): String =
    title.trim.split("\\s+").map(w => ArabicToRoman.getOrElse(w, w)).mkString(" ").toLowerCase

  sealed trait Verdict
  case object SAME             extends Verdict
  case object OURS_EXTRA       extends Verdict
  case object FW_EXTRA         extends Verdict
  case object BOTH_DIFFER      extends Verdict
  case object OUR_FETCH_FAILED extends Verdict
  case object FW_FETCH_FAILED  extends Verdict

  /** One cinema's comparison result. Times are multisets (Seq, not Set) so a
   *  duplicate slot on one side counts as a difference. */
  private case class CinemaDiff(
    cinema:    Cinema,
    oursCount: Int,
    fwCount:   Int,
    shared:    Int,
    oursOnly:  Int,
    fwOnly:    Int,
    verdict:   Verdict,
    detail:    String
  )

  def main(args: Array[String]): Unit = {
    val daysAhead = args.headOption.flatMap(a => Try(a.toInt).toOption).getOrElse(3)
    val today     = LocalDate.now(ZoneId.of("Europe/Warsaw"))
    val windowEnd = today.plusDays(daysAhead.toLong)

    // Remaining args: city slugs to restrict to (default: all cities).
    val knownSlugs = City.all.map(_.slug).toSet
    val cityFilter: Set[String] = args.drop(1).filter(knownSlugs).toSet
    val outPath = args.find(a => a.startsWith("/") || a.endsWith(".txt"))
      .getOrElse("/tmp/filmweb-diff-output.txt")

    val http    = new RealHttpFetch()
    val catalog = new CinemaScraperCatalog(http, today = today)

    // Our scrapers for the cinemas Filmweb lists, scoped to the requested
    // cities, in city order — one scraper per cinema (the catalog wires exactly
    // one), so a simple by-cinema lookup is enough.
    val ourScrapers: Seq[CinemaScraper] = catalog.all.filter { s =>
      filmwebCinemaIds.contains(s.cinema) &&
        (cityFilter.isEmpty || cityOf(s.cinema).exists(c => cityFilter(c.slug)))
    }

    val out = new StringBuilder
    def emit(line: String): Unit = { println(line); out.append(line).append('\n'); () }

    emit(s"FilmwebDiff — window [$today .. $windowEnd] ($daysAhead days ahead)")
    emit(s"Cinemas compared: ${ourScrapers.size}" +
      (if (cityFilter.nonEmpty) s"  (cities: ${cityFilter.toSeq.sorted.mkString(", ")})" else ""))
    emit("")

    var consecutiveFwFailures = 0
    val diffs: Seq[CinemaDiff] = ourScrapers.zipWithIndex.map { case (scraper, i) =>
      if (i > 0) Thread.sleep(if (consecutiveFwFailures >= 3) 8000L else 1500L)
      val cinema = scraper.cinema
      emit(s"… ${cinema.displayName}")

      val oursTry = Try(scraper.fetch())
      val fwTry   = Try(new FilmwebShowtimesClient(http, filmwebCinemaIds(cinema), cinema, daysAhead, today).fetch())

      fwTry match {
        case Failure(_) => consecutiveFwFailures += 1
        case Success(_) => consecutiveFwFailures = 0
      }

      diffFor(cinema, oursTry, fwTry, today, windowEnd)
    }

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

    Try {
      val pw = new PrintWriter(outPath)
      try pw.write(out.toString) finally pw.close()
    } match {
      case Success(_) => emit(s"\nFull output written to $outPath")
      case Failure(e) => emit(s"\nCould not write $outPath: ${e.getMessage}")
    }
  }

  private def cityOf(cinema: Cinema): Option[City] =
    City.all.find(_.cinemas.contains(cinema))

  /** Restrict a side's showtimes to the comparison window and key by film. */
  private def withinWindow(
    movies: Seq[CinemaMovie], today: LocalDate, windowEnd: LocalDate
  ): Map[String, Seq[LocalDateTime]] = {
    val startOfDay = today.atStartOfDay()
    val endOfDay   = windowEnd.plusDays(1).atStartOfDay() // exclusive: whole windowEnd day
    movies
      .map(m => normalize(m.movie.title) -> m.showtimes.map(_.dateTime)
        .filter(dt => !dt.isBefore(startOfDay) && dt.isBefore(endOfDay)))
      .filter(_._2.nonEmpty)
      .groupMapReduce(_._1)(_._2)(_ ++ _)
  }

  private def diffFor(
    cinema:    Cinema,
    oursTry:   Try[Seq[CinemaMovie]],
    fwTry:     Try[Seq[CinemaMovie]],
    today:     LocalDate,
    windowEnd: LocalDate
  ): CinemaDiff = (oursTry, fwTry) match {
    case (Failure(e), _) =>
      CinemaDiff(cinema, 0, 0, 0, 0, 0, OUR_FETCH_FAILED, s"  OUR fetch failed: ${msg(e)}")
    case (_, Failure(e)) =>
      CinemaDiff(cinema, 0, 0, 0, 0, 0, FW_FETCH_FAILED, s"  Filmweb fetch failed: ${msg(e)}")
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
        cinema, oursTimes.size, fwTimes.size, shared.size, oursOnly.size, fwOnly.size,
        verdict, perFilmDetail(oursByFilm, fwByFilm)
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

  // Multiset ops over LocalDateTime: count duplicates correctly.
  private def multisetIntersect(a: Seq[LocalDateTime], b: Seq[LocalDateTime]): Seq[LocalDateTime] = {
    val bCounts = scala.collection.mutable.HashMap[LocalDateTime, Int]()
    b.foreach(x => bCounts.update(x, bCounts.getOrElse(x, 0) + 1))
    a.flatMap { x =>
      val c = bCounts.getOrElse(x, 0)
      if (c > 0) { bCounts.update(x, c - 1); Some(x) } else None
    }
  }

  private def multisetDiff(a: Seq[LocalDateTime], b: Seq[LocalDateTime]): Seq[LocalDateTime] = {
    val bCounts = scala.collection.mutable.HashMap[LocalDateTime, Int]()
    b.foreach(x => bCounts.update(x, bCounts.getOrElse(x, 0) + 1))
    a.flatMap { x =>
      val c = bCounts.getOrElse(x, 0)
      if (c > 0) { bCounts.update(x, c - 1); None } else Some(x)
    }
  }

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
  }

  private def msg(e: Throwable): String =
    Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
}
