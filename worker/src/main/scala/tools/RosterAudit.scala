package tools

import models.Country
import services.cinemas.CinemaScraperCatalog
import services.cinemas.roster.{ChainDirectory, RosterFinding, RosterLocationAudit, RosterSourceReader, SourceReading}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.{Failure, Success}

/**
 * The ONLINE half of the cinema-roster audit (the offline half is
 * `CinemaRosterAuditSpec`): fetch every Polish venue's source page that names a
 * town — bilety24 organiser pages and Filmweb cinema pages — plus the Helios,
 * Cinema City and Multikino venue lists (one request per chain), and report where
 * the source disagrees with the roster: a town other than the one we file the
 * venue under (Koło's Kino nad Wartą read from Konin's culture centre), a venue
 * whose published coordinates are further from its city page than any venue
 * we file on purpose, one
 * street address published for two of our venues (Kino Etiuda wired twice), an
 * address the source now only redirects, or a venue the source dropped.
 *
 * Run weekly by `.github/workflows/roster-audit.yml`, or by hand:
 *   sbt "worker/runMain tools.RosterAudit [summary.md]"
 * Appends a Markdown report to the given file (the job summary in CI) and exits
 * with [[verdict]]'s code: 0 clean, [[DriftFound]] when a source disagrees with
 * the roster (data drift: the owner's task, never a red run --
 * scripts/ci/roster-audit.sh turns it into `overdue=true`, OverdueIsNotRedSpec),
 * 1 when the audit could not judge. A page that could not be fetched this time
 * is listed, not failed on — except a chain venue list still unread through the
 * residential proxy (see [[ChainListEgress]]), or too few pages read at all.
 *
 * `def main`, not `extends App` — see `FilmwebDiff`.
 */
object RosterAudit {

  /** The exit code for a run whose only failing findings are the sources disagreeing with the roster. */
  val DriftFound = 3

  /** The run's exit code. A town, an address or a venue the sources now publish differently is DATA DRIFT -- a fix
   *  to the roster for the owner, not a regression of the code -- so it is [[DriftFound]], which the workflow reports
   *  as overdue work. Red (1) is left for an audit that could not judge: a chain list unread through the residential
   *  proxy, or too few pages read to call a pass clean. (Run 2877, 2026-10-26, was red with a log cut off before the
   *  report: drift and a broken audit read the same then.) */
  def verdict(findings: Seq[RosterFinding]): Int = {
    val failing = findings.filter(_.failing)
    val unjudged = failing.exists {
      case _: RosterFinding.LowCoverage | _: RosterFinding.DirectoryNotRead => true
      case _                                                                => false
    }
    if (unjudged) 1 else if (failing.nonEmpty) DriftFound else 0
  }

  /** Filmweb soft-blocks past ~5 concurrent callers (external-api-rate-limits skill). */
  private val Workers = 5

  /** The one optional argument: the file the report is appended to (the workflow's step summary) — or why not: a
   *  second argument, or a flag, which the old `args.headOption` ignored after a whole audit's reads. */
  def reportPath(args: Seq[String]): Either[String, Option[java.nio.file.Path]] = args match {
    case Seq()                               => Right(None)
    case Seq(path) if !path.startsWith("-")  => Right(Some(Paths.get(path)))
    case Seq(flag)                           => Left(s"unknown option $flag (usage: RosterAudit [report.md])")
    case _ +: extra +: _                     => Left(s"unexpected argument $extra (usage: RosterAudit [report.md])")
  }

  def main(args: Array[String]): Unit = {
    val reportFile = reportPath(args.toSeq).fold(why => { System.err.println(s"RosterAudit: $why"); sys.exit(2) }, identity)
    val clock      = java.time.Clock.systemUTC()   // the tool's composition root: its one clock, handed down
    val venueClock = new models.VenueClock(clock)
    val stopwatch  = Stopwatch.System
    // Before the ~170 direct reads below: the chain lists that follow them tunnel through
    // the residential proxy, and the JDK reads this once — see ProxyTunnelAuthentication.
    ProxyTunnelAuthentication.BasicAllowed.applyToJvm()
    IssuerCertificateFetching.Enabled.applyToJvm()
    val process = settings.ProcessConfiguration.resolve()
    val http    = new RealHttpFetch()
    val catalog = new CinemaScraperCatalog(http, venueClock, clock)
    val venues  = RosterSourceReader.venuesOf(Country.Poland.cities, city => catalog.byCity.getOrElse(city, Nil))
    println(s"RosterAudit: ${venues.size} Polish venue source pages to read")

    val (results, stats) = AdaptiveParallel.map(venues, Workers)(RosterSourceReader.isThrottle)(
      RosterSourceReader.read(http.getPage))
    val pageReadings = results.map {
      case (venue, Success(reading)) => venue -> reading
      case (venue, Failure(e))       => venue -> SourceReading.Unreachable(s"gave up: ${e.getMessage}")
    }

    val today       = venueClock.todayInPoland
    val chainVenues = RosterSourceReader.chainVenuesOf(Country.Poland.cities, city => catalog.byCity.getOrElse(city, Nil), today)
    println(s"RosterAudit: ${chainVenues.size} Polish chain venues to look up in ${ChainDirectory.all.size} chain venue lists")
    val chainEgress = ChainListEgress.fromConfiguration(http, process, clock, stopwatch)
    val chainResults = chainVenues.groupMap(_._1)(v => v._2 -> v._3).toSeq.map { case (directory, venues) =>
      RosterSourceReader.readDirectory(chainEgress.fetchFor(directory), today)(directory, venues).left.map(chainEgress.judged)
    }
    val readings = pageReadings ++ chainResults.flatMap(_.toSeq.flatten)
    val findings = RosterLocationAudit.findings(readings) ++ chainResults.flatMap(_.left.toSeq)
    val report   = RosterLocationAudit.markdown(readings, findings, s"Done: ${stats.summary}.")

    println(report)
    reportFile.foreach { path =>
      Files.writeString(path, report + "\n", StandardCharsets.UTF_8,
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    }
    val failing = findings.count(_.failing)
    println(s"RosterAudit: $failing finding(s) to fix. ${stats.summary}")
    sys.exit(verdict(findings))
  }
}
