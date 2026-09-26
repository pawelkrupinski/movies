package tools

import models.Country
import services.cinemas.CinemaScraperCatalog
import services.cinemas.roster.{ChainDirectory, RosterLocationAudit, RosterSourceReader, SourceReading}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.{LocalDate, ZoneId}
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
 * 1 when anything needs fixing. A page that could not be fetched this time is
 * listed, not failed on — except a chain venue list still unread through the
 * residential proxy (see [[ChainListEgress]]).
 *
 * `def main`, not `extends App` — see `FilmwebDiff`.
 */
object RosterAudit {

  /** Filmweb soft-blocks past ~5 concurrent callers (external-api-rate-limits skill). */
  private val Workers = 5

  def main(args: Array[String]): Unit = {
    // Before the ~170 direct reads below: the chain lists that follow them tunnel through
    // the residential proxy, and the JDK reads this once — see ProxyTunnelAuthentication.
    ProxyTunnelAuthentication.BasicAllowed.applyToJvm()
    IssuerCertificateFetching.Enabled.applyToJvm()
    val process = ProcessConfiguration.resolve()
    val http    = new RealHttpFetch()
    val catalog = new CinemaScraperCatalog(http, env = process.env)
    val venues  = RosterSourceReader.venuesOf(Country.Poland.cities, slug => catalog.byCity.getOrElse(slug, Nil))
    println(s"RosterAudit: ${venues.size} Polish venue source pages to read")

    val (results, stats) = AdaptiveParallel.map(venues, Workers)(RosterSourceReader.isThrottle)(
      RosterSourceReader.read(http.getPage))
    val pageReadings = results.map {
      case (venue, Success(reading)) => venue -> reading
      case (venue, Failure(e))       => venue -> SourceReading.Unreachable(s"gave up: ${e.getMessage}")
    }

    val chainVenues = RosterSourceReader.chainVenuesOf(Country.Poland.cities, slug => catalog.byCity.getOrElse(slug, Nil))
    println(s"RosterAudit: ${chainVenues.size} Polish chain venues to look up in ${ChainDirectory.all.size} chain venue lists")
    val today = LocalDate.now(ZoneId.of("Europe/Warsaw"))
    val chainEgress = ChainListEgress.fromEnv(http, process.env)
    val chainResults = chainVenues.groupMap(_._1)(v => v._2 -> v._3).toSeq.map { case (directory, venues) =>
      RosterSourceReader.readDirectory(chainEgress.fetchFor(directory), today)(directory, venues).left.map(chainEgress.judged)
    }
    val readings = pageReadings ++ chainResults.flatMap(_.toSeq.flatten)
    val findings = RosterLocationAudit.findings(readings) ++ chainResults.flatMap(_.left.toSeq)
    val report   = RosterLocationAudit.markdown(readings, findings, s"Done: ${stats.summary}.")

    println(report)
    args.headOption.foreach { path =>
      Files.writeString(Paths.get(path), report + "\n", StandardCharsets.UTF_8,
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    }
    val failing = findings.count(_.failing)
    println(s"RosterAudit: $failing finding(s) to fix. ${stats.summary}")
    sys.exit(if (failing > 0) 1 else 0)
  }
}
