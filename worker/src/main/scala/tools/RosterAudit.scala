package tools

import models.Country
import services.cinemas.CinemaScraperCatalog
import services.cinemas.roster.{RosterLocationAudit, RosterSourceReader, SourceReading}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.{Failure, Success}

/**
 * The ONLINE half of the cinema-roster audit (the offline half is
 * `CinemaRosterAuditSpec`): fetch every Polish venue's source page that names a
 * town — bilety24 organiser pages and Filmweb cinema pages — and report where
 * the source disagrees with the roster: a town other than the one we file the
 * venue under (Koło's Kino nad Wartą read from Konin's culture centre), one
 * street address published for two of our venues (Kino Etiuda wired twice), an
 * address the source now only redirects, or a venue the source dropped.
 *
 * Run weekly by `.github/workflows/roster-audit.yml`, or by hand:
 *   sbt "worker/runMain tools.RosterAudit [summary.md]"
 * Appends a Markdown report to the given file (the job summary in CI) and exits
 * 1 when anything needs fixing. A page that could not be fetched this time is
 * listed, not failed on.
 *
 * `def main`, not `extends App` — see `FilmwebDiff`.
 */
object RosterAudit {

  /** Filmweb soft-blocks past ~5 concurrent callers (external-api-rate-limits skill). */
  private val Workers = 5

  def main(args: Array[String]): Unit = {
    val http    = new RealHttpFetch()
    val catalog = new CinemaScraperCatalog(http)
    val venues  = RosterSourceReader.venuesOf(Country.Poland.cities, slug => catalog.byCity.getOrElse(slug, Nil))
    println(s"RosterAudit: ${venues.size} Polish venue source pages to read")

    val (results, stats) = AdaptiveParallel.map(venues, Workers)(RosterSourceReader.isThrottle)(
      RosterSourceReader.read(http.getPage))
    val readings = results.map {
      case (venue, Success(reading)) => venue -> reading
      case (venue, Failure(e))       => venue -> SourceReading.Unreachable(s"gave up: ${e.getMessage}")
    }
    val findings = RosterLocationAudit.findings(readings)
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
