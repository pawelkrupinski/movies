package tools

import models.{Country, MovieRecord}
import services.movies.ShowtimesDigest

import java.time.{Instant, LocalDateTime, ZoneId}

/**
 * A replayed corpus's enrichment coverage, counted the way [[ProdCoverage]] counts
 * production's — so the convergence band compares two measurements of ONE quantity
 * rather than two different ones.
 *
 * The prod side already draws the distinction that matters: it counts only the films
 * SCREENING at the capture instant, "because prod keeps a film's row after its last
 * showtime passes, so the collection is a running total while the corpus is a single
 * day". The replay has the mirror-image tail and had no such restriction. Every venue
 * keeps its last CONTENT-BEARING scrape in `cinema_scrapes` for as long as it stays
 * white or red — that rule is what stops an outage erasing a cinema — so a venue that
 * stopped answering in July is still in the corpus in August, with its July showtimes,
 * and the replay dutifully builds films out of them. Production, asked the same
 * question, does not count those films: they hold no future showtime.
 *
 * So the two sides were counting a running total against a single day from opposite
 * ends, and the difference was never enrichment. On Poland's 2026-08-22 corpus it was
 * 7 venues whose newest showtime had already passed (Kino Zachęta last answered on
 * 07-31, DKF Rumcajs's newest screening was 06-22) contributing ~60 raw titles that
 * production had long since stopped screening — enough to put the `films` axis 40
 * films and 5.9% from prod on a band of 5%, with every enrichment axis it feeds
 * sitting inside 3%. That shape — the denominator out, everything measured against it
 * in — is what a units mismatch looks like, not a regression.
 */
object CorpusCoverage {

  /** The clock the corpus's showtimes are written on. They are `LocalDateTime` with no
   *  zone — a cinema lists 19:00 local — so the capture instant has to be read in the
   *  country's own zone before the two can be compared. Every city of a country shares
   *  one zone today; the first is representative. */
  def zoneOf(country: Country): ZoneId =
    country.cities.headOption.map(_.zoneId).getOrElse(ZoneId.systemDefault())

  /** The capture instant as the corpus's own wall clock reads it. */
  def localise(at: Instant, country: Country): LocalDateTime =
    LocalDateTime.ofInstant(at, zoneOf(country))

  /**
   * Was any cinema still showing this film at `at`?
   *
   * Judged with [[models.Showtime.isUpcoming]] — the same grace-windowed rule the web
   * filters list views by and the worker counts source films by — so a film is never
   * dropped here for a reason that would keep it on the page.
   *
   * A slot whose showtimes were STRIPPED for cache residency counts as screening when
   * it stamped a non-zero count. Its dates are gone (`ShowtimesDigest.stripForCache`
   * keeps only a digest and a count), so the only honest answers are "unknown" and
   * "empty", and convicting on unknown is how a rule that reads a stripped slot ends
   * up dead in production while every spec on the embedded shape passes. This harness
   * stores showtimes embedded, so the branch is a guard rather than today's path.
   */
  def isScreening(record: MovieRecord, at: LocalDateTime): Boolean =
    record.cinemaSlots.exists { case (_, slot) =>
      if (slot.showtimes.nonEmpty) slot.showtimes.exists(_.isUpcoming(at))
      else ShowtimesDigest.slotShowtimeCount(slot) > 0
    }

  /** The films of `records` a cinema was still showing at `at`. */
  def screening(records: Seq[MovieRecord], at: LocalDateTime): Seq[MovieRecord] =
    records.filter(isScreening(_, at))

  /** These records' coverage, in the shape production's is recorded in, so the two can
   *  be compared field by field. `recordedAt` is [[Instant.EPOCH]]: this side is never
   *  the reference, and a timestamp on it would only invite reading one. */
  def of(records: Seq[MovieRecord]): ProdCoverageBaseline = {
    def count(predicate: MovieRecord => Boolean): Int = records.count(predicate)
    ProdCoverageBaseline(
      recordedAt     = Instant.EPOCH,
      films          = records.size,
      tmdbId         = count(_.tmdbId.isDefined),
      imdbId         = count(_.imdbId.isDefined),
      imdbRating     = count(_.imdbRating.isDefined),
      filmwebRating  = count(_.filmwebRating.isDefined),
      metascore      = count(_.metascore.isDefined),
      rottenTomatoes = count(_.rottenTomatoes.isDefined))
  }
}
