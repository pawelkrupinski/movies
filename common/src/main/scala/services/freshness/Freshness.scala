package services.freshness

import tools.Env

import scala.concurrent.duration._

/** The kinds of network-touching work whose recency we track so a scheduler can
 *  skip it when it was done recently enough. Each kind owns a short `label` that
 *  prefixes its [[FreshnessStore]] key (e.g. `imdb|<documentId>`), so the label must
 *  stay stable — it's the on-disk key shape, not just a display string. */
sealed trait FreshnessKind { def label: String }

object FreshnessKind {
  case object CinemaScrape  extends FreshnessKind { val label = "scrape"  }
  case object DetailEnrich  extends FreshnessKind { val label = "detail"  }
  case object TmdbResolve   extends FreshnessKind { val label = "tmdb"    }
  case object ImdbRating    extends FreshnessKind { val label = "imdb"    }
  case object FilmwebRating extends FreshnessKind { val label = "fw"      }
  case object RtRating      extends FreshnessKind { val label = "rt"      }
  case object McRating      extends FreshnessKind { val label = "mc"      }

  val all: Seq[FreshnessKind] =
    Seq(CinemaScrape, DetailEnrich, TmdbResolve, ImdbRating, FilmwebRating, RtRating, McRating)

  def byLabel(s: String): Option[FreshnessKind] = all.find(_.label == s)
}

/**
 * How long each kind of work stays "fresh" — i.e. how long after doing it we may
 * skip redoing it. A `None` TTL means **permanent**: once recorded it never goes
 * stale (TMDB resolution is effectively immutable once found; rows still missing
 * a resolution are retried by a separate missing-id reaper, not by re-resolving
 * resolved rows).
 *
 * Values match the cadence the worker ran before the queue existed: the four
 * rating sources were re-fetched on a 4h periodic walk, so their TTL is 4h —
 * the difference is that the walk is now *gated* (a row refreshed inside the
 * window is skipped instead of re-fetched unconditionally). Detail enrichment
 * (6h) and the cinema scrape window are new freshness windows the queue
 * introduces. The scrape TTL is the dominant lever on the worker's Mongo write
 * rate (each pass that finds a real change writes through to `movies` and
 * cascades to the read model), so it's tunable via `KINOWO_SCRAPE_FRESHNESS_MINUTES`
 * and defaults to 15min — short enough that showtimes stay current.
 *
 * The scrape window is applied PER-SCRAPER, not purely per-kind: the scrape
 * scheduler reads each scraper's [[services.cinemas.CinemaScraper.scrapeFreshness]]
 * (default [[defaultScrapeTtl]]) so a metered source can run on a longer cadence —
 * Multikino, served through the paid Zyte residential proxy, refreshes every 60min
 * while ordinary directly-scraped venues stay at 15min. [[ttlFor]]'s `CinemaScrape`
 * case is the kind-level default/fallback used when no per-scraper window applies.
 */
object Freshness {
  import FreshnessKind._

  /** The default cinema-scrape freshness window for an ordinary venue, tunable
   *  via `KINOWO_SCRAPE_FRESHNESS_MINUTES` (default 15min). The scrape scheduler
   *  applies this unless the scraper declares a longer per-source window. */
  def defaultScrapeTtl: FiniteDuration = Env.positiveLong("KINOWO_SCRAPE_FRESHNESS_MINUTES", 15L).minutes

  def ttlFor(kind: FreshnessKind): Option[FiniteDuration] = kind match {
    case CinemaScrape  => Some(defaultScrapeTtl)
    case DetailEnrich  => Some(6.hours)
    case ImdbRating    => Some(4.hours)
    case FilmwebRating => Some(4.hours)
    case RtRating      => Some(4.hours)
    case McRating      => Some(4.hours)
    case TmdbResolve   => None // permanent — see the missing-id reaper
  }
}
