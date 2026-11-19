package services.sharecards

import models.{City, Country}
import play.api.Logging
import services.readmodel.ReadModelReader
import services.tasks.{DueWindow, Enqueued, TaskProducer}
import services.tasktypes.TaskType
import tools.{CityCardRenderer, Digest}

import java.nio.file.{Files, Path}
import java.time.Clock
import scala.concurrent.duration.*
import scala.util.Try

/**
 * The country's CITY share cards: one per city in the catalog, `city-<slug>.jpg` in the share-card
 * directory beside the film cards, served by Caddy at `/share-cards/<cc>/city-<slug>.jpg` and named
 * by every `/{city}/` page's `og:image` (the web's `ShareCardUrl.city`).
 *
 * WHAT A CARD DRAWS ([[CityCardRenderer]]): the posters of the films the city shows most widely —
 * ranked by how many of its cinemas show each, read off `web_screenings` — under the brand, the
 * city's localised heading and the rating pills. The posters come from the film cards' own poster
 * cache, so a city card fetches nothing: a film whose poster is not cached is skipped.
 *
 * WHEN, spread like the scrapes. Every city is re-rendered once a [[Period]] (a week), each at its
 * own phase hashed from its key ([[DueWindow]], the schedule the scrape and rating reapers share),
 * so the fleet's ~3,000 cities trickle through the week instead of arriving at once. A reaper tick
 * ([[enqueueDue]], every minute, claimed) enqueues the cities with NO card first — a new city, a
 * lost directory — then the ones whose phase came round, at most [[MaxEnqueuePerTick]] a tick; the
 * handler re-checks the same schedule at pickup. A card's "last rendered" is its file's mtime, so
 * the schedule needs no storage and survives restarts. A re-render that would draw the same picture
 * only moves the mtime (`unchanged`).
 *
 * A render that FAILS or is REFUSED moves no mtime, so the schedule alone would ask for that city
 * again at every tick, decoding its posters each time. [[CityCardBackoff]] holds such a city out of
 * the reaper's ticks for a wait that doubles while it keeps failing; a drawn card clears it. And a
 * city with no card is refused before anything is read or decoded when the directory has no room.
 *
 * Cards of cities gone from the catalog are pruned by [[ShareCardJanitor]].
 */
class CityCardService(
  country: Country,
  store:   ShareCardStore,
  reader:  ReadModelReader,
  queue:   TaskProducer,
  metrics: ShareCardMetrics,
  clock:   Clock,
  schedule: DueWindow = new DueWindow(CityCardService.Period),
  maxEnqueuePerTick: Int = CityCardService.MaxEnqueuePerTick
) extends Logging {
  import CityCardService.*
  import ShareCardMetrics.CityOutcome

  private val citiesBySlug: Map[String, City] = country.cities.iterator.map(city => city.slug -> city).toMap

  private val brand = CityCardRenderer.Brand(country.brandName, country.shareHost, country.filmwebEnabled)

  private val backoff = new CityCardBackoff(clock)

  private val cityFilms = new CityFilms(reader, citiesBySlug.keySet, clock)

  /** The reaper's tick: enqueue the cities with no card, then those due, up to the cap; publish
   *  the coverage gauge. Returns what its enqueues came to: how many renders it added, and how many failed. */
  def enqueueDue(): Enqueued = {
    val now     = clock.instant()
    val written = country.cities.map(city => city -> store.written(store.cityCardPath(city)))
    if (written.nonEmpty)
      metrics.cityCoverage(written.count { case (_, at) => at.exists(_.isAfter(now.minusMillis(StaleAfter.toMillis))) }.toDouble / written.size)
    val ready   = written.iterator.filterNot { case (city, _) => backoff.holds(city.slug, now) }.toVector
    val missing = ready.iterator.collect { case (city, None) => city }
    val due     = ready.iterator.collect { case (city, at @ Some(_)) if schedule.isDue(dedupKey(city), at, now) => city }
    (missing ++ due).take(maxEnqueuePerTick).foldLeft(Enqueued.Zero) { (sum, city) =>
      sum + Enqueued.of(queue.enqueue(TaskType.RenderCityCard, dedupKey(city), Map(CityKey -> city.slug), submittedAt = now))
    }
  }

  /** The handler's entry: render the city's card unless the schedule says it is not due — the
   *  same question [[enqueueDue]] asked, so the two never disagree. */
  def renderIfDue(slug: String): Option[String] =
    citiesBySlug.get(slug).flatMap { city =>
      val written = store.written(store.cityCardPath(city))
      Option.when(written.isEmpty || schedule.isDue(dedupKey(city), written, clock.instant()))(render(city))
    }

  /** Draw the city's card from its current films and write it — or, when it would be the picture
   *  already on disk, only mark it rendered now. A city with no card the directory has no room for
   *  is refused first, before its films are read or a poster decoded. */
  def render(city: City): String = {
    val path    = store.cityCardPath(city)
    var posters = Vector.empty[Path]
    val outcome = Try {
      store.admitNew(path)
      posters = cityFilms.of(city.slug).iterator.map(store.posterPath).filter(Files.isRegularFile(_)).take(CityCardRenderer.PosterCount).toVector
      val version = versionOf(city, posters)
      if (store.version(path).contains(version)) { store.touch(path, clock.instant()); CityOutcome.Unchanged }
      else {
        store.writeAtomically(path, CityCardDrawing.png(city.heading, brand, posters), version)
        // The schedule reads the injected clock, so the file is stamped by it, not the filesystem's.
        store.touch(path, clock.instant())
        CityOutcome.Rendered
      }
    }.recover {
      case _: NoRoomForShareCard => CityOutcome.Refused
      case e: Exception =>
        logger.warn(s"city card: ${city.slug} could not be drawn: ${e.getClass.getSimpleName}: ${e.getMessage}")
        CityOutcome.Failed
    }.get
    outcome match {
      case CityOutcome.Rendered | CityOutcome.Unchanged => backoff.drawn(city.slug)
      case _                                            => backoff.missed(city.slug)
    }
    logger.info(s"city card: ${city.slug} $outcome — ${posters.size} posters")
    metrics.cityRender(outcome)
    outcome
  }

  /** The card's version: everything it draws — the template, the heading, the brand and, of each
   *  poster, its film and the poster's own version. */
  private def versionOf(city: City, posters: Seq[Path]): String =
    Digest.sha256Hex((Seq(TemplateVersion.toString, city.heading, brand.toString) ++
      posters.map(poster => s"${poster.getFileName}:${store.version(poster).getOrElse("")}")).mkString("\u0000")).take(16)
}

object CityCardService {
  /** How often each city's card is re-rendered — once a week, spread like the scrapes':
   *  spread: the films a city shows turn over about weekly. */
  val Period: FiniteDuration = 7.days

  /** A card not rendered for this long counts as missing in the coverage gauge: one missed week. */
  val StaleAfter: FiniteDuration = 14.days

  /** The most renders one tick enqueues. Steady state is ~0.3 a minute (the US's ~2,000 cities over
   *  a week's 10,080 minutes); this bounds a cold start — a first deploy, a lost directory — at 20 a
   *  minute, the US's whole catalog in under two hours. Re-asking a city still waiting is a
   *  duplicate, so the queue never holds more than about this many. */
  val MaxEnqueuePerTick: Int = 20

  /** The first wait before the reaper asks again for a city whose render failed or was refused,
   *  doubled per further miss up to [[MaxFailureRetry]] — the renders are weekly, so a city held a
   *  few hours loses nothing a minute's retry would win. */
  val FirstFailureRetry: FiniteDuration = 10.minutes
  val MaxFailureRetry: FiniteDuration   = 6.hours

  /** The films' ranking is re-read at most this often. */
  val FilmsMaxAge: FiniteDuration = 1.hour

  /** How many of a city's films are kept as poster candidates: twice the posters a card draws, as
   *  some have no poster cached. */
  val Candidates: Int = CityCardRenderer.PosterCount * 2

  /** Bumped when the card's look changes, so every card re-renders at its next phase. */
  val TemplateVersion: Int = 1

  val CityKey = "city"

  def dedupKey(city: City): String = s"city-card|${city.slug}"
}
