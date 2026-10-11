package services.sharecards

import models.{City, Country}
import play.api.Logging
import services.readmodel.ReadModelReader
import services.tasks.{DueWindow, EnqueueResult, HandlerOutcome, Task, TaskHandler, TaskQueue, TaskType}
import tools.{CityCardRenderer, Digest, OgCardRenderer, PosterDecode}

import java.nio.file.{Files, Path}
import java.time.{Clock, Instant}
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
 * Cards of cities gone from the catalog are pruned by [[ShareCardJanitor]].
 */
class CityCardService(
  country: Country,
  store:   ShareCardStore,
  reader:  ReadModelReader,
  queue:   TaskQueue,
  metrics: ShareCardMetrics,
  clock:   Clock,
  schedule: DueWindow = new DueWindow(CityCardService.Period),
  maxEnqueuePerTick: Int = CityCardService.MaxEnqueuePerTick
) extends Logging {
  import CityCardService.*
  import ShareCardMetrics.CityOutcome

  private val citiesBySlug: Map[String, City] = country.cities.iterator.map(city => city.slug -> city).toMap

  private val brand = CityCardRenderer.Brand(country.brandName, country.shareHost, country.filmwebEnabled)

  /** The reaper's tick: enqueue the cities with no card, then those due, up to the cap; publish
   *  the coverage gauge. Returns how many renders it added. */
  def enqueueDue(): Int = {
    val now     = clock.instant()
    val written = country.cities.map(city => city -> store.written(store.cityCardPath(city)))
    if (written.nonEmpty)
      metrics.cityCoverage(written.count { case (_, at) => at.exists(_.isAfter(now.minusMillis(StaleAfter.toMillis))) }.toDouble / written.size)
    val missing = written.iterator.collect { case (city, None) => city }
    val due     = written.iterator.collect { case (city, at @ Some(_)) if schedule.isDue(dedupKey(city), at, now) => city }
    (missing ++ due).take(maxEnqueuePerTick).count { city =>
      queue.enqueue(TaskType.RenderCityCard, dedupKey(city), Map(CityKey -> city.slug), submittedAt = now) == EnqueueResult.Added
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
   *  already on disk, only mark it rendered now. */
  def render(city: City): String = {
    val path    = store.cityCardPath(city)
    val posters = filmsOf(city).iterator.map(store.posterPath).filter(Files.isRegularFile(_)).take(CityCardRenderer.PosterCount).toVector
    val version = versionOf(city, posters)
    val outcome =
      if (store.version(path).contains(version)) { store.touch(path, clock.instant()); CityOutcome.Unchanged }
      else Try {
        val images = posters.flatMap(poster => Try(PosterDecode.fromFile(poster.toFile, CityCardRenderer.TileWidth, CityCardRenderer.TileHeight)).toOption.flatten)
        store.writeAtomically(path, OgCardRenderer.encodeCard(CityCardRenderer.render(city.heading, brand, images)), version)
        // The schedule reads the injected clock, so the file is stamped by it, not the filesystem's.
        store.touch(path, clock.instant())
        CityOutcome.Rendered
      }.recover {
        case _: NoRoomForShareCard => CityOutcome.Refused
        case e: Exception =>
          logger.warn(s"city card: ${city.slug} could not be drawn: ${e.getClass.getSimpleName}: ${e.getMessage}")
          CityOutcome.Failed
      }.get
    logger.info(s"city card: ${city.slug} $outcome — ${posters.size} posters")
    metrics.cityRender(outcome)
    outcome
  }

  /** The card's version: everything it draws — the template, the heading, the brand and, of each
   *  poster, its film and the poster's own version. */
  private def versionOf(city: City, posters: Seq[Path]): String =
    Digest.sha256Hex((Seq(TemplateVersion.toString, city.heading, brand.toString) ++
      posters.map(poster => s"${poster.getFileName}:${store.version(poster).getOrElse("")}")).mkString("\u0000")).take(16)

  // THE FILMS OF EVERY CITY, from one read of the screenings' ids, kept for an hour: a render needs
  // a city's films, and one `_id` scan an hour (the janitor's daily prune reads the same) is cheaper
  // than a query per city on a collection with no city index. Held: each city's first
  // [[Candidates]] film ids — strings the ids already are — so at most cities × 24 references.
  private final case class Films(byCity: Map[String, Vector[String]], readAt: Instant)
  @volatile private var films: Option[Films] = None

  /** The city's films, the most widely shown first. Throws when no read has ever succeeded. */
  private def filmsOf(city: City): Vector[String] = {
    val now = clock.instant()
    val snapshot = synchronized {
      if (!films.exists(_.readAt.isAfter(now.minusMillis(FilmsMaxAge.toMillis))))
        reader.findAllScreeningRefsChecked().answered match {
          case Some(refs) => films = Some(Films(rank(refs), now))
          case None       => logger.warn("city cards: web_screenings read failed — drawing from the last read, if any.")
        }
      films.getOrElse(throw new IllegalStateException("no read of web_screenings has succeeded yet"))
    }
    snapshot.byCity.getOrElse(city.slug, Vector.empty)
  }

  private def rank(refs: Seq[services.readmodel.ScreeningRef]): Map[String, Vector[String]] = {
    val venues = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.HashMap[String, Int]]
    refs.foreach(ref => ref.citySlug.filter(citiesBySlug.contains).foreach { slug =>
      val counts = venues.getOrElseUpdate(slug, scala.collection.mutable.HashMap.empty)
      counts.update(ref.filmId, counts.getOrElse(ref.filmId, 0) + 1)
    })
    venues.iterator.map { case (slug, counts) =>
      slug -> counts.toVector.sortBy { case (film, n) => (-n, film) }.iterator.take(Candidates).map(_._1).toVector
    }.toMap
  }
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

/** `RenderCityCard`: render the city's card when it is due. A failure is retried, then given up —
 *  the city keeps its old card, or the country's home card while it has none. */
class RenderCityCardHandler(service: CityCardService, maxAttempts: Int = 3) extends TaskHandler {
  val taskType: TaskType = TaskType.RenderCityCard
  def handle(task: Task): HandlerOutcome =
    task.payload.get(CityCardService.CityKey).flatMap(service.renderIfDue) match {
      case Some(ShareCardMetrics.CityOutcome.Failed) if task.attempts < maxAttempts => HandlerOutcome.Reschedule(Some("the city card could not be drawn"))
      case Some(_) => HandlerOutcome.Done
      case None    => HandlerOutcome.Skipped
    }
}
