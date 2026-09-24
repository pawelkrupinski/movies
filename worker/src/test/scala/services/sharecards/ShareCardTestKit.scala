package services.sharecards

import models.{CityScreening, Country, ResolvedMovie, ResolvedRatings, Showtime}
import services.readmodel.InMemoryReadModelRepository
import services.tasks.InMemoryTaskQueue

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO

/** What the share-card specs share: a temp directory store, a counting poster download that
 *  serves real JPEG bytes, a film, and a service wired over in-memory fakes. */
object ShareCardTestKit {
  val T0: Instant = Instant.parse("2026-06-01T10:00:00Z")
  def clockAt(at: Instant): Clock = Clock.fixed(at, ZoneOffset.UTC)

  def tempStore(): ShareCardStore = {
    val store = new ShareCardStore(Files.createTempDirectory("share-cards-"))
    assert(store.usable)
    store
  }

  /** A real, decodable poster: a 600×900 JPEG. */
  lazy val posterJpeg: Array[Byte] = {
    val image = new BufferedImage(600, 900, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()
    g.setColor(new Color(0x33, 0x66, 0x99)); g.fillRect(0, 0, 600, 900)
    g.setColor(Color.ORANGE); g.fillOval(100, 200, 400, 400); g.dispose()
    val out = new ByteArrayOutputStream()
    ImageIO.write(image, "jpg", out)
    out.toByteArray
  }

  /** Serves [[posterJpeg]] for every URL outside `failing`, counting each fetch per URL. */
  final class CountingDownload(failing: Set[String] = Set.empty) extends PosterDownload {
    val calls = new java.util.concurrent.ConcurrentHashMap[String, AtomicInteger]()
    def total: Int = { import scala.jdk.CollectionConverters.*; calls.values.asScala.map(_.get).sum }
    def fetch(url: String): Option[Path] = {
      calls.computeIfAbsent(url, _ => new AtomicInteger()).incrementAndGet()
      Option.when(!failing(url)) {
        val file = Files.createTempFile("poster-", ".img")
        Files.write(file, posterJpeg)
      }
    }
  }

  /** The bounded JDK decode only — no vips, so a spec runs the same on every machine. */
  val javaShrinker: PosterShrinker = new VipsPosterShrinker(binary = None)

  val ratings: ResolvedRatings = ResolvedRatings(Some(7.8), None, Some(81), "", Some(91), "", None, "")

  def film(id: String = "f0123456789abcd", poster: String = "https://cdn.example/poster-a.jpg",
           ratings: ResolvedRatings = ratings, title: String = "Diuna"): ResolvedMovie =
    ResolvedMovie(_id = id, title = title, originalTitle = None, posterUrl = Some(poster), fallbackPosterUrls = Nil,
      runtimeMinutes = Some(155), releaseYear = Some(2021), genres = Seq("Sci-Fi"), countries = Nil,
      directors = Seq("Denis Villeneuve"), cast = Nil, synopsis = Some("Paul Atryda."), trailerUrls = Nil,
      ratings = ratings, weightedRating = 8.0)

  def screening(filmId: String, city: String = "poznan"): CityScreening =
    CityScreening(_id = s"$filmId|$city|Kino Muza", filmId = filmId, city = city, cinema = "Kino Muza", filmUrl = None,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 2, 18, 0), None)))

  /** Every waiting task, oldest `submittedAt` first — claimed (and completed) at a far-future
   *  instant so a `notBefore` never hides one. */
  def drain(queue: InMemoryTaskQueue): Seq[services.tasks.Task] = {
    import scala.concurrent.duration.*
    Iterator.continually(queue.claim("spec", 1.minute, T0.plusSeconds(365L * 86400)))
      .takeWhile(_.isDefined).flatten.map { task => queue.complete(task.id, "spec"); task }.toSeq
  }

  class Rig(val store: ShareCardStore = tempStore(), val clock: Clock = clockAt(T0),
                  val download: CountingDownload = new CountingDownload()) {
    val readModel = new InMemoryReadModelRepository
    val queue     = new InMemoryTaskQueue
    val metrics: ShareCardMetrics = ShareCardMetrics.noop
    lazy val posters = new ShareCardPosters(store, download, javaShrinker, metrics)
    lazy val service = new ShareCardService(Country.default, store, posters, queue, metrics, clock)
  }
}
