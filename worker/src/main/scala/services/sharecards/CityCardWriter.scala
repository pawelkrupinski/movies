package services.sharecards

import models.City
import play.api.Logging
import tools.{CityCardRenderer, Digest}

import java.nio.file.{Files, Path}
import java.time.Clock
import scala.util.Try

/** Draws a city's card from its current films ([[CityFilms]]) and writes it to the share-card directory — or, when it
 *  would be the picture already on disk, only marks it rendered now. When a city is due is [[CityCardService]]'s. */
final class CityCardWriter(store: ShareCardStore, films: CityFilms, brand: CityCardRenderer.Brand, metrics: ShareCardMetrics,
    clock: Clock) extends Logging {
  import ShareCardMetrics.CityOutcome

  /** Draw and write `city`'s card. A city with no card the directory has no room for is refused first, before its
   *  films are read or a poster decoded. */
  def render(city: City): String = {
    val path    = store.cityCardPath(city)
    var posters = Vector.empty[Path]
    val outcome = Try {
      store.admitNew(path)
      posters = films.of(city.slug).iterator.map(store.posterPath).filter(Files.isRegularFile(_)).take(CityCardRenderer.PosterCount).toVector
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
    logger.info(s"city card: ${city.slug} $outcome — ${posters.size} posters")
    metrics.cityRender(outcome)
    outcome
  }

  /** The card's version: everything it draws — the template, the heading, the brand and, of each
   *  poster, its film and the poster's own version. */
  private def versionOf(city: City, posters: Seq[Path]): String =
    Digest.sha256Hex((Seq(CityCardWriter.TemplateVersion.toString, city.heading, brand.toString) ++
      posters.map(poster => s"${poster.getFileName}:${store.version(poster).getOrElse("")}")).mkString("\u0000")).take(16)
}

object CityCardWriter {
  /** Bumped when the card's look changes, so every card re-renders at its next phase. */
  val TemplateVersion: Int = 1
}
