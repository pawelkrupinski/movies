package clients.tools

import models.SpanishRoster
import play.api.libs.json._
import services.cinemas.es.{TaquillaClient, TaquillaVenue}
import tools.RealHttpFetch

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDate
import scala.jdk.CollectionConverters._

/**
 * Record mk2 El Ingenio's (Vélez-Málaga) taquilla.com responses as the fixtures
 * `TaquillaClientSpec` replays: the calendar and every day it sells — the venue's
 * whole scrape.
 *
 * Run with `sbt "worker/Test/runMain clients.tools.RecordTaquilla"`. The spec pins
 * `today` to the capture date, so re-recording means updating it there.
 *
 * El Ingenio because its calendar is the shape that matters: a full near week, then
 * single advance-sale days months out (to 2027-02 when captured), and VOSE sessions
 * beside dubbed ones.
 *
 * The one edit made to the recorded day bodies: each film's `entity_map` row is cut
 * to the `details.movie_info` the parser reads, and the offer/featured blocks are
 * dropped — ~100KB of SEO copy and promotions a day the client never reads.
 */
object RecordTaquilla {
  val Venue: TaquillaVenue = TaquillaVenue("33333", "/velez-malaga/mk2-cines-el-ingenio-velez-malaga")
  val TheaterId = "E0433"

  def main(args: Array[String]): Unit = {
    val cinema = SpanishRoster.theaterIdByCinema.collectFirst { case (c, TheaterId) => c }
      .getOrElse(sys.error(s"no Spanish roster venue with SensaCine id $TheaterId (mk2 El Ingenio)"))
    val client = new TaquillaClient(new RecordingHttpFetch("taquilla", new RealHttpFetch()), Venue, cinema,
      today = LocalDate.now(TaquillaClient.Zone))
    val films     = client.fetch()
    val showtimes = films.flatMap(_.showtimes)
    val days      = showtimes.map(_.dateTime.toLocalDate).distinct.sorted
    println(s"mk2 El Ingenio: ${films.size} films, ${showtimes.size} showtimes, ${days.size} days " +
      s"${days.headOption.getOrElse("-")} .. ${days.lastOption.getOrElse("-")}")

    val root = Paths.get(settings.FixtureRoot.RepositoryRelative.of("taquilla"), "www.taquilla.com", "json2", "search")
    Files.list(root).iterator.asScala.filter(_.getFileName.toString.startsWith("events")).foreach(trim)
  }

  private def trim(file: Path): Unit = {
    val body = Json.parse(Files.readAllBytes(file)).as[JsObject]
    val catalog = (body \ "entity_map").asOpt[JsObject].getOrElse(Json.obj()).fields.map { case (id, row) =>
      id -> Json.obj("details" -> Json.obj("movie_info" -> (row \ "details" \ "movie_info").getOrElse(JsNull)))
    }
    val trimmed = body - "featured" - "place_offers" - "place_offers_map" - "exemplar" ++ Json.obj("entity_map" -> JsObject(catalog))
    Files.write(file, Json.prettyPrint(trimmed).getBytes(StandardCharsets.UTF_8))
  }
}
