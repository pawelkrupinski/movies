package clients.tools

import models.SpanishRoster
import services.cinemas.es.OcineClient
import tools.RealHttpFetch

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDate
import scala.jdk.CollectionConverters._

/**
 * Record Ocine Girona's live ticketing-server responses as the fixtures
 * `OcineClientSpec` replays: the cartelera listing (one POST) and every film's
 * detail (one GET each) — the venue's whole scrape.
 *
 * Run with `sbt "worker/Test/runMain clients.tools.RecordOcine"`. The spec pins
 * `today` to the capture date, so re-recording means updating it there.
 *
 * Girona because it is the richest shape the chain serves: grouped films with
 * 3D / ATMOS / 4D / Infinity Vision / VOSE / Catalan variants, plus an ungrouped
 * live-event relay that carries its sessions on itself.
 *
 * The one edit made to the recorded bodies: every `poster` value — the film's
 * JPEG, base64-inlined, ~30KB a film and ~1MB across the venue — is blanked.
 * The client never reads it (there is no URL in it to store), so the fixture
 * keeps every byte the parser does read and loses only the image.
 */
object RecordOcine {
  def main(args: Array[String]): Unit = {
    val venue = SpanishRoster.theaterIdByCinema.collectFirst { case (c, "E0362") => c }
      .getOrElse(sys.error("no Spanish roster venue with SensaCine id E0362 (Ocine Girona)"))
    val client = new OcineClient(new RecordingHttpFetch("ocine", new RealHttpFetch()), "tickets.ocinegirona.es", venue,
      today = Some(LocalDate.now(OcineClient.Zone)))
    val films     = client.fetch()
    val showtimes = films.flatMap(_.showtimes)
    val days      = showtimes.map(_.dateTime.toLocalDate).distinct.sorted
    println(s"Ocine Girona: ${films.size} films, ${showtimes.size} showtimes, ${days.size} days " +
      s"${days.headOption.getOrElse("-")} .. ${days.lastOption.getOrElse("-")}")

    val root = Paths.get(FakeHttpFetch.rootFor("ocine"), "tickets.ocinegirona.es")
    Files.walk(root).iterator.asScala.filter(Files.isRegularFile(_)).foreach(blankPosters)
  }

  private val Poster = """"poster":"[^"]*"""".r

  private def blankPosters(file: Path): Unit = {
    val body = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    Files.write(file, Poster.replaceAllIn(body, "\"poster\":\"\"").getBytes(StandardCharsets.UTF_8))
  }
}
