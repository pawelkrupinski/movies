package clients.tools

import models.SpanishRoster
import services.cinemas.es.{ArtesieteClient, ArtesieteParser}
import tools.RealHttpFetch

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/**
 * Record Artesiete La Torre's (Zaragoza) venue page as the fixture
 * `ArtesieteClientSpec` replays — the venue's whole scrape, one page.
 *
 * Run with `sbt "scrapers/Test/runMain clients.tools.RecordArtesiete"`. The spec pins
 * nothing to the capture date: the page carries no relative dates.
 *
 * La Torre because it shows the widest format vocabulary the chain sells: 4K Laser,
 * Atmos, 3D, Infinity Vision, "Láser VO" and "INGLES SUB ESPANOL".
 *
 * The recorded page is CUT to the one prop the parser reads, `:fullsessionsinfo`.
 * Every Artesiete page also embeds the chain's back-office rows (`:theaters`,
 * `:cinema`) with database, FTP and mail credentials in them — never to be checked
 * in — and ~500KB of markup the client never reads.
 */
object RecordArtesiete {
  val Page      = "https://latorre.artesiete.es/Cine/21/Artesiete-La-Torre"
  val TheaterId = "E1041"

  def main(args: Array[String]): Unit = {
    val cinema = SpanishRoster.theaterIdByCinema.collectFirst { case (c, TheaterId) => c }
      .getOrElse(sys.error(s"no Spanish roster venue with SensaCine id $TheaterId (Artesiete La Torre)"))
    new ArtesieteClient(new RecordingHttpFetch("artesiete", new RealHttpFetch()), Page, cinema).fetch()

    val root = Paths.get(settings.FixtureRoot.RepositoryRelative.of("artesiete"))
    Files.walk(root).iterator.asScala.filter(Files.isRegularFile(_)).foreach(cut)
    val films = new ArtesieteClient(new FakeHttpFetch("artesiete", strict = true), Page, cinema).fetch()
    val showtimes = films.flatMap(_.showtimes)
    val days = showtimes.map(_.dateTime.toLocalDate).distinct.sorted
    println(s"Artesiete La Torre: ${films.size} films, ${showtimes.size} showtimes, ${days.size} days " +
      s"${days.headOption.getOrElse("-")} .. ${days.lastOption.getOrElse("-")}")
  }

  /** The page reduced to its sessions prop, still HTML-escaped as served. */
  private def cut(file: Path): Unit = {
    val page = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    val prop = ArtesieteParser.SessionsProp.findFirstIn(page)
      .getOrElse(sys.error(s"$file carries no :fullsessionsinfo prop"))
    Files.write(file, s"<!-- Artesiete venue page, cut to its sessions prop (RecordArtesiete) -->\n<div $prop></div>\n"
      .getBytes(StandardCharsets.UTF_8))
  }
}
