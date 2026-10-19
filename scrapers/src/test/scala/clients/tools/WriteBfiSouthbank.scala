package clients.tools

import models._
import org.jsoup.Jsoup
import tools.RealHttpFetch
import services.cinemas.uk.BfiSouthbankClient

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** Measure BFI Southbank on cinemas.bfi.org.uk live — film and showtime count and last
 *  date — and, with `--record`, re-record every page it reads under
 *  test/resources/fixtures/bfi/ for BfiSouthbankClientSpec.
 *
 *  A scrape reads ~150 pages of ~170 KB each, nearly all of it markup, styles and scripts
 *  the client never reads, so a recorded page is trimmed to what it does: a list page to
 *  its `article.showCard`s, a film page to its `section#shows` — the day headings and the
 *  performance blocks under them, a sold-out one included (`--trim-only` redoes just that on
 *  the recorded tree). */
object WriteBfiSouthbank {
  private val FixtureTree = Paths.get("test/resources/fixtures/bfi")

  def main(args: Array[String]): Unit =
    if (args.contains("--trim-only")) trimAll() else measure(record = args.contains("--record"))

  private def measure(record: Boolean): Unit = {
    val live   = new RealHttpFetch()
    val http   = if (record) new RecordingHttpFetch("bfi", live) else live
    val movies = new BfiSouthbankClient(http, BfiLondonSouthbank, VenueClock.system.today(TimeZones.UnitedKingdom)).fetch()
    val times  = movies.flatMap(_.showtimes)
    movies.foreach(m => println(s"FILM\t${m.movie.title}\t${m.showtimes.size}\t${m.showtimes.flatMap(_.room).distinct.mkString(",")}"))
    println(s"MEASURE\t${BfiLondonSouthbank.displayName}\t${movies.size}\t${times.size}\t${times.map(_.dateTime.toLocalDate).maxOption.getOrElse("")}")
    if (record) trimAll()
  }

  private def trimAll(): Unit = Files.walk(FixtureTree).iterator.asScala.filter(Files.isRegularFile(_)).foreach(trim)

  private def trim(file: Path): Unit = {
    val page = Jsoup.parse(new String(Files.readAllBytes(file), UTF_8))
    page.select("script, style, svg").remove()
    val keep = if (file.getFileName.toString.startsWith("show-list-page")) "article.showCard" else "section#shows"
    val kept = page.select(keep).asScala.map(_.outerHtml).mkString("\n")
    Files.write(file, s"<html><body>\n$kept\n</body></html>\n".getBytes(UTF_8))
  }
}
