package clients.tools

import models._
import services.cinemas.de.ArsenalBerlinClient
import tools.RealHttpFetch

/** Record Arsenal Berlin's calendar — every page of its search API — and the screening pages of the
 *  films ArsenalBerlinClientSpec reads, under test/resources/fixtures/arsenal-berlin/. Pass the titles
 *  whose page to record (default: the Japanese retrospective's four the aggregators list bare, and a
 *  film with a cast). Prints what the client read, film by film. */
object WriteArsenalBerlin {
  val DefaultTitles: Seq[String] = Seq("Kaidan", "Mozu", "Ototo", "Ogin-sama", "Die bitteren Tränen der Petra von Kant")

  def main(args: Array[String]): Unit = {
    val titles = if (args.isEmpty) DefaultTitles else args.toSeq
    val http   = new RecordingHttpFetch("arsenal-berlin", new RealHttpFetch())
    val arsenal: Cinema = GermanRoster.theaterIdByCinema.collectFirst { case (c, "A0050") => c }.get
    val client = new ArsenalBerlinClient(http, arsenal, VenueClock.system.today(TimeZones.Germany))
    val movies = client.fetch()
    movies.foreach { m =>
      println(s"${m.movie.title}\t${m.movie.releaseYear.getOrElse("-")}\t${m.director.mkString(", ")}\t" +
        s"${m.movie.countries.mkString(", ")}\t${m.movie.runtimeMinutes.getOrElse("-")}\t${m.showtimes.size}\t${m.filmUrl.getOrElse("-")}")
    }
    movies.filter(m => titles.contains(m.movie.title)).flatMap(_.filmUrl).foreach { url =>
      println(s"PAGE\t$url\t${client.fetchFilmDetail(url)}")
    }
  }
}
