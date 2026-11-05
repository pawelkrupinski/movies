package clients.tools

import models.GermanCinema
import services.cinemas.common.{CineamoClient, KinoheldClient, KinoheldVenue}
import tools.{PacedHttpFetch, RealHttpFetch}

import java.time.{Clock, LocalDate}
import scala.concurrent.duration._

/** Record venues' Cineamo AND kinoheld.de programmes on one day — every page each client reads — under
 *  test/resources/fixtures/second-feed-facts/ for SecondFeedFactsSpec: kinoheld's facts are lent to the showings Cineamo
 *  bills bare only when both feeds list the same screening, so both must be read the same day. Each venue is there for
 *  a shape the lending has to get right:
 *
 *   - Cinema Borken: "Bad Apples FK", bare on Cineamo; kinoheld states 2026, Jonatan Etzler, 100 minutes;
 *   - Babylon (Mitte), Berlin: venue-made kinoheld entries, undated ("Cicle Gaudí: LA FURIA", 107 minutes), whose
 *     facts are never lent;
 *   - Universum-City Karlsruhe: "Lauras Stern" (the 2021 film), which kinoheld links to the 2009 "Lauras Stern und der
 *     geheimnisvolle Drache Nian" — another film's facts, never lent;
 *   - Neues Arena, München: "IN THE MOOD FOR LOVE", Cineamo's record restating it, kinoheld's undated entry "In the
 *     Mood for Love (2000)".
 *
 *  Replay-first, like [[WriteKinoheld]]. Pass the recording day, which the spec pins. */
object WriteSecondFeedFacts {
  val Dir = "second-feed-facts"

  val Venues: Seq[(String, CineamoClient.CinemaId, KinoheldVenue)] = Seq(
    ("Cinema Borken",            CineamoClient.CinemaId("337"), KinoheldVenue("3873", "/kino/ahaus/cinema-in-ahaus")),
    ("Babylon (Mitte)",          CineamoClient.CinemaId("997"), KinoheldVenue("670", "/kino/berlin/babylon-berlin")),
    ("Universum-City Karlsruhe", CineamoClient.CinemaId("104"), KinoheldVenue("1261", "/kino/karlsruhe-baden/universum-city")),
    ("Neues Arena",              CineamoClient.CinemaId("718"), KinoheldVenue("29", "/kino/muenchen/arena-filmtheater")))

  def main(args: Array[String]): Unit = {
    val today = LocalDate.parse(args.headOption.getOrElse(sys.error("usage: WriteSecondFeedFacts <yyyy-MM-dd>")))
    val live  = PacedHttpFetch.perProcess(new RealHttpFetch(), _ => Some(500.millis), Clock.systemUTC())
    val fetch = new RecordMissingFetch(Dir, Set("cineamo.com", "kinoheld.de"), live)
    Venues.foreach { case (name, cineamo, kinoheld) =>
      val cinema = new GermanCinema(name, name)
      val primary = new CineamoClient(fetch, cineamo, cinema, today = today).fetch()
      val second  = new KinoheldClient(fetch, kinoheld, cinema, today = today).fetch()
      println(s"$name: Cineamo ${primary.size} films, kinoheld ${second.size} films")
    }
  }
}
