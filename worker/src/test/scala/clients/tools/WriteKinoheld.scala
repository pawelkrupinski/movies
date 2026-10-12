package clients.tools

import models.GermanCinema
import services.cinemas.common.{KinoheldClient, KinoheldVenue}
import tools.{RateLimitedHttpFetch, RealHttpFetch}

import java.time.{Clock, LocalDate}
import scala.concurrent.duration._

/** Record a handful of venues' kinoheld.de programmes — every `programShows` page the
 *  client walks — under test/resources/fixtures/kinoheld/ for KinoheldClientSpec. Each
 *  venue is there for a shape the client has to get right:
 *
 *   - Capitol Kino Bochum: a city Filmstarts never listed, and a chain-ticketed venue
 *     (its `deeplink` books on kinotickets.express);
 *   - UCI Kinowelt Ruhr Park: a multiplex five pages deep, IMAX and 3D;
 *   - Casablanca Bochum: a travel talk kinoheld linked to the wrong film;
 *   - Das Lumen Solingen: sells through kinoheld itself (no `deeplink`), a sneak preview;
 *   - nestkino Hamburg: subtitled prints, German and English;
 *   - Arthaus-Kino Bonn: a catalogue film whose production year is a slip ("Hirten", 2040).
 *
 *  Replay-first, like [[WriteKinoprogramm]]: pages already on disk are served from it,
 *  so re-running for the pinned day only fills what a client change newly asks for.
 *  Pass the recording day, which the spec pins. */
object WriteKinoheld {
  val Venues: Seq[(String, KinoheldVenue)] = Seq(
    "Capitol Kino Bochum"            -> KinoheldVenue("88", "/kino/bochum/capitol"),
    "UCI Kinowelt Ruhr Park"         -> KinoheldVenue("1237", "/kino/bochum/uci-kinowelt-ruhr-park"),
    "Casablanca Filmtheater Bochum"  -> KinoheldVenue("149", "/kino/bochum/casablanca-filmtheater"),
    "Das Lumen Filmtheater Solingen" -> KinoheldVenue("224", "/kino/solingen/das-lumen-filmtheater-solingen"),
    "nestkino Hamburg"               -> KinoheldVenue("1853", "/kino/hamburg/schanzenkino-73"),
    "Arthaus-Kino Bonn"              -> KinoheldVenue("1461", "/kino/bonn/arthaus-kino-im-rheinischen-landesmuseum"))

  def main(args: Array[String]): Unit = {
    val today = LocalDate.parse(args.headOption.getOrElse(sys.error("usage: WriteKinoheld <yyyy-MM-dd>")))
    val live  = new RateLimitedHttpFetch(new RealHttpFetch(), _ => Some(500.millis), Clock.systemUTC())
    val fetch = new RecordMissingFetch("kinoheld", Set("kinoheld.de"), live)
    Venues.foreach { case (name, venue) =>
      val movies = new KinoheldClient(fetch, venue, new GermanCinema(name, name), today = today).fetch()
      println(s"$name: ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes, " +
        s"${movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).maxOption.getOrElse("-")} last day")
    }
  }
}
