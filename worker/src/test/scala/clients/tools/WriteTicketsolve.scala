package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas.uk.TicketsolveClient

/** Measure every UK Ticketsolve venue live — film and showtime count and last date, the
 *  measurement the catalogue's wiring was decided on — and, with `--record`, re-record
 *  the feeds TicketsolveClientSpec replays under test/resources/fixtures/ticketsolve/.
 *  Pass client names to run only those. */
object WriteTicketsolve {
  val Venues: Seq[(String, Cinema)] = Seq(
    "mwldan"               -> TheatrMwldanCardigan,
    "nervecentre"          -> NerveCentreLondonderry,
    "pocklingtonartscentre" -> PocklingtonArtsCentre,
    "aberystwythartscentre" -> AberystwythArtsCentre,
    "artscentre"            -> ArtsCentreStamford,
    "chapter"               -> ChapterCardiff,
    "gwynhall"              -> GwynHallNeath,
    "neuadddwyfor"          -> NeuaddDwyforPwllheli,
    "number8"               -> Number8Pershore,
    "pontardaweartscentre"  -> PontardaweArtsCentre,
    "theatrcolwyn"          -> TheatrColwyn,
    "torchtheatre"          -> TorchTheatreMilfordHaven,
    "watershed"             -> WatershedBristol,
    "wyeside"               -> WyesideArtsCentreBuilthWells
  )

  /** The feeds TicketsolveClientSpec replays; the rest are fetched to measure only. */
  private val Recorded = Set("mwldan", "nervecentre", "pocklingtonartscentre")

  def main(args: Array[String]): Unit = {
    val record = args.contains("--record")
    val asked  = args.filterNot(_ == "--record").toSet
    val live   = new RealHttpFetch()
    val today  = VenueClock.system.today(TimeZones.UnitedKingdom)
    Venues.filter { case (client, _) => asked.isEmpty || asked(client) }.foreach { case (client, cinema) =>
      val http = if (record && Recorded(client)) new RecordingHttpFetch("ticketsolve", live) else live
      scala.util.Try(new TicketsolveClient(http, client, cinema, today).fetch()).fold(
        e => println(s"MEASURE\t${cinema.displayName}\t$client\tFAILED ${e.getMessage}"),
        movies => {
          val times = movies.flatMap(_.showtimes)
          println(s"MEASURE\t${cinema.displayName}\t$client\t${movies.size}\t${times.size}\t${times.map(_.dateTime.toLocalDate).maxOption.getOrElse("")}")
        })
    }
  }
}
