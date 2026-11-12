package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas.uk.SavoyClient

/** Measure the live Savoy Systems venues read from a booking-only box office's root page
 *  — film and showtime count and last date, the measurement their wiring was decided on —
 *  and, with `--record`, re-record the pages SavoyClientSpec replays under
 *  test/resources/fixtures/savoy/: the `WhatsOn` pages of Phoenix East Finchley and Savoy Corby
 *  and the box offices of Ilkley Cinema, the Phoenix Leicester, Wetherby Film Theatre and Theatr Gwaun. Pass
 *  venue names to run only those. */
object WriteSavoy {
  /** Full venue sites, the programme in the `WhatsOn` page's literal. */
  private val WhatsOnSites: Seq[(String, Cinema)] = Seq(
    "https://www.phoenixcinema.co.uk/PhoenixCinemaLondon.dll/" -> UkRoster("PhoenixCinemaEastFinchley"),
    "https://savoycorby.co.uk/SavoyCorby.dll/"                 -> UkRoster("SavoyCinemaCorby")
  )

  /** Booking-only hosts: the venue's own site is elsewhere, Savoy only sells the tickets. */
  val BoxOffices: Seq[(String, Cinema)] = Seq(
    "https://book.ilkleycinema.co.uk/IlkleyCinema.dll/"                                      -> UkRoster("IlkleyCinema"),
    "https://tickets.phoenix.org.uk/PhoenixCinemaLeicester.dll/"                              -> UkRoster("PhoenixCinemaAndArtCentreLeicester"),
    "https://book.wetherbycinema.com/WetherbyFilmTheatre.dll/"                                -> UkRoster("WetherbyFilmTheatre"),
    "http://arthousecrouchend.savoysystems.co.uk/ArthouseCrouchEnd.dll/"                      -> UkRoster("ArthouseCrouchEnd"),
    "https://cinelumiere.savoysystems.co.uk/CineLumiere.dll/"                                 -> UkRoster("CineLumiereLondon"),
    "https://bookings.thegardencinema.co.uk/TheGardenCinema.dll/"                             -> UkRoster("TheGardenCinema"),
    "https://palacecinemagorleston.savoysystems.co.uk/PalaceCinemaGorleston.dll/"             -> UkRoster("PalaceCinemaGorlestonOnSea"),
    "http://thepoly.savoysystems.co.uk/ThePoly.dll/"                                          -> UkRoster("ThePolyFalmouth"),
    "http://pavilionsteignmouth.savoysystems.co.uk/PavilionsTeignmouth.dll/"                  -> UkRoster("PavilionsTeignmouth"),
    "http://thebeehive.savoysystems.co.uk/TheBeehive.dll/"                                    -> UkRoster("TheBeehiveHoniton"),
    "http://shaftesburyartscentre.savoysystems.co.uk/ShaftesburyArtsCentre.dll/"              -> UkRoster("HilltopCinemaShaftesburyArtsCentre"),
    "https://newtonstewartcinema.savoysystems.co.uk/NewtonStewartCinema.dll/"                 -> UkRoster("TheCinemaNewtonStewart"),
    "https://theatrgwaun.savoysystems.co.uk/TheatrGwaun.dll/"                                 -> UkRoster("TheatrGwaunFishguard"),
    "https://bookings.chichestercinema.org/ChichesterCinema.dll/"                             -> UkRoster("ChichesterCinemaAtNewPark"),
    "https://southseacinema.savoysystems.co.uk/SouthseaCinema.dll/"                           -> UkRoster("SouthseaCinemaArtsCentre"),
    "http://therexberkhamsted.savoysystems.co.uk/TheRexBerkhamsted.dll/"                      -> UkRoster("TheRexCinemaBerkhamsted"),
    "https://silverscreenfolkestone.savoysystems.co.uk/SilverScreenFolkestone.dll/"           -> UkRoster("SilverScreenFolkestone"),
    "https://uppoxford.savoysystems.co.uk/UPPOxford.dll/"                                     -> UkRoster("UltimatePicturePalaceOxford"),
    "https://maonacinema.savoysystems.co.uk/MaonaCinema.dll/"                                 -> UkRoster("MaonaCinemaOswestry"),
    "https://cinebowlonline.org/CinebowlUttoxeter.dll/"                                       -> UkRoster("CinebowlUttoxeter"),
    "https://leistonfilmtheatre.savoysystems.co.uk/LeistonFilmTheatre.dll/"                   -> UkRoster("FilmTheatreLeiston"),
    "https://riversidewoodbridge.savoysystems.co.uk/RiversideWoodbridge.dll/"                 -> UkRoster("RiversideTheatreWoodbridge")
  )

  private val Recorded: Set[Cinema] = Set(UkRoster("PhoenixCinemaEastFinchley"), UkRoster("SavoyCinemaCorby"), UkRoster("IlkleyCinema"), UkRoster("PhoenixCinemaAndArtCentreLeicester"),
    UkRoster("WetherbyFilmTheatre"), UkRoster("TheatrGwaunFishguard"))

  val commandLine: _root_.tools.CommandLine = VenueMeasure.commandLine("WriteSavoy", "Measures the live Savoy Systems venues, by cinema name.")

  def runOf(arguments: _root_.tools.CommandLine.Arguments): VenueMeasure.Run =
    VenueMeasure.runOf(arguments, (WhatsOnSites ++ BoxOffices).map(_._2.displayName))

  def main(args: Array[String]): Unit = {
    val run    = runOf(commandLine.parseOrExit(args))
    val live   = new RealHttpFetch()
    val today  = VenueClock.system.today(TimeZones.UnitedKingdom)
    def wanted(cinema: Cinema) = run.wants(cinema.displayName)
    def http(cinema: Cinema) = if (run.record && Recorded(cinema)) new RecordingHttpFetch("savoy", live) else live

    WhatsOnSites.filter { case (_, cinema) => wanted(cinema) }.foreach { case (dllBase, cinema) =>
      measure(cinema, new SavoyClient(http(cinema), dllBase, cinema, today))
    }
    BoxOffices.filter { case (_, cinema) => wanted(cinema) }.foreach { case (dllBase, cinema) =>
      measure(cinema, new SavoyClient(http(cinema), dllBase, cinema, today, SavoyClient.BoxOffice))
    }
  }

  private def measure(cinema: Cinema, client: SavoyClient): Unit =
    scala.util.Try(client.fetch()).fold(
      e => println(s"MEASURE\t${cinema.displayName}\tFAILED ${e.getMessage}"),
      movies => {
        val times = movies.flatMap(_.showtimes)
        println(s"MEASURE\t${cinema.displayName}\t${movies.size}\t${times.size}\t${times.map(_.dateTime.toLocalDate).maxOption.getOrElse("")}")
      })
}
