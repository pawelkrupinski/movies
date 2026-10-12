package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas.uk.SavoyClient

/** Measure the live Savoy Systems venues read from a booking-only box office's root page
 *  — film and showtime count and last date, the measurement their wiring was decided on —
 *  and, with `--record`, re-record the pages SavoyClientSpec replays under
 *  test/resources/fixtures/savoy/: Phoenix East Finchley's `WhatsOn` and the box offices
 *  of Ilkley Cinema, the Phoenix Leicester and Wetherby Film Theatre. Pass venue names to run only those. */
object WriteSavoy {
  private val PhoenixEastFinchley = "https://www.phoenixcinema.co.uk/PhoenixCinemaLondon.dll/"

  /** Booking-only hosts: the venue's own site is elsewhere, Savoy only sells the tickets. */
  val BoxOffices: Seq[(String, Cinema)] = Seq(
    "https://book.ilkleycinema.co.uk/IlkleyCinema.dll/"                                      -> IlkleyCinema,
    "https://tickets.phoenix.org.uk/PhoenixCinemaLeicester.dll/"                              -> PhoenixCinemaAndArtCentreLeicester,
    "https://book.wetherbycinema.com/WetherbyFilmTheatre.dll/"                                -> WetherbyFilmTheatre,
    "http://arthousecrouchend.savoysystems.co.uk/ArthouseCrouchEnd.dll/"                      -> ArthouseCrouchEnd,
    "https://cinelumiere.savoysystems.co.uk/CineLumiere.dll/"                                 -> CineLumiereLondon,
    "https://bookings.thegardencinema.co.uk/TheGardenCinema.dll/"                             -> TheGardenCinema,
    "https://palacecinemagorleston.savoysystems.co.uk/PalaceCinemaGorleston.dll/"             -> PalaceCinemaGorlestonOnSea,
    "http://thepoly.savoysystems.co.uk/ThePoly.dll/"                                          -> ThePolyFalmouth,
    "http://pavilionsteignmouth.savoysystems.co.uk/PavilionsTeignmouth.dll/"                  -> PavilionsTeignmouth,
    "http://thebeehive.savoysystems.co.uk/TheBeehive.dll/"                                    -> TheBeehiveHoniton,
    "http://shaftesburyartscentre.savoysystems.co.uk/ShaftesburyArtsCentre.dll/"              -> HilltopCinemaShaftesburyArtsCentre,
    "https://newtonstewartcinema.savoysystems.co.uk/NewtonStewartCinema.dll/"                 -> TheCinemaNewtonStewart,
    "https://theatrgwaun.savoysystems.co.uk/TheatrGwaun.dll/"                                 -> TheatrGwaunFishguard,
    "https://bookings.chichestercinema.org/ChichesterCinema.dll/"                             -> ChichesterCinemaAtNewPark,
    "https://southseacinema.savoysystems.co.uk/SouthseaCinema.dll/"                           -> SouthseaCinemaArtsCentre,
    "http://therexberkhamsted.savoysystems.co.uk/TheRexBerkhamsted.dll/"                      -> TheRexCinemaBerkhamsted,
    "https://silverscreenfolkestone.savoysystems.co.uk/SilverScreenFolkestone.dll/"           -> SilverScreenFolkestone,
    "https://uppoxford.savoysystems.co.uk/UPPOxford.dll/"                                     -> UltimatePicturePalaceOxford,
    "https://maonacinema.savoysystems.co.uk/MaonaCinema.dll/"                                 -> MaonaCinemaOswestry,
    "https://cinebowlonline.org/CinebowlUttoxeter.dll/"                                       -> CinebowlUttoxeter,
    "https://leistonfilmtheatre.savoysystems.co.uk/LeistonFilmTheatre.dll/"                   -> FilmTheatreLeiston,
    "https://riversidewoodbridge.savoysystems.co.uk/RiversideWoodbridge.dll/"                 -> RiversideTheatreWoodbridge
  )

  private val Recorded: Set[Cinema] = Set(PhoenixCinemaEastFinchley, IlkleyCinema, PhoenixCinemaAndArtCentreLeicester, WetherbyFilmTheatre)

  def main(args: Array[String]): Unit = {
    val record = args.contains("--record")
    val asked  = args.filterNot(_ == "--record").toSet
    val live   = new RealHttpFetch()
    val today  = VenueClock.system.today(TimeZones.UnitedKingdom)
    def wanted(cinema: Cinema) = asked.isEmpty || asked.contains(cinema.displayName)
    def http(cinema: Cinema) = if (record && Recorded(cinema)) new RecordingHttpFetch("savoy", live) else live

    if (wanted(PhoenixCinemaEastFinchley))
      measure(PhoenixCinemaEastFinchley,
        new SavoyClient(http(PhoenixCinemaEastFinchley), PhoenixEastFinchley, PhoenixCinemaEastFinchley, today))
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
