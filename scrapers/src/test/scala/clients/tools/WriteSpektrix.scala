package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas.uk.SpektrixClient

/** Measure every UK Spektrix venue live — its film and showtime count and last date, the
 *  measurement the catalogue's wiring was decided on — and, with `--record`, re-record
 *  the clients SpektrixClientSpec replays under test/resources/fixtures/spektrix/. Pass
 *  client names to measure only those. */
object WriteSpektrix {
  private val Venues: Seq[(String, Cinema, Option[String])] = Seq(
    ("ica", UkRoster("InstituteOfContemporaryArts"), None),
    ("arcstockton", UkRoster("ARCStocktonOnTees"), None),
    ("breweryarts", UkRoster("BreweryArtsCentreKendal"), None),
    ("brewhousetaunton", UkRoster("TauntonBrewhouse"), None),
    ("broadwaynottingham", UkRoster("BroadwayCinemaNottingham"), None),
    ("buxtonoperahouse", UkRoster("BuxtonCinemaPavilionArtsCentre"), Some("Pavilion Arts Centre")),
    ("cumbernauldtheatretrust", UkRoster("LanternhouseCinema"), None),
    ("dartington", UkRoster("BarnCinemaDartingtonArtCentre"), None),
    ("derbyquad", UkRoster("QuadDerby"), None),
    ("electricpalace", UkRoster("ElectricPalaceBridport"), None),
    ("falkirkcommunitytrust", UkRoster("HippodromeBoNess"), Some("Hippodrome")),
    ("galadurham", UkRoster("GalaCinemaDurham"), Some("Gala Theatre & Cinema")),
    ("galadurham", UkRoster("EmpireTheatreConsett"), Some("Consett Empire")),
    ("haverhillartscentre", UkRoster("HaverhillArtsCentre"), None),
    ("junctiongoole", UkRoster("JunctionGoole"), None),
    ("leedsheritagetheatres", UkRoster("HydeParkPictureHouse"), Some("Hyde Park Picture House")),
    ("lichfieldgarrick", UkRoster("LichfieldGarrickTheatreStudio"), Some("Studio")),
    ("lowtherpavilion", UkRoster("LowtherPavilionLytham"), None),
    ("macbirmingham", UkRoster("MidlandsArtsCentreBirmingham"), None),
    ("maltingsberwick", UkRoster("TheMaltingsBerwickUponTweed"), None),
    ("marinatheatre", UkRoster("MarinaTheatreLowestoft"), None),
    ("mowlemswanage", UkRoster("MowlemTheatre"), None),
    ("plymouthartscentre", UkRoster("PlymouthArtsCinema"), None),
    ("richmix", UkRoster("RichMixBethnalGreen"), None),
    ("rosestheatretewkesbury", UkRoster("RosesTheatreTewkesbury"), None),
    ("theatresevern", UkRoster("OldMarketHallShrewsbury"), Some("Old Market Hall")),
    ("theregentchristchurch", UkRoster("RegentChristchurch"), None),
    ("townereastbourne", UkRoster("TownerEastbourneCinema"), None),
    ("tricycle", UkRoster("KilnKilburn"), None),
    ("barbicancentre", UkRoster("BarbicanLondonCinema1"), None),
    ("jw3", UkRoster("Jw3Hampstead"), None),
    ("riversidestudios", UkRoster("RiversideStudiosHammersmith"), None),
    ("selladoor", UkRoster("KeyTheatrePeterborough"), Some("Key Theatre Peterborough")),
    ("lighthousepoole", UkRoster("LighthousePoole"), None),
    ("uckfieldpicturehouse", UkRoster("PictureHouseUckfield"), None),
    ("beamhertford", UkRoster("BEAMHertfordTheatre"), None),
    ("campuswest", UkRoster("TheCinemaCampusWest"), None),
    ("dukeslancaster", UkRoster("TheDukesLancaster"), None),
    ("dundeeca", UkRoster("DundeeContemporaryArtsDCA"), None),
    ("tynesidecinema", UkRoster("TynesideNewcastle"), None),
    ("warwickartscentre", UkRoster("WarwickArtsCentreCoventry"), None),
    ("showroomworkstation", UkRoster("ShowroomSheffield"), None),
    ("theploughartscentre", UkRoster("PloughArtsCentreTorrington"), None)
  )

  /** The clients SpektrixClientSpec replays; the rest are fetched to measure only. */
  private val Recorded = Set("ica", "galadurham", "broadwaynottingham", "theatresevern", "theploughartscentre", "dundeeca",
    "brewhousetaunton", "breweryarts", "leedsheritagetheatres", "lighthousepoole", "dukeslancaster")

  val commandLine: _root_.tools.CommandLine = VenueMeasure.commandLine("WriteSpektrix", "Measures every UK Spektrix venue live.")

  def runOf(arguments: _root_.tools.CommandLine.Arguments): VenueMeasure.Run = VenueMeasure.runOf(arguments, Venues.map(_._1))

  def main(args: Array[String]): Unit = {
    val run    = runOf(commandLine.parseOrExit(args))
    val live   = new RealHttpFetch()
    val today  = VenueClock.system.today(TimeZones.UnitedKingdom)
    Venues.filter { case (client, _, _) => run.wants(client) }.foreach { case (client, cinema, venue) =>
      val http = if (run.record && Recorded(client)) new RecordingHttpFetch("spektrix", live) else live
      scala.util.Try(new SpektrixClient(http, client, cinema, today, venue).fetch()).fold(
        e => println(s"MEASURE\t${cinema.displayName}\t$client\tFAILED ${e.getMessage}"),
        movies => {
          val times = movies.flatMap(_.showtimes)
          println(s"MEASURE\t${cinema.displayName}\t$client\t${movies.size}\t${times.size}\t${times.map(_.dateTime.toLocalDate).maxOption.getOrElse("")}")
        })
    }
  }
}
