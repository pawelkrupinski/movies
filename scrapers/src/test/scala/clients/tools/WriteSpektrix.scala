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
    ("ica", InstituteOfContemporaryArts, None),
    ("arcstockton", ARCStocktonOnTees, None),
    ("breweryarts", BreweryArtsCentreKendal, None),
    ("brewhousetaunton", TauntonBrewhouse, None),
    ("broadwaynottingham", BroadwayCinemaNottingham, None),
    ("buxtonoperahouse", BuxtonCinemaPavilionArtsCentre, Some("Pavilion Arts Centre")),
    ("cumbernauldtheatretrust", LanternhouseCinema, None),
    ("dartington", BarnCinemaDartingtonArtCentre, None),
    ("derbyquad", QuadDerby, None),
    ("electricpalace", ElectricPalaceBridport, None),
    ("falkirkcommunitytrust", HippodromeBoNess, Some("Hippodrome")),
    ("galadurham", GalaCinemaDurham, Some("Gala Theatre & Cinema")),
    ("galadurham", EmpireTheatreConsett, Some("Consett Empire")),
    ("haverhillartscentre", HaverhillArtsCentre, None),
    ("junctiongoole", JunctionGoole, None),
    ("leedsheritagetheatres", HydeParkPictureHouse, Some("Hyde Park Picture House")),
    ("lichfieldgarrick", LichfieldGarrickTheatreStudio, Some("Studio")),
    ("lowtherpavilion", LowtherPavilionLytham, None),
    ("macbirmingham", MidlandsArtsCentreBirmingham, None),
    ("maltingsberwick", TheMaltingsBerwickUponTweed, None),
    ("marinatheatre", MarinaTheatreLowestoft, None),
    ("mowlemswanage", MowlemTheatre, None),
    ("plymouthartscentre", PlymouthArtsCinema, None),
    ("richmix", RichMixBethnalGreen, None),
    ("rosestheatretewkesbury", RosesTheatreTewkesbury, None),
    ("theatresevern", OldMarketHallShrewsbury, Some("Old Market Hall")),
    ("theregentchristchurch", RegentChristchurch, None),
    ("townereastbourne", TownerEastbourneCinema, None),
    ("tricycle", KilnKilburn, None),
    ("barbicancentre", BarbicanLondonCinema1, None),
    ("jw3", Jw3Hampstead, None),
    ("riversidestudios", RiversideStudiosHammersmith, None),
    ("selladoor", KeyTheatrePeterborough, Some("Key Theatre Peterborough")),
    ("lighthousepoole", LighthousePoole, None),
    ("uckfieldpicturehouse", PictureHouseUckfield, None),
    ("beamhertford", BEAMHertfordTheatre, None),
    ("campuswest", TheCinemaCampusWest, None),
    ("dukeslancaster", TheDukesLancaster, None),
    ("dundeeca", DundeeContemporaryArtsDCA, None),
    ("tynesidecinema", TynesideNewcastle, None),
    ("warwickartscentre", WarwickArtsCentreCoventry, None),
    ("showroomworkstation", ShowroomSheffield, None),
    ("theploughartscentre", PloughArtsCentreTorrington, None)
  )

  /** The clients SpektrixClientSpec replays; the rest are fetched to measure only. */
  private val Recorded = Set("ica", "galadurham", "broadwaynottingham", "theatresevern", "theploughartscentre", "dundeeca",
    "brewhousetaunton", "leedsheritagetheatres", "lighthousepoole", "dukeslancaster")

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
