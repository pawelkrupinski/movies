package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Koszalin: see [[PolishVenueRegistry]]. */
object KoszalinVenues extends PolishVenueRegistry {
  /** Kino Kryterium's MSI portal: it times out the worker's IP and every Decodo proxy IP, so its pages AND its posters
   *  go through the Zyte route (`EgressWiring.zyteFetch`). Above the venues, which read it as the object initialises. */
  val KinoKryteriumUrl = "https://bilety.ck105.koszalin.pl"

  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Koszalin),
    multikino("0015", MultikinoKoszalin),
    msi(KinoKryterium, KinoKryteriumUrl, route = PolishVenue.Route.Zyte),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    msi(KinoBajkaDarlowo, "https://darlowo.vectorsoft.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-spotkan-europejskich-w-bialogardzie-1685", KinoCentrumBialogard),
    biletyna(KinoDK, "https://biletyna.pl/Slawno/Slawienski-Dom-Kultury"),
    msi(KinoGOK, "https://bilety.goktychowo.pl"),
    msi(KinoGoplana, "https://bilety.ckpolczyn.pl"),
    msi(KinoWybrzeze, "http://bilety.rck.kolobrzeg.pl", titleSuffix = Some("KINO WYBRZEŻE")),
  )
}
