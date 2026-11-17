package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Przemysl: see [[PolishVenueRegistry]]. */
object PrzemyslVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Przemysl),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    own(kit => new KinoCentrum3DPrzemyslClient(kit.http, KinoCentrum3DPrzemysl)),
    msi(KinoIkar, "https://kinoikar.mok-jar.pl"),
    msi(KinoNaBiegunach, "https://jaroslaw.kinonabiegunach.pl"),
    own(kit => new KinoSDKSanokClient(kit.http, KinoSDK, kit.today)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new SystemBiletowyClient(kit.http, VisualSoftPortal("https://udk.systembiletowy.pl"), KinoOrzelUstrzyki, titles = kit.titles)),   // Ustrzyki Dolne
    systemBiletowy(KinoBieszczadzkiDK, "https://bdk.systembiletowy.pl"),   // Lesko
  )
}
