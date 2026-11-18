package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Bydgoszcz: see [[PolishVenueRegistry]]. */
object BydgoszczVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1086", CinemaCityBydgoszcz),
    multikino("0006", MultikinoBydgoszcz),
    helios(HeliosVenues.Bydgoszcz),
    own(kit => new KinoOrzelClient(kit.http, KinoOrzel)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    msi(KinoKinomax, "https://bilety.kinomax.info.pl"),
    biletyna(KinoRondo, "https://biletyna.pl/Chelmno/Kinoteatr-Rondo"),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoCKSwiecie, "https://biletyna.pl/Swiecie/Sala-Widowiskowa-Osrodek-Kultury-Sportu-i-Rekreacji"),   // Świecie
    biletyna(KinoRelaks, "https://biletyna.pl/Naklo-nad-Notecia/Nakielski-Osrodek-Kultury"),   // Nakło nad Notecią
    // Kino Manhattan's TLS leaf cert doesn't validate from here (times out
    // over https, plain http serves the identical page) — same shape as the
    // three expired-cert venues MsiClient's baseUrl doc names.
    msi(KinoManhattanJanikowo, "http://bilety.mgok.janikowo.com.pl"),   // Janikowo
  )
}
