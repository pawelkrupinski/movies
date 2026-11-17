package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Wielun: see [[PolishVenueRegistry]]. */
object WielunVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    filmweb(2034, KinoSyrena),   // Wieluń
    msi(KinoTeatrSieradz, "https://bilety.scksieradz.pl", filmLabel = Some("FILM")),   // Sieradz
    msi(KinoSlonce, "https://kinoslonce.pl"),   // Wieruszów
    own(kit => new IksorisRepertoireClient(kit.http, IksorisOrigin("https://kinosokolnia.org"), KinoSokolniaKepno, kit.today)),   // Kępno
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    msi(KinoWRatuszu, "https://bilety.mdk-zdunskawola.pl"),   // Zduńska Wola
  )
}
