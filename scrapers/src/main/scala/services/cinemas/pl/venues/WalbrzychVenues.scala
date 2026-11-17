package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Walbrzych: see [[PolishVenueRegistry]]. */
object WalbrzychVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1091", CinemaCityWalbrzych),
    bilety24Subdomain(KinoApolloWalbrzych, "https://kino-apollo.bilety24.pl"),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    msi(KinoMOKNowaRuda, "https://bilety.nowaruda.pl"),
    ekobilet("mokis-bielawa", KinoMOKiS),
    own(kit => new KinoSlezaClient(kit.http, KinoSleza)),
    own(kit => new KinoZbyszekClient(kit.http, KinoZbyszek)),
    multikino("0041", MultikinoKlodzko),
    multikino("0043", MultikinoSwidnica),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoSCKStrzegom, "https://biletyna.pl/Strzegom/Kino-SCK"),   // Strzegom
  )
}
