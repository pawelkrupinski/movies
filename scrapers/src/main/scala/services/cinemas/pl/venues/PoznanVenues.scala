package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Poznan: see [[PolishVenueRegistry]]. */
object PoznanVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino(MultikinoClient.PoznanStaryBrowarId, Multikino),
    own(kit => new CharlieMonroeClient(kit.http)),
    own(kit => new KinoPalacoweClient(kit.http, titles = kit.titles)),
    helios(HeliosVenues.Poznan),
    cinemaCity("1078", CinemaCityPoznanPlaza),
    cinemaCity("1081", CinemaCityKinepolis),
    own(kit => new KinoMuzaClient(kit.http, kit.today, titles = kit.titles)),
    own(kit => new KinoBulgarskaClient(kit.http, kit.today)),
    own(kit => new KinoApolloClient(kit.http, titles = kit.titles)),
    own(kit => new RialtoClient(kit.http)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoCKPrzezmierowo, "https://biletyna.pl/Przezmierowo/Dom-Kultury"),   // Przeźmierowo
    biletyna(KinoNOKNowyTomysl, "https://biletyna.pl/Nowy-Tomysl/Nowotomyski-Osrodek-Kultury"),   // Nowy Tomyśl
    bilety24("https://www.bilety24.pl/kino/organizator/kino-baszta-477", KinoBasztaSroda),   // Środa Wielkopolska
    bilety24("https://www.bilety24.pl/kino/organizator/kino-halszka-494", KinoHalszka),   // Szamotuły
    bilety24("https://www.bilety24.pl/kino/organizator/sremski-osrodek-kultury-kinoteatr-slonko-478", KinoteatrSlonko),   // Śrem
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-kino-miasta-i-gminy-buk-596", KinoWielkopolanin),   // Buk
    bilety24("https://www.bilety24.pl/kino/organizator/przedsiebiorstwo-wielobranzowe-trojka-s-c-475", KinoTrojka),   // Września
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-biblioteka-imienia-powstancow-wielkopolskich-w-opalenicy-967", KinoOpalenica),   // Opalenica
    bilety24("https://www.bilety24.pl/kino/organizator/kinoteatr-gwiazda-962", KinoteatrGwiazda),   // Wronki
  )
}
