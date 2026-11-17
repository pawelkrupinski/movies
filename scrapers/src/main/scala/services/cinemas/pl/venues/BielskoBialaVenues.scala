package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Bielsko Biala: see [[PolishVenueRegistry]]. */
object BielskoBialaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.BielskoBiala),
    cinemaCity("1088", CinemaCityBielskoBiala),
    own(kit => new KinoKreskaClient(kit.http, KinoKreska)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/kino-janosik-1500", KinoJanosik),
    systemBiletowy(KinoPckulKino, "https://bilety.pckul.pl"),
    biletyna(KinoSwitCzechowiceDziedzice, "https://biletyna.pl/Czechowice-Dziedzice/Kino-Swit"),
    biletyna(KinoTeatrElektryczny, "https://biletyna.pl/Skoczow/Teatr-Elektryczny"),
    bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-brzeszczach-1539", KinoWislaBrzeszcze),
    multikino("0033", MultikinoCzechowiceDziedzice),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoPromyk, "https://biletyna.pl/Bystra/Gminny-Osrodek-Kultury-Promyk"),   // Bystra
    biletyna(KinoCentrumWadowice, "https://biletyna.pl/Wadowice/Wadowickie-Centrum-Kultury"),   // Wadowice
    cinemaCity("1098", CinemaCityCieszyn),   // Cieszyn
    // Sucha Beskidzka also sells its cabaret and musicals here
    systemBiletowy(KinoCKiF, "https://bilety.ckifsucha.pl", filmGroup = Some(EventCategory("Kino"))),   // Sucha Beskidzka
  )
}
