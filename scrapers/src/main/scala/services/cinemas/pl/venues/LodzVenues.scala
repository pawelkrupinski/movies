package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Lodz: see [[PolishVenueRegistry]]. */
object LodzVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1080", CinemaCityManufaktura),
    multikino("0023", MultikinoLodz),
    helios(HeliosVenues.Lodz),
    // Charlie's homepage (the old CharlieClient) renders today only; its portal lists
    // every booked day. The portal also sells Kino Spójnia's screenings, which
    // KinoSpojniaClient scrapes from Spójnia's own site.
    msi(KinoCharlie, "https://bilety.charlie.pl", otherVenueLocations = Set("Kino Spójna")),
    own(kit => new KinematografLodzClient(kit.http, KinematografLodz, kit.today, titles = kit.titles)),
    own(kit => new NckfClient(kit.http, Nckf, kit.today)),
    own(kit => new KinoTatryClient(kit.http, KinoTatry, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    own(kit => new KinoSpojniaClient(kit.http, KinoSpojnia)),
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-stary-mlyn-w-zgierzu-1697", KinoStaryMlyn),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoMOKGlowno, "https://biletyna.pl/Glowno/Miejski-Osrodek-Kultury"),   // Głowno
    own(kit => new KinoTomiClient(kit.http, KinoTomi)),   // Pabianice
    helios(HeliosVenues.Pabianice),   // Pabianice
    msi(KinoGornikLeczyca, "https://kinogornik.eurobilet.pl"),   // Łęczyca
  )
}
