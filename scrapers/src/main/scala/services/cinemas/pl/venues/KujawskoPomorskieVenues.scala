package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the KujawskoPomorskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object KujawskoPomorskieVenues {
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

  /** The Polish venues wired near Torun: see [[PolishVenueRegistry]]. */
  object TorunVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1077", CinemaCityTorunCzerwonaDroga),
      cinemaCity("1093", CinemaCityTorunPlaza),
      own(kit => new KinoCentrumCswClient(kit.http, KinoCentrumCsw)),
      // Kino 1410's own site (kino1410.pl) links kht.systembiletowy.pl for booking —
      // an existing platform, so it reuses SystemBiletowyClient rather than a new
      // client. Mostly "event cinema" broadcasts (André Rieu, Met Opera) alongside
      // the odd real film/classic-restoration screening; OnlyMovieEventsFilter
      // keeps the broadcasts (see NonMovieEventClassifier's broadcast veto) and
      // drops anything else event-shaped.
      systemBiletowy(Kino1410, "https://kht.systembiletowy.pl"),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      biletyna(KinoMiejskieCentrumKultury, "https://biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury"),
      biletyna(KinoZdroj, "https://biletyna.pl/Ciechocinek/Kino-Zdroj"),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Grudziadz),   // Grudziądz
    )
  }

  /** The Polish venues wired near Wloclawek: see [[PolishVenueRegistry]]. */
  object WloclawekVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0008", MultikinoWloclawek),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      msi(KinoJutrzenka, "https://kino.sierpc.pl"),
      biletyna(KinoNawojka, "https://biletyna.pl/Lipno/Kino-Nawojka"),
      msi(KinoNoweKinoWarszawa, "http://bilety.mck-gostynin.pl"),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/rypinski-dom-kultury-1053", KinoBaltykRypin),   // Rypin
    )
  }
}
