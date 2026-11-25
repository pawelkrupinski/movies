package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Zachodniopomorskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object ZachodniopomorskieVenues {
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

  /** The Polish venues wired near Zlocieniec: see [[PolishVenueRegistry]]. */
  object ZlocieniecVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/zlocieniecki-osrodek-kultury-1710", KinoMewaZlocieniec),   // Złocieniec
      bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-drawsku-pomorskim-1680", KinoOKDrawsko),   // Drawsko Pomorskie
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoChDKChoszczno, "https://biletyna.pl/Choszczno/Choszczenski-Dom-Kultury"),   // Choszczno
      biletyna(KinoDOKDrawno, "https://biletyna.pl/Drawno/Drawienski-Osrodek-Kultury"),   // Drawno
      biletyna(KinoMGOKRecz, "https://biletyna.pl/Recz/Miejsko-Gminny-Osrodek-Kultury"),   // Recz
      msi(KinoWolnoscSzczecinek, "https://bilety.sapik.pl"),   // Szczecinek
    )
  }

  /** The Polish venues wired near Szczecin: see [[PolishVenueRegistry]]. */
  object SzczecinVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Szczecin),
      multikino("0007", MultikinoSzczecin),
      own(kit => new PionierClient(kit.http, KinoPionier)),
      helios(HeliosVenues.SzczecinOutletPark),
      own(kit => new KinoZamekClient(kit.http, KinoZamekSzczecin, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      systemBiletowy(KinoKawiarnia, "https://kgl.systembiletowy.pl"),
      biletyna(KinoPDK, "https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury"),
      own(kit => new SckStargardClient(kit.http, KinoSCK)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoMOKPolice, "https://biletyna.pl/Police/Miejski-Osrodek-Kultury"),   // Police
      own(kit => new KinoEvaClient(kit.http, KinoEva)),   // Międzyzdroje
      biletyna(KinoGryfinskiDomKultury, "https://biletyna.pl/Gryfino/Sala-widowiskowa-Gryfinskiego-Domu-Kultury"),   // Gryfino
    )
  }
}
