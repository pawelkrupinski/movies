package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the WarminskoMazurskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object WarminskoMazurskieVenues {
  /** The Polish venues wired near Olsztyn: see [[PolishVenueRegistry]]. */
  object OlsztynVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      // Chain ids verified against each chain's own cinema-list API. Each city's
      // independent screen, where one has a machine-readable source, is added via a
      // shared platform client: FilmwebShowtimesClient (Filmweb's seances JSON, by
      // internal cinema id — verified to return non-empty seances), Bilety24Client
      // (a bilety24.pl venue), or NoveKinoClient.
      helios(HeliosVenues.Olsztyn),
      multikino("0036", MultikinoOlsztyn),
      own(kit => new KinoAwangarda2Client(kit.http, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      msi(KinoCinemaLumiere, "https://bilety.kinoszczytno.pl"),
      msi(KinoIgnacy, "https://www.biletyignacy.pl"),
      bilety24("https://www.bilety24.pl/kino/organizator/moraski-dom-kultury-1682", KinoNarie),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      own(kit => new KinoGrunwaldClient(kit.http, KinoGrunwald, kit.today)),   // Olsztynek
    )
  }

  /** The Polish venues wired near Elblag: see [[PolishVenueRegistry]]. */
  object ElblagVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0037", MultikinoElblag),
      cinemaCity("1099", CinemaCityElblag),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Tczew),
      filmweb(2352, KinoBaszta),
      msi(KinoPowisle, "https://kinosztumbilety.pl"),
      biletyna(KinoZulawskiOsrodekKultury, "https://biletyna.pl/Nowy-Dwor-Gdanski/Zulawski-Osrodek-Kultury"),
      own(kit => new KinoSwiatowidElblagClient(kit.http, KinoSwiatowidElblag, kit.today)),
    )
  }

  /** The Polish venues wired near Ketrzyn: see [[PolishVenueRegistry]]. */
  object KetrzynVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      own(kit => new KinoGwiazdaKetrzynClient(kit.http, KinoGwiazdaKetrzyn)),   // Kętrzyn
      ekobilet("mck-mragowo", KinoZodiak),   // Mrągowo
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      own(kit => new KinoNowaFalaClient(kit.http, KinoNowaFalaGizycko)),   // Giżycko
    )
  }

  /** The Polish venues wired near Ilawa: see [[PolishVenueRegistry]]. */
  object IlawaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      msi(KinoteatrPasja, "https://biletyick.miastoilawa.pl"),   // Iława
      ekobilet("mok-lubawa", KinoPokoj),   // Lubawa
      bilety24("https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-1291", KinoteatrHarmonia),   // Nowe Miasto Lubawskie
    )
  }
}
