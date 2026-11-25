package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Slaskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object SlaskieVenues {
  /** The Polish venues wired near Katowice: see [[PolishVenueRegistry]]. */
  object KatowiceVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1065", CinemaCityPunkt44),
      cinemaCity("1079", CinemaCitySilesia),
      multikino("0035", MultikinoKatowice),
      helios(HeliosVenues.Katowice),
      // Silesia Film's art-house trio, all Bilety24-hosted: listing at `/repertuar/`
      // linking per-film `/wydarzenie/?id=N` pages, so they reuse the shared Bilety24Client.
      bilety24("https://www.bilety24.pl/kino/organizator/kino-kosmos-1501", KinoKosmos),
      bilety24("https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503", KinoSwiatowid),
      bilety24Subdomain(KinoteatrRialto, "https://kinoteatrrialto.bilety24.pl"),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      cinemaCity("1062", CinemaCity),
      own(kit => new KinoPatriaClient(kit.http, KinoPatria, kit.today)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      own(kit => new KinoGrajfkaClient(kit.http, KinoGrajfka)),   // Chorzów
      // Kino Frajda (Starochorzowski Dom Kultury) shares its bilety.chck.pl
      // systembiletowy portal with Chorzowskie Centrum Kultury's own events —
      // filmGroups keeps only the "Imprezy SDK" rows, same pattern as
      // KinoBCKBytom below but with this venue's own data-group value.
      systemBiletowy(KinoFrajda, "https://bilety.chck.pl", filmGroup = Some(EventCategory("Imprezy SDK"))),   // Chorzów
    )
  }

  /** The Polish venues wired near Bytom: see [[PolishVenueRegistry]]. */
  object BytomVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      // BCKino sells theatre/workshops/concerts through the same listing as its
      // films, tagged with a `data-group` per event ("BCKino" for films) —
      // `filmGroups` keeps only those and peels the resulting "BCKino – " title
      // prefix. Verified screening 2026-09-23.
      cinemaCity("1092", CinemaCityBytom),
      systemBiletowy(KinoBCKBytom, "https://bck.systembiletowy.pl", filmGroup = Some(EventCategory("BCKino"))),
    )
  }

  /** The Polish venues wired near Dabrowa Gornicza: see [[PolishVenueRegistry]]. */
  object DabrowaGorniczaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.DabrowaGornicza),
      own(kit => new VisualTicketClient(kit.http, "https://bilety.palac.art.pl", KinoKadr, locationId = 2)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoZbyszekOlkusz, "https://biletyna.pl/Olkusz/Kino-Zbyszek"),   // Olkusz
      multikino("0038", MultikinoJaworzno),   // Jaworzno
      biletyna(KinoSztuka, "https://biletyna.pl/Chrzanow/Miejski-Osrodek-Kultury-Sportu-i-Rekreacji"),   // Chrzanów
    )
  }

  /** The Polish venues wired near Rybnik: see [[PolishVenueRegistry]]. */
  object RybnikVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0014", MultikinoRybnik),
      cinemaCity("1082", CinemaCityRybnik),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Zory),
      bilety24("https://www.bilety24.pl/kino/organizator/kino-baltyk-1499", KinoBaltyk),
      ekobilet("kino-centrum-jastrzebiezdrj", KinoCentrum),
      systemBiletowy(KinoNaStarowce, "https://bilety.mok.zory.pl"),
      biletyna(KinoPegaz, "https://biletyna.pl/Wodzislaw-Slaski/Wodzislawskie-Centrum-Kultury"),
      own(kit => new TeatrZiemiRybnickiejClient(kit.http)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoFeniks, "https://biletyna.pl/Rydultowy/Rydultowskie-Centrum-Kultury-sala-teatralna"),   // Rydułtowy
    )
  }

  /** The Polish venues wired near Gliwice: see [[PolishVenueRegistry]]. */
  object GliwiceVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1085", CinemaCityGliwice),
      own(kit => new KinoAmokClient(kit.http, KinoAmok, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      own(kit => new KinoScenaKulturaClient(kit.http, KinoScenaKultura)),
    )
  }

  /** The Polish venues wired near Bielsko Biala: see [[PolishVenueRegistry]]. */
  object BielskoBialaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.BielskoBiala),
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

  /** The Polish venues wired near Sosnowiec: see [[PolishVenueRegistry]]. */
  object SosnowiecVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Sosnowiec),
      cinemaCity("1083", CinemaCitySosnowiec),
    )
  }

  /** The Polish venues wired near Tychy: see [[PolishVenueRegistry]]. */
  object TychyVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0053", MultikinoTychy),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      // Oświęcim's culture centre sells its concerts on the same instance
      systemBiletowy(KinoNaszeKino, "https://ock.systembiletowy.pl", institution = Some(Institution("Nasze Kino"))),
      msi(KinoPlanetCinema, "https://oswiecim.planetcinema.pl"),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      msi(KinoAleKinoLibiaz, "https://biletylck.libiaz.pl"),   // Libiąż
    )
  }

  /** The Polish venues wired near Czestochowa: see [[PolishVenueRegistry]]. */
  object CzestochowaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1089", CinemaCityCzestochowaJurajska),
      cinemaCity("1075", CinemaCityCzestochowaWolnosc),
      own(kit => new OkfIluzjaClient(kit.http, OkfIluzja)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      own(kit => new KinoDKFRumcajsClient(kit.http, KinoDKFRumcajs, today = kit.today)),
      own(kit => new KinoKarolinkaClient(kit.http, KinoKarolinka)),
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-radomsku-1546", KinoMDK),
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-centrum-im-adama-mickiewicza-w-zawierciu-1305", KinoMOKCentrum),
      own(kit => new KinoZaciszeClient(kit.http, KinoZacisze)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      msi(KinoPlanetCinemaZawiercie, "https://zawiercie.planetcinema.pl"),   // Zawiercie
      own(kit => new KinoMDKMyszkowClient(kit.http, KinoMDKMyszkow)),   // Myszków
    )
  }

  /** The Polish venues wired near Zabrze: see [[PolishVenueRegistry]]. */
  object ZabrzeVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0003", MultikinoZabrze),
      own(kit => new KinoRomaClient(kit.http, KinoRoma, kit.today)),
      msi(KinoPlanetCinemaZabrze, "https://zabrze.planetcinema.pl"),
    )
  }
}
