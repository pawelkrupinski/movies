package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Krakow: see [[PolishVenueRegistry]]. */
object KrakowVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1090", CinemaCityBonarka),
    cinemaCity("1076", CinemaCityKazimierz),
    cinemaCity("1064", CinemaCityZakopianka),
    multikino("0005", MultikinoKrakow),
    // the two Mikro screens share one instance
    systemBiletowy(KinoMikro, "https://bilety.kinomikro.pl", institution = Some(Institution("Kino Mikro"))),
    systemBiletowy(MikroBronowice, "https://bilety.kinomikro.pl", institution = Some(Institution("Mikro Bronowice"))),
    own(kit => new KinoSfinksClient(kit.http, KinoSfinks)),
    own(kit => new KinoPodBaranamiClient(kit.http, KinoPodBaranami, kit.today)),
    own(kit => new KinoKijowClient(kit.http, KinoKijow, kit.today, titles = kit.titles)),
    own(kit => new KinoKikaClient(kit.http, KinoKika)),
    own(kit => new KinoAgrafkaClient(kit.http, KinoAgrafka)),
    own(kit => new KinoParadoxClient(kit.http, KinoParadox)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    systemBiletowy(KinoMuzaMyslenice, "https://bilety.kino.myslenice.pl"),   // Myślenice
    ekobilet("dk-wolbrom", KinoRadosc),   // Wolbrom
    systemBiletowy(KinoGryf, "https://bilety.ckis.miechow.eu"),   // Miechów
    bilety24("https://www.bilety24.pl/kino/organizator/kino-wazka-w-niepolomicach-pole-kultury-samorzadowa-instytucja-kultury-miasta-i-gminy-niepolomice-1273", KinoWazka),   // Niepołomice
    ekobilet("kino-wielicka-mediateka", KinoWielickaMediateka),   // Wieliczka
    bilety24("https://www.bilety24.pl/kino/organizator/kino-lubon-2-0-1549", KinoLubon),   // Mszana Dolna
  )
}
