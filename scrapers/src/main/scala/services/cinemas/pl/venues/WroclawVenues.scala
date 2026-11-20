package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Wroclaw: see [[PolishVenueRegistry]]. */
object WroclawVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1097", CinemaCityWroclavia),
    cinemaCity("1067", CinemaCityKorona),
    multikino("0010", MultikinoPasazGrunwaldzki),
    helios(HeliosVenues.Magnolia),
    helios(HeliosVenues.AlejaBielany),
    own(kit => new NoweHoryzontyClient(kit.http, kit.today)),
    own(kit => new DcfClient(kit.http)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    own(kit => new Bilety24SubdomainClient(kit.http, "https://kulturalne-oborniki.bilety24.pl/repertuar/", KinoAstra, today = kit.today, titles = kit.titles)),
    filmweb(1645, KinoDyskusyjnyKlubFilmowyPolitechnika),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoNowyPafawag, "https://biletyna.pl/Wroclaw/Centrum-Kultury-Wroclaw-Zachod"),   // Wrocław
    bilety24("https://www.bilety24.pl/kino/organizator/kino-odra-1601", KinoOdraBrzegDolny),   // Brzeg Dolny
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-sztuki-w-olawie-1498", KinoOdraOlawa),   // Oława
    msi(GoKinoOlawa, "https://olawa.gokino.pl"),   // Oława
    msi(KinoGrazyna, "https://bilety.kinostrzelin.pl"),   // Strzelin
    bilety24("https://www.bilety24.pl/kino/organizator/kino-polonia-1090", KinoPoloniaTrzebnica),   // Trzebnica
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-i-sztuki-w-olesnicy-1193", KinoMOKiSOlesnica),   // Oleśnica
  )
}
