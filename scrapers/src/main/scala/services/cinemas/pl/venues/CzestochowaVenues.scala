package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

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
