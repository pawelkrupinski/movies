package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Trojmiasto: see [[PolishVenueRegistry]]. */
object TrojmiastoVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0004", MultikinoGdansk),
    helios(HeliosVenues.Metropolia),
    helios(HeliosVenues.Forum),
    helios(HeliosVenues.Riviera),
    own(kit => new KinoSpektrumClient(kit.http, KinoSpektrum)),
    // biletyna.pl 403s our datacenter IP, so route through `bnFetch` — the
    // residential proxy in production, the fixture fake in
    // tests. See EgressWiring.biletynaFetch.
    biletyna(KinoKameralne, "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe"),
    own(kit => new KinoIkmClient(kit.http, KinoIkm, kit.today)),
    own(kit => new KinoMuzeumGdanskClient(kit.http, KinoMuzeumGdansk)),
    own(kit => new KinoZakClient(kit.http, KinoZak, kit.today)),
    // KinoPort (GCSW, formerly CSW Łaźnia, Gdańsk) was read off Filmweb (id 1735)
    // after a 2026-06 site rebuild dropped its stable programme alias. Filmweb
    // then went silently empty for it — `[]` on every date while the venue was
    // screening five films a day — so we read GCSW's own repertoire post instead,
    // via the WP REST route that survives the post's rotating permalink.
    own(kit => new KinoPortClient(kit.http, KinoPort, kit.today)),
    // Migrated off the legacy MSI portal onto the "POSitive Cinema" Angular SPA
    // platform in 2026; the SPA renders no server HTML, so this reads the
    // platform's own JSON REST API directly (see Cinema1Client's scaladoc).
    own(kit => new Cinema1Client(kit.http, Cinema1Gdansk, cinemaId = "8d3b10d9-f892-4f57-bf74-9f86905ce3ea", today = kit.today)),
    own(kit => new GdynskieCentrumFilmoweClient(kit.http, GdynskieCentrumFilmowe)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    biletyna(KinoNaSzekspirowskim, "https://biletyna.pl/Gdansk/Kino-na-Szekspirowskim"),
    multikino("0027", MultikinoRumia),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(TeatrBoto, "https://biletyna.pl/Sopot/Teatr-Boto"),   // Sopot
    biletyna(TeatrAtelier, "https://biletyna.pl/Sopot/Teatr-Atelier-Sopot"),   // Sopot
    biletyna(KinoCKGniewino, "https://biletyna.pl/Gniewino/Centrum-Kultury-Sportu-Turystyki-i-Biblioteka-w-Gniewinie"),   // Gniewino
    own(kit => new IksorisCalendarClient(kit.http, IksorisBookingPage(IksorisOrigin("https://bilety.wck.org.pl")), KinoWCK, kit.today)),   // Wejherowo
    biletyna(KinoZaRogiemChmielno, "https://biletyna.pl/Chmielno/Kino-za-Rogiem-w-Chmielnie"),   // Chmielno
    bilety24("https://www.bilety24.pl/kino/organizator/kino-zeglarz-1224", KinoZeglarz),   // Jastarnia
    bilety24("https://www.bilety24.pl/kino/organizator/kartuskie-centrum-kultury-w-kartuzach-1364", KinoKCK),   // Kartuzy
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-sportu-w-pruszczu-gdanskim-1569", KinoCKiSPruszczGdanski),   // Pruszcz Gdański
  )
}
