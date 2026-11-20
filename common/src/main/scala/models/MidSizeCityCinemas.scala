package models

// ── New mid-size cities — national-chain branches only (chains-first sweep) ────
// Each city's independent/art-house screen, where one exists, lands later as a
// bespoke scraper. IDs verified live against each chain's own cinema-list API.

// Olsztyn
case object HeliosOlsztyn    extends Cinema("Helios Olsztyn", "Helios")
case object MultikinoOlsztyn extends Cinema("Multikino Olsztyn", "Multikino")
// Bielsko-Biała
case object HeliosBielskoBiala     extends Cinema("Helios Bielsko-Biała", "Helios")
case object CinemaCityBielskoBiala extends Cinema("Cinema City Bielsko-Biała", "Cinema City")
// Opole
case object HeliosOpoleKarolinka extends Cinema("Helios Karolinka", "Karolinka")
case object HeliosOpoleSolaris   extends Cinema("Helios Solaris", "Solaris")
// Rybnik
case object MultikinoRybnik  extends Cinema("Multikino Rybnik", "Multikino")
case object CinemaCityRybnik extends Cinema("Cinema City Rybnik", "Cinema City")
// Gorzów Wielkopolski
case object HeliosGorzow    extends Cinema("Helios Gorzów Wielkopolski", "Helios")
case object MultikinoGorzow extends Cinema("Multikino Gorzów Wielkopolski", "Multikino")
// Elbląg
case object MultikinoElblag  extends Cinema("Multikino Elbląg", "Multikino")
case object CinemaCityElblag extends Cinema("Cinema City Elbląg", "Cinema City")
// Koszalin
case object HeliosKoszalin    extends Cinema("Helios Koszalin", "Helios")
case object MultikinoKoszalin extends Cinema("Multikino Koszalin", "Multikino")
// Kalisz
case object HeliosKalisz    extends Cinema("Helios Kalisz", "Helios")
case object MultikinoKalisz extends Cinema("Multikino Kalisz", "Multikino")
// Zielona Góra
case object CinemaCityZielonaGora extends Cinema("Cinema City Zielona Góra", "Cinema City")
// Tychy
case object MultikinoTychy extends Cinema("Multikino Tychy", "Multikino")
// Wałbrzych
case object CinemaCityWalbrzych extends Cinema("Cinema City Wałbrzych", "Cinema City")
// Tarnów
case object MultikinoTarnow extends Cinema("Multikino Tarnów", "Multikino")
// Włocławek
case object MultikinoWloclawek extends Cinema("Multikino Włocławek", "Multikino")
// Legnica
case object HeliosLegnica extends Cinema("Helios Legnica", "Helios")
// Płock
case object HeliosPlock extends Cinema("Helios Płock", "Helios")
// Bytom
case object CinemaCityBytom extends Cinema("Cinema City Bytom", "Cinema City")
// Dąbrowa Górnicza
case object HeliosDabrowaGornicza extends Cinema("Helios Dąbrowa Górnicza", "Helios")
// Nowy Sącz
case object HeliosNowySacz extends Cinema("Helios Nowy Sącz", "Helios")
// Słupsk
case object MultikinoSlupsk extends Cinema("Multikino Słupsk", "Multikino")
// Jelenia Góra
case object HeliosJeleniaGora extends Cinema("Helios Jelenia Góra", "Helios")
// Przemyśl
case object HeliosPrzemysl extends Cinema("Helios Przemyśl", "Helios")
// Konin
case object HeliosKonin extends Cinema("Helios Konin", "Helios")
case object KinoOskard  extends Cinema("Kino Oskard", "Oskard")  // Konin — bilety24

// ── Independent cinemas served via shared platform clients ────────────────────
// No bespoke scraper needed: each reuses an existing, fixture-tested client —
// FilmwebShowtimesClient (by Filmweb internal cinema id), Bilety24Client (by
// bilety24.pl venue URL), or NoveKinoClient (by novekino.pl slug). Filmweb ids
// were verified to return non-empty seances on current dates.
case object KinoAwangarda2      extends Cinema("Kino Awangarda 2", "Awangarda 2")     // Olsztyn — filmweb
case object KinoKryterium       extends Cinema("Kino Kryterium", "Kryterium")         // Koszalin — filmweb
case object KinoRejs            extends Cinema("Kino Rejs", "Rejs")                   // Słupsk — filmweb
case object KinoKreska          extends Cinema("Kino Kreska", "Kreska")              // Bielsko-Biała — filmweb
case object KinoMeduza          extends Cinema("Kino Meduza", "Meduza")              // Opole — filmweb
case object KinoKadr            extends Cinema("Kino Studyjne Kadr", "Kadr")          // Dąbrowa Górnicza — filmweb
case object KinoSokol           extends Cinema("Kino Sokół", "Sokół")               // Nowy Sącz — filmweb
case object KinoMillenium       extends Cinema("Kino Millenium", "Millenium")         // Tarnów — filmweb
case object Kino60Krzesel       extends Cinema("Kino 60 Krzeseł", "60 Krzeseł")      // Gorzów Wielkopolski — filmweb
case object KinoTatry           extends Cinema("Kino Tatry", "Tatry")               // Łódź — filmweb
case object KinoChatkaZaka      extends Cinema("Kino Chatka Żaka", "Chatka Żaka")    // Lublin — filmweb
case object KinoApolloWalbrzych extends Cinema("Kino Apollo Wałbrzych", "Apollo")     // Wałbrzych — bilety24 (DCF)
case object KinoPiast           extends Cinema("Kino Piast", "Piast")               // Legnica — bilety24 (DCF)
case object KinoLot             extends Cinema("Kino Lot", "Lot")                   // Jelenia Góra — bilety24 (DCF)
case object KinoPrzedwiosnie    extends Cinema("Kino Przedwiośnie", "Przedwiośnie")  // Płock — novekino
// Konin catchment: the remaining venues Filmweb lists under its Konin showtimes
// page, each in a nearby town. Helios Konin (chain client) and Kino Oskard
// (Bilety24, above) are already covered; Września's Kino Trójka is excluded.
case object KinoZacheta         extends Cinema("Kino Zachęta", "Zachęta")            // Kleczew — filmweb
case object KinoNadWarta        extends Cinema("Kino nad Wartą", "nad Wartą")        // Koło — bilety24 1621 (filmweb 1526 lists a day or two)
case object KinoStudyjneCentrum extends Cinema("Kino Studyjne Centrum", "Centrum")  // Konin — bilety24
case object KinoHel             extends Cinema("Kino Hel", "Hel")                   // Pleszew — filmweb
case object KinoSokolnia        extends Cinema("Kino Sokolnia", "Sokolnia")          // Słupca — filmweb
case object KinoTur             extends Cinema("Kino Tur", "Tur")                   // Turek — filmweb
case object KinoMok             extends Cinema("Kino MOK", "MOK")                   // Zagórów — filmweb
