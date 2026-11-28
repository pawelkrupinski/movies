package pl.kinowo.filter

/**
 * Long cinema name → short pill label, one entry per cinema across every city — mirrors the
 * web's `Cinema.pillMap` (same city grouping/order). Split by region so each literal stays short.
 */
internal object CinemaPillNames {
    private val part1: Map<String, String> = mapOf(
        // Poznań
        "Kino Apollo" to "Apollo",
        "Kino Bułgarska 19" to "Bułgarska 19",
        "Kino Malta Charlie Monroe" to "Malta Charlie Monroe",
        "Helios Posnania" to "Helios",
        "Cinema City Kinepolis" to "Kinepolis",
        "Kino Muza" to "Muza",
        "Multikino Stary Browar" to "Multikino",
        "Kino Pałacowe" to "Pałacowe",
        "Cinema City Poznań Plaza" to "Poznań Plaza",
        "Kino Rialto" to "Rialto",
        // Wrocław
        "Cinema City Wroclavia" to "Wroclavia",
        "Cinema City Korona" to "Korona",
        "Multikino Pasaż Grunwaldzki" to "Pasaż Grunwaldzki",
        "Helios Magnolia Park" to "Magnolia",
        "Helios Aleja Bielany" to "Aleja Bielany",
        "Kino Nowe Horyzonty" to "Nowe Horyzonty",
        "Dolnośląskie Centrum Filmowe" to "DCF",
    )

    private val part2: Map<String, String> = mapOf(
        // Warszawa
        "Cinema City Arkadia" to "Arkadia",
        "Cinema City Bemowo" to "Bemowo",
        "Cinema City Galeria Północna" to "Galeria Północna",
        "Cinema City Janki" to "Janki",
        "Cinema City Mokotów" to "Mokotów",
        "Cinema City Promenada" to "Promenada",
        "Cinema City Sadyba" to "Sadyba",
        "Multikino Złote Tarasy" to "Złote Tarasy",
        "Multikino Młociny" to "Młociny",
        "Multikino Reduta" to "Reduta",
        "Multikino Targówek" to "Targówek",
        "Multikino Wola Park" to "Wola Park",
        "Helios Blue City" to "Blue City",
        "Kino Muranów" to "Muranów",
        "Kino Luna" to "Luna",
        "Kino Elektronik" to "Elektronik",
        "Kino Iluzjon" to "Iluzjon",
        "KinoGram" to "KinoGram",
        "Kino Kultura" to "Kultura",
        "Kino Amondo" to "Amondo",
        "Kino na Boku" to "na Boku",
        "Kino Głębocka 66" to "Głębocka 66",
        "KINOMUZEUM" to "Kinomuzeum",
        "Kino Świt" to "Świt",
        "Kino Kępa" to "Kępa",
        "KINOkawiarnia Stacja Falenica" to "Stacja Falenica",
        "Służewski Dom Kultury" to "SDK",
        "Kino Atlantic" to "Atlantic",
        "Kinoteka" to "Kinoteka",
        "Kino U-jazdowski" to "U-jazdowski",
        "Kino Cytadela" to "Cytadela",
    )

    private val part3: Map<String, String> = mapOf(
        // Kraków
        "Cinema City Bonarka" to "Bonarka",
        "Cinema City Kazimierz" to "Kazimierz",
        "Cinema City Zakopianka" to "Zakopianka",
        "Multikino Kraków" to "Multikino",
        "Kino Mikro" to "Mikro",
        "Mikro Bronowice" to "Mikro Bronowice",
        "Kino Sfinks" to "Sfinks",
        // Łódź
        "Cinema City Manufaktura" to "Manufaktura",
        "Multikino Łódź" to "Multikino",
        "Helios Łódź" to "Helios",
        "Kino Charlie" to "Charlie",
        // Katowice
        "Cinema City Punkt 44" to "Punkt 44",
        "Cinema City Silesia" to "Silesia",
        "Multikino Katowice" to "Multikino",
        "Helios Katowice" to "Helios",
        "Kino Kosmos" to "Kosmos",
        "Kino Światowid" to "Światowid",
        // Szczecin
        "Helios Kupiec" to "Helios",
        "Multikino Szczecin" to "Multikino",
        "Kino Pionier 1907" to "Pionier",
        // Białystok
        "Helios Alfa" to "Alfa",
        "Helios Biała" to "Biała",
        "Helios Jurowiecka" to "Jurowiecka",
        "Kino Forum" to "Forum",
    )

    private val part4: Map<String, String> = mapOf(
        // Trójmiasto (Gdańsk · Gdynia · Sopot)
        "Multikino Gdańsk" to "Multikino",
        "Helios Metropolia" to "Metropolia",
        "Helios Forum" to "Forum",
        "Helios Riviera" to "Riviera",
        "Kino Spektrum" to "Spektrum",
        "Kino Kameralne Cafe" to "Kameralne",
        "Kino IKM" to "IKM",
        "Kino Muzeum" to "Muzeum",
        "Kino Żak" to "Żak",
        "KinoPort" to "KinoPort",
        // Bydgoszcz
        "Cinema City Bydgoszcz" to "Cinema City",
        "Multikino Bydgoszcz" to "Multikino",
        "Helios Bydgoszcz" to "Helios",
        "Kino Orzeł" to "Orzeł",
        // Lublin
        "Cinema City Felicity" to "Felicity",
        "Cinema City Lublin Plaza" to "Lublin Plaza",
        "Multikino Lublin" to "Multikino",
        "Kino Bajka" to "Bajka",
        // Częstochowa
        "Cinema City Jurajska" to "Jurajska",
        "Cinema City Wolność" to "Wolność",
        // Radom
        "Helios Radom" to "Helios",
        "Multikino Radom" to "Multikino",
    )

    private val part5: Map<String, String> = mapOf(
        // Sosnowiec
        "Helios Sosnowiec" to "Helios",
        "Cinema City Sosnowiec" to "Cinema City",
        // Toruń
        "Cinema City Czerwona Droga" to "Czerwona Droga",
        "Cinema City Toruń Plaza" to "Plaza",
        // Kielce
        "Helios Kielce" to "Helios",
        "Multikino Kielce" to "Multikino",
        // Rzeszów
        "Helios Rzeszów" to "Helios",
        "Multikino Rzeszów" to "Multikino",
        "Kino Zorza" to "Zorza",
        // Gliwice
        "Cinema City Gliwice" to "Cinema City",
        // Zabrze
        "Multikino Zabrze" to "Multikino",
    )

    val all: Map<String, String> = part1 + part2 + part3 + part4 + part5
}
