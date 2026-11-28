import Foundation

/// One cinema's slice of a filtered film list: every film that plays at
/// this cinema, with each film's `showings` restricted to this cinema's
/// dates and slots. Drives the Kina tab's cinema-grouped layout —
/// mirrors the web's `_cinemaCards` (one `.cinema-section` per cinema,
/// holding film cards whose `_filmShowings` see only that cinema's
/// slots).
struct CinemaSection: Identifiable, Hashable {
    var id: String { cinema }
    let cinema: String
    let films: [Film]

    /// Full cinema-name → short pill label map, one entry per cinema across
    /// every city — mirrors the web's `Cinema.pillMap` (and grouped in the same
    /// city order) so a pill reads identically on every platform. A name absent
    /// here falls back to itself via `pillName(for:)`.
    static let pillNames: [String: String] = [
        // Poznań
        "Kino Apollo": "Apollo",
        "Kino Bułgarska 19": "Bułgarska 19",
        "Kino Malta Charlie Monroe": "Malta Charlie Monroe",
        "Helios Posnania": "Helios",
        "Cinema City Kinepolis": "Kinepolis",
        "Kino Muza": "Muza",
        "Multikino Stary Browar": "Multikino",
        "Kino Pałacowe": "Pałacowe",
        "Cinema City Poznań Plaza": "Poznań Plaza",
        "Kino Rialto": "Rialto",
        // Wrocław
        "Cinema City Wroclavia": "Wroclavia",
        "Cinema City Korona": "Korona",
        "Multikino Pasaż Grunwaldzki": "Pasaż Grunwaldzki",
        "Helios Magnolia Park": "Magnolia",
        "Helios Aleja Bielany": "Aleja Bielany",
        "Kino Nowe Horyzonty": "Nowe Horyzonty",
        "Dolnośląskie Centrum Filmowe": "DCF",
        // Warszawa
        "Cinema City Arkadia": "Arkadia",
        "Cinema City Bemowo": "Bemowo",
        "Cinema City Galeria Północna": "Galeria Północna",
        "Cinema City Janki": "Janki",
        "Cinema City Mokotów": "Mokotów",
        "Cinema City Promenada": "Promenada",
        "Cinema City Sadyba": "Sadyba",
        "Multikino Złote Tarasy": "Złote Tarasy",
        "Multikino Młociny": "Młociny",
        "Multikino Reduta": "Reduta",
        "Multikino Targówek": "Targówek",
        "Multikino Wola Park": "Wola Park",
        "Helios Blue City": "Blue City",
        "Kino Muranów": "Muranów",
        "Kino Luna": "Luna",
        "Kino Elektronik": "Elektronik",
        "Kino Iluzjon": "Iluzjon",
        "KinoGram": "KinoGram",
        "Kino Kultura": "Kultura",
        "Kino Amondo": "Amondo",
        "Kino na Boku": "na Boku",
        "Kino Głębocka 66": "Głębocka 66",
        "KINOMUZEUM": "Kinomuzeum",
        "Kino Świt": "Świt",
        "Kino Kępa": "Kępa",
        "KINOkawiarnia Stacja Falenica": "Stacja Falenica",
        "Służewski Dom Kultury": "SDK",
        "Kino Atlantic": "Atlantic",
        "Kinoteka": "Kinoteka",
        "Kino U-jazdowski": "U-jazdowski",
        "Kino Cytadela": "Cytadela",
        // Kraków
        "Cinema City Bonarka": "Bonarka",
        "Cinema City Kazimierz": "Kazimierz",
        "Cinema City Zakopianka": "Zakopianka",
        "Multikino Kraków": "Multikino",
        "Kino Mikro": "Mikro",
        "Mikro Bronowice": "Mikro Bronowice",
        "Kino Sfinks": "Sfinks",
        // Łódź
        "Cinema City Manufaktura": "Manufaktura",
        "Multikino Łódź": "Multikino",
        "Helios Łódź": "Helios",
        "Kino Charlie": "Charlie",
        // Katowice
        "Cinema City Punkt 44": "Punkt 44",
        "Cinema City Silesia": "Silesia",
        "Multikino Katowice": "Multikino",
        "Helios Katowice": "Helios",
        "Kino Kosmos": "Kosmos",
        "Kino Światowid": "Światowid",
        // Szczecin
        "Helios Kupiec": "Helios",
        "Multikino Szczecin": "Multikino",
        "Kino Pionier 1907": "Pionier",
        // Białystok
        "Helios Alfa": "Alfa",
        "Helios Biała": "Biała",
        "Helios Jurowiecka": "Jurowiecka",
        "Kino Forum": "Forum",
        // Trójmiasto (Gdańsk · Gdynia · Sopot)
        "Multikino Gdańsk": "Multikino",
        "Helios Metropolia": "Metropolia",
        "Helios Forum": "Forum",
        "Helios Riviera": "Riviera",
        "Kino Spektrum": "Spektrum",
        "Kino Kameralne Cafe": "Kameralne",
        "Kino IKM": "IKM",
        "Kino Muzeum": "Muzeum",
        "Kino Żak": "Żak",
        "KinoPort": "KinoPort",
        // Bydgoszcz
        "Cinema City Bydgoszcz": "Cinema City",
        "Multikino Bydgoszcz": "Multikino",
        "Helios Bydgoszcz": "Helios",
        "Kino Orzeł": "Orzeł",
        // Lublin
        "Cinema City Felicity": "Felicity",
        "Cinema City Lublin Plaza": "Lublin Plaza",
        "Multikino Lublin": "Multikino",
        "Kino Bajka": "Bajka",
        // Częstochowa
        "Cinema City Jurajska": "Jurajska",
        "Cinema City Wolność": "Wolność",
        // Radom
        "Helios Radom": "Helios",
        "Multikino Radom": "Multikino",
        // Sosnowiec
        "Helios Sosnowiec": "Helios",
        "Cinema City Sosnowiec": "Cinema City",
        // Toruń
        "Cinema City Czerwona Droga": "Czerwona Droga",
        "Cinema City Toruń Plaza": "Plaza",
        // Kielce
        "Helios Kielce": "Helios",
        "Multikino Kielce": "Multikino",
        // Rzeszów
        "Helios Rzeszów": "Helios",
        "Multikino Rzeszów": "Multikino",
        "Kino Zorza": "Zorza",
        // Gliwice
        "Cinema City Gliwice": "Cinema City",
        // Zabrze
        "Multikino Zabrze": "Multikino",
    ]

    static func pillName(for cinema: String) -> String {
        pillNames[cinema] ?? cinema
    }
}
