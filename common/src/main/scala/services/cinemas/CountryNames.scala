package services.cinemas

/**
 * Polish-language production-country names recognised across cinema
 * scrapers, plus an alias map that folds every spelling variant a source
 * might return into one canonical name. Without normalisation a single
 * film co-produced by Helios's "USA" + a hypothetical "Stany Zjednoczone"
 * from another cinema would surface as both on the merged record; with
 * it, both resolve to "USA".
 *
 * Usage:
 *   - Parsers stay simple (return whatever the source said).
 *   - `MovieCache.recordCinemaScrape` calls `canonical` per entry when
 *     building the SourceData slot, so stored data is already
 *     canonical and `MovieRecord.countries`' union/dedup operates on
 *     consistent strings.
 *   - `isPolish` is the dictionary-membership check used by Kino
 *     Pałacowe to separate co-director chunks from country chunks in
 *     its bilingual meta line, and by Apollo to validate whether its
 *     `Producent:` line is a list of countries or a studio name.
 */
object CountryNames {
  /** Canonical Polish names — one entry per country. Whatever a source
   *  returns gets folded into one of these (or passes through verbatim
   *  if `canonical` doesn't know an alias). */
  val Polish: Set[String] = Set(
    "Polska", "USA", "Niemcy", "Francja", "Włochy", "Hiszpania", "Belgia", "Maroko",
    "Łotwa", "Węgry", "Dania", "Norwegia", "Czechy", "Irlandia",
    "Wielka Brytania", "Holandia", "Szwecja", "Finlandia", "Szwajcaria",
    "Austria", "Portugalia", "Rosja", "Turcja", "Ukraina", "Białoruś", "Litwa",
    "Estonia", "Słowacja", "Rumunia", "Bułgaria", "Grecja", "Indie", "Pakistan",
    "Japonia", "Chiny", "Korea Południowa", "Korea Północna", "Iran", "Izrael",
    "Brazylia", "Argentyna", "Australia", "Kanada", "Meksyk", "Nowa Zelandia",
    "Egipt", "RPA", "Hongkong", "Tajwan", "Wietnam", "Palestyna", "Katar",
    "Islandia", "Słowenia", "Chorwacja", "Serbia", "Luksemburg", "ZSRR"
  )

  /** Aliases → canonical. Keys are lowercased for case-insensitive lookup
   *  (so `francja`, `FRANCJA`, `Francja` all resolve to `Francja`). Add a
   *  new entry per spelling/translation the corpus surfaces.
   *
   *  English-language entries are defensive — some Helios REST payloads
   *  occasionally come back English; better to map than to leak. */
  private val Aliases: Map[String, String] = (
    Polish.iterator.map(c => c.toLowerCase -> c).toMap ++ Map(
      // United States
      "stany zjednoczone"           -> "USA",
      "stany zjednoczone ameryki"   -> "USA",
      "united states"               -> "USA",
      "united states of america"    -> "USA",
      "u.s."                        -> "USA",
      "u.s.a."                      -> "USA",
      "us"                          -> "USA",
      // United Kingdom
      "uk"                          -> "Wielka Brytania",
      "u.k."                        -> "Wielka Brytania",
      "united kingdom"              -> "Wielka Brytania",
      "great britain"               -> "Wielka Brytania",
      "anglia"                      -> "Wielka Brytania",
      // Italy
      "italia"                      -> "Włochy",
      "italy"                       -> "Włochy",
      // Germany
      "germany"                     -> "Niemcy",
      "deutschland"                 -> "Niemcy",
      // Czechia
      "republika czeska"            -> "Czechy",
      "czech republic"              -> "Czechy",
      "czechia"                     -> "Czechy",
      // Netherlands
      "netherlands"                 -> "Holandia",
      "niderlandy"                  -> "Holandia",
      // France
      "france"                      -> "Francja",
      // Spain
      "spain"                       -> "Hiszpania",
      "españa"                      -> "Hiszpania",
      // Poland
      "poland"                      -> "Polska",
      // Korea
      "south korea"                 -> "Korea Południowa",
      "north korea"                 -> "Korea Północna",
      "korea"                       -> "Korea Południowa",
      // Soviet Union
      "soviet union"                -> "ZSRR",
      "ussr"                        -> "ZSRR",
      "związek radziecki"           -> "ZSRR",
      // Other
      "japan"                       -> "Japonia",
      "china"                       -> "Chiny",
      "india"                       -> "Indie",
      "russia"                      -> "Rosja",
      "ukraine"                     -> "Ukraina",
      "belarus"                     -> "Białoruś",
      "hong kong"                   -> "Hongkong",
      "taiwan"                      -> "Tajwan",
      "vietnam"                     -> "Wietnam",
      "palestine"                   -> "Palestyna",
      "qatar"                       -> "Katar",
      "iceland"                     -> "Islandia",
      "slovenia"                    -> "Słowenia",
      "croatia"                     -> "Chorwacja",
      "serbia"                      -> "Serbia",
      "saudi arabia"                -> "Arabia Saudyjska",
      "arabia saudyjska"            -> "Arabia Saudyjska",
      "peru"                        -> "Peru",
      "argentina"                   -> "Argentyna",
      "brazil"                      -> "Brazylia",
      "australia"                   -> "Australia",
      "canada"                      -> "Kanada",
      "mexico"                      -> "Meksyk",
      "new zealand"                 -> "Nowa Zelandia",
      "egypt"                       -> "Egipt",
      "south africa"                -> "RPA",
      "ireland"                     -> "Irlandia",
      "sweden"                      -> "Szwecja",
      "norway"                      -> "Norwegia",
      "denmark"                     -> "Dania",
      "finland"                     -> "Finlandia",
      "switzerland"                 -> "Szwajcaria",
      "austria"                     -> "Austria",
      "portugal"                    -> "Portugalia",
      "greece"                      -> "Grecja",
      "turkey"                      -> "Turcja",
      "israel"                      -> "Izrael",
      "iran"                        -> "Iran",
      "pakistan"                    -> "Pakistan",
      "estonia"                     -> "Estonia",
      "latvia"                      -> "Łotwa",
      "lithuania"                   -> "Litwa",
      "slovakia"                    -> "Słowacja",
      "romania"                     -> "Rumunia",
      "bulgaria"                    -> "Bułgaria",
      "hungary"                     -> "Węgry",
      "belgium"                     -> "Belgia",
      "morocco"                     -> "Maroko",
      "luxembourg"                  -> "Luksemburg",
      "luxemburg"                   -> "Luksemburg"
    )
  )

  /** Resolve a raw country string to its canonical Polish name. Falls
   *  back to the trimmed input when no alias is known — preserves new /
   *  exotic country names rather than dropping them silently. Matching
   *  is case-insensitive and whitespace-tolerant. */
  def canonical(raw: String): String = {
    val t = raw.trim
    Aliases.getOrElse(t.toLowerCase, t)
  }

  def isPolish(name: String): Boolean = Polish.contains(canonical(name))
}
