package services.cinemas

import java.util.Locale

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
  /** The canonicalisation language when a caller doesn't specify one — Polish,
   *  the historical default that keeps every existing single-country call site
   *  and its expected strings unchanged. */
  val DefaultLanguage: Locale = Locale.forLanguageTag("pl-PL")

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
    "Islandia", "Słowenia", "Chorwacja", "Serbia", "Luksemburg", "ZSRR", "Irak"
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
      "wlk. brytania"               -> "Wielka Brytania",
      "wlk brytania"                -> "Wielka Brytania",
      // Iraq
      "iraq"                        -> "Irak",
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
    val trimmed = raw.trim
    Aliases.getOrElse(trimmed.toLowerCase, trimmed)
  }

  /** Canonicalise a raw country name for a deployment serving `language`.
   *
   *  Poland folds every spelling variant into the curated Polish short-name set
   *  above (the form Polish cinema scrapers and the Polish corpus already use).
   *
   *  Any OTHER language must ALSO fold variants — otherwise a film whose sources
   *  report "USA", "United States" and "United States of America" surfaces all
   *  three side by side. We reuse the existing alias map as the folding key: the
   *  raw name folds to its Polish canonical, that maps to an ISO 3166-1 code
   *  ([[IsoOf]]), and the code renders as the country's display name in the
   *  target language ("US" → "United States" in English, "Vereinigte Staaten" in
   *  German). Names the alias map doesn't recognise (or that have no ISO entry)
   *  fall back to the trimmed input, so an exotic country still passes through. */
  def canonical(raw: String, language: Locale): String =
    if (language.getLanguage == "pl") canonical(raw)
    else {
      val folded = canonical(raw) // fold spelling variants via the alias map first
      IsoOf.get(folded.toLowerCase) match {
        case Some(iso) =>
          val name = Locale.of("", iso).getDisplayCountry(language)
          // The JDK returns the bare code for a country it can't localise; keep
          // the folded name rather than leaking "US" to the reader.
          if (name.nonEmpty && name != iso) name else folded
        case None => folded
      }
    }

  /** Canonical Polish name (lowercased) → ISO 3166-1 alpha-2. One entry per
   *  country the corpus surfaces; the pivot that lets a non-Polish deployment
   *  fold + localise without a second per-language alias map. */
  private val IsoOf: Map[String, String] = Map(
    "polska" -> "PL", "usa" -> "US", "niemcy" -> "DE", "francja" -> "FR", "włochy" -> "IT",
    "hiszpania" -> "ES", "belgia" -> "BE", "maroko" -> "MA", "łotwa" -> "LV", "węgry" -> "HU",
    "dania" -> "DK", "norwegia" -> "NO", "czechy" -> "CZ", "irlandia" -> "IE",
    "wielka brytania" -> "GB", "holandia" -> "NL", "szwecja" -> "SE", "finlandia" -> "FI",
    "szwajcaria" -> "CH", "austria" -> "AT", "portugalia" -> "PT", "rosja" -> "RU",
    "turcja" -> "TR", "ukraina" -> "UA", "białoruś" -> "BY", "litwa" -> "LT", "estonia" -> "EE",
    "słowacja" -> "SK", "rumunia" -> "RO", "bułgaria" -> "BG", "grecja" -> "GR", "indie" -> "IN",
    "pakistan" -> "PK", "japonia" -> "JP", "chiny" -> "CN", "korea południowa" -> "KR",
    "korea północna" -> "KP", "iran" -> "IR", "izrael" -> "IL", "brazylia" -> "BR",
    "argentyna" -> "AR", "australia" -> "AU", "kanada" -> "CA", "meksyk" -> "MX",
    "nowa zelandia" -> "NZ", "egipt" -> "EG", "rpa" -> "ZA", "hongkong" -> "HK", "tajwan" -> "TW",
    "wietnam" -> "VN", "palestyna" -> "PS", "katar" -> "QA", "islandia" -> "IS", "słowenia" -> "SI",
    "chorwacja" -> "HR", "serbia" -> "RS", "luksemburg" -> "LU", "irak" -> "IQ",
    "arabia saudyjska" -> "SA", "peru" -> "PE"
  )

  def isPolish(name: String): Boolean = Polish.contains(canonical(name))
}
