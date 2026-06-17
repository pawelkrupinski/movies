package services.titlerules

import RuleScope._

/** The seed rule set — every formerly-hardcoded `TitleNormalizer` regex,
 *  transcribed verbatim and tagged with the tier it ran in. This is BOTH:
 *
 *   1. the in-code fallback `TitleNormalizer` installs at class-load, so the
 *      normaliser behaves correctly before Mongo is read and in tests that
 *      don't wire a rules store; and
 *   2. the documents `SeedTitleRules` upserts into an empty `titleRules`
 *      collection.
 *
 *  `TitleRuleMigrationSpec` asserts that running titles through this set
 *  produces byte-identical output to the pre-rules implementation. DO NOT edit a
 *  pattern here to change behaviour — edit the rule in the DB via the admin page;
 *  this set only exists to reproduce the original baseline.
 *
 *  Per-cinema rules (the old per-client `cleanTitle`) are added in a later step;
 *  at this baseline the per-cinema tier is empty and clients still clean inline. */
object TitleRuleDefaults {

  // ── apiQuery tier (merged GlobalStructural) — programme/access/event strips
  //    fold FIRST (orders 10–14), then the decoration strips (orders 20–60),
  //    reproducing the legacy `structural(searchRules(t))` composition
  //    byte-for-byte. NOT in the merge key — a decoration/programme edition keys
  //    by its own form and stays a separate row. ──────────────────────────────
  private val ProgrammePrefixPattern =
    """(?i)^(?:Kino\s+bez\s+barier|""" +
    """Pokaz\s+sensorycznie\s+przyjazny|""" +
    """Filmow[ey]\s+Poran(?:ki|ek)(?:\s+[^:]+)?|""" +
    """Zimowe\s+Poranki(?:\s+[^:]+)?|""" +
    """Poranek\s+dla\s+dzieci|""" +
    """Filmowy\s+Klub\s+Seniora|""" +
    """Dyskusyjny\s+Klub\s+Filmowy|""" +
    """Filmowe\s+spotkania\s+z\s+psychoanaliz[ąa]|""" +
    """Cinema\s+Italia\s+Oggi|""" +
    """Plenerowe\s+Pa[łl]acowe):\s+"""

  private val structural: Seq[TitleRule] = Seq(
    TitleRule("search-programme-prefix", GlobalStructural, None,
      ProgrammePrefixPattern, "", applyAll = false, order = 10,
      tag = Some("programmePrefix"),
      note = Some("Cinema programme banners (Kino bez barier, DKF, Filmowe Poranki…)")),
    TitleRule("search-accessibility-tag", GlobalStructural, None,
      """(?i)\s*\(\s*AD\b[^)]*\)?\s*$""", "", applyAll = false, order = 12,
      note = Some("Trailing accessibility tag: (AD), (AD + CC + PJM)")),
    TitleRule("search-plus-event-suffix", GlobalStructural, None,
      """\s+\+\s+\p{L}[^)]*$""", "", applyAll = false, order = 14,
      note = Some("'+ <event>' suffix: '+ spotkanie z producentką'")),
    // Case-insensitivity is scoped to the words ((?i:…)) — NOT global — because a
    // global (?i) makes Java's \p{Lu} also match lowercase, which would let the
    // lookahead accept 'spotkania trzeciego' and amputate 'Bliskie spotkania
    // trzeciego stopnia'. \p{Lu} must stay case-sensitive.
    TitleRule("search-meeting-suffix", GlobalStructural, None,
      """\s*[-–—|+]\s*(?:[^|]*?\s)?(?i:spotkani)\p{L}*""" +
        """(?=\s+(?i:z|ze|po|przed)\b|\s*[|+]|\s+\p{Lu}|\s*$).*$""",
      "", applyAll = false, order = 15,
      note = Some("'<sep> … spotkanie <…>' meeting suffix introduced by a separator " +
        "(+ | – —): '… | spotkanie z reżyserem', 'Carmilla – pokaz … + spotkanie z X'. " +
        "Lookahead (meeting tail = z/ze/po/przed, a pipe/plus, a Capitalised name, or " +
        "end) keeps a real film whose title contains 'spotkania' intact " +
        "(Bliskie spotkania trzeciego stopnia).")),
    TitleRule("search-spotkanie-banner-prefix", GlobalStructural, None,
      """(?i)^(?:Filmowe\s+)?spotkani\p{L}*[^:]*:\s*""", "", applyAll = false, order = 11,
      note = Some("'Spotkanie/Spotkania …:' meeting-cycle banner prefix: " +
        "'SPOTKANIA FILOZOFICZNE: Wędrówka na północ', 'FILMOWE SPOTKANIA Z PSYCHOLOGIĄ: " +
        "Ojczyzna'. Anchored at start so a film with its own colon " +
        "(SMOK (…). Trzy kolory: Czerwony) is left alone.")),
    TitleRule("search-with-event-suffix", GlobalStructural, None,
      """(?i)\s*[-–—]?\s*z\s+(?:autorską\s+narracją|prelekcj[ąae]|wprowadzeniem|udziałem)\S*.*$""",
      "", applyAll = false, order = 16,
      note = Some("'z <event>' screening suffix: 'z autorską narracją Łony', 'z prelekcją'")),
    TitleRule("structural-cykl-prefix", GlobalStructural, None,
      """^Cykl\s+[„"][^„""]*[„""]?\s+[-–—]\s+""", "", applyAll = false, order = 20,
      note = Some("Festival cycle banner: Cykl \"…\" – ")),
    TitleRule("structural-slash-suffix", GlobalStructural, None,
      """\s+/\s+.+$""", "", applyAll = false, order = 30,
      note = Some("Slash postfix: 'Top Gun / 40th Anniversary'")),
    TitleRule("structural-anniversary-suffix", GlobalStructural, None,
      """(?i)\s*[-–—|.]?\s*\d+(?:st|nd|rd|th)?\.?\s*(?:anniversary|rocznica)\s*$""", "",
      applyAll = false, order = 40, note = Some("Anniversary rerelease suffix")),
    TitleRule("structural-restored-suffix", GlobalStructural, None,
      """(?i)\s*[-–—|.]?\s*\d+\s*k\s+(?:restored|remaster(?:ed)?)\s*$""", "",
      applyAll = false, order = 50, note = Some("Restored / remastered suffix (4K restored)")),
    TitleRule("structural-wersja-suffix", GlobalStructural, None,
      """(?i)\s*[-–—.]\s+wersja\s+\p{L}+\s*$""", "", applyAll = false, order = 60,
      note = Some("Polish language-version suffix: '- wersja polska'"))
  )

  // ── canonical tier — cross-cinema spelling unifications (sanitize) ─────────
  private val canonical: Seq[TitleRule] = Seq(
    TitleRule("canonical-gwiezdne-wojny", Canonical, None,
      """^Gwiezdne Wojny: """, "", applyAll = false, order = 10,
      note = Some("Strip the 'Gwiezdne Wojny: ' franchise prefix")),
    TitleRule("canonical-ampersand-to-i", Canonical, None,
      """ & """, " i ", applyAll = true, order = 20,
      note = Some("Unify ' & ' and ' i ' spellings"))
  )

  // ── per-cinema tier — migrated from each client's `cleanTitle` ─────────────
  // Keyed by `TitleRuleKey.of(cinema)`. A chain shares one key across its venues.
  // As each client's inline `cleanTitle` is removed, its rules land here.
  private val perCinema: Seq[TitleRule] = Seq(
    // Cinema City (chain) — "Ladies Night - X", "X - powrót do kin",
    // "Kolekcja Mamoru Hosody: X".
    TitleRule("cc-ladies-night", PerCinema, Some("cinema-city"),
      "^Ladies Night - ", "", applyAll = false, order = 10,
      note = Some("Cinema City ladies-night prefix")),
    TitleRule("cc-powrot-do-kin", PerCinema, Some("cinema-city"),
      " - powrót do kin$", "", applyAll = false, order = 20,
      note = Some("Cinema City re-release suffix")),
    TitleRule("cc-mamoru-hosody", PerCinema, Some("cinema-city"),
      """^Kolekcja\s+Mamoru\s+Hosody:\s*""", "", applyAll = false, order = 30,
      note = Some("Cinema City anime-retrospective prefix")),
    // Multikino (chain) — "Kino na obcasach: X", "Kolekcja Mamoru Hosody: X".
    TitleRule("mk-kino-na-obcasach", PerCinema, Some("multikino"),
      """^Kino na obcasach:\s*""", "", applyAll = false, order = 10,
      note = Some("Multikino ladies' programme prefix")),
    TitleRule("mk-mamoru-hosody", PerCinema, Some("multikino"),
      """^Kolekcja\s+Mamoru\s+Hosody:\s*""", "", applyAll = false, order = 20,
      note = Some("Multikino anime-retrospective prefix")),
    // Kino Muza — "X | najlepsze z najgorszych" recurring-programme suffix.
    TitleRule("muza-najlepsze-z-najgorszych", PerCinema, Some("kino-muza"),
      """(?i)\s*\|\s*najlepsze\s+z\s+najgorszych\s*$""", "", applyAll = false, order = 10,
      note = Some("Kino Muza 'najlepsze z najgorszych' programme suffix")),
    // Kino Alternatywy — drop "Okładka" prefix + typographic quotes, collapse ws.
    TitleRule("alt-okladka", PerCinema, Some("kino-alternatywy"),
      """(?i)^okładka\s*""", "", applyAll = false, order = 10,
      note = Some("Kino Alternatywy 'Okładka' cover prefix")),
    TitleRule("alt-quotes", PerCinema, Some("kino-alternatywy"),
      """[„“”‟"]""", " ", applyAll = true, order = 20,
      note = Some("Kino Alternatywy typographic quote marks → space")),
    TitleRule("alt-collapse-ws", PerCinema, Some("kino-alternatywy"),
      """\s+""", " ", applyAll = true, order = 30,
      note = Some("Kino Alternatywy whitespace collapse")),
    // Kino Pałacowe — cycle/programme prefixes.
    TitleRule("palacowe-poranek", PerCinema, Some("kino-palacowe"),
      """^Poranek dla dzieci: """, "", applyAll = false, order = 10,
      note = Some("Kino Pałacowe kids-matinee prefix")),
    TitleRule("palacowe-dkf-zamek", PerCinema, Some("kino-palacowe"),
      """^DKF Zamek: """, "", applyAll = false, order = 20,
      note = Some("Kino Pałacowe film-club prefix")),
    TitleRule("palacowe-wajda", PerCinema, Some("kino-palacowe"),
      """^WAJDA: re-wizje\. """, "", applyAll = false, order = 30,
      note = Some("Kino Pałacowe Wajda retrospective prefix")),
    // Kinematograf Łódź — director + release-year suffixes the museum appends.
    TitleRule("kinematograf-rez", PerCinema, Some("kino-kinematograf"),
      """,\s*reż\.\s*.+$""", "", applyAll = false, order = 10,
      note = Some("Kinematograf ', reż. …' director suffix")),
    TitleRule("kinematograf-director", PerCinema, Some("kino-kinematograf"),
      """,\s+\p{Lu}\S+\s+\p{Lu}\S+$""", "", applyAll = false, order = 20,
      note = Some("Kinematograf bare ', Firstname Lastname' director suffix")),
    TitleRule("kinematograf-year", PerCinema, Some("kino-kinematograf"),
      """\s*\(\d{4}\)\s*$""", "", applyAll = false, order = 30,
      note = Some("Kinematograf trailing ' (YYYY)' suffix")),
    // BoK (both venues) — normalise nbsp+whitespace, drop a trailing ALL-CAPS
    // promo tag, rewrite remaining "|" separators to ": ".
    TitleRule("bok-ws", PerCinema, Some("bok"),
      """[\s ]+""", " ", applyAll = true, order = 10,
      note = Some("BoK whitespace + nbsp collapse")),
    TitleRule("bok-promo", PerCinema, Some("bok"),
      """\s*\|\s*[A-ZĄĆĘŁŃÓŚŹŻ0-9 ]{3,}\s*$""", "", applyAll = false, order = 20,
      note = Some("BoK trailing ALL-CAPS promo tag")),
    TitleRule("bok-pipe-to-colon", PerCinema, Some("bok"),
      """\s*\|\s*""", ": ", applyAll = true, order = 30,
      note = Some("BoK programme-banner '|' → ': '")),
    // Kino Apollo — kids'-day prefix + pre-premiere suffix.
    TitleRule("apollo-dzien-dziecka", PerCinema, Some("kino-apollo"),
      """^DZIEŃ DZIECKA W APOLLO - """, "", applyAll = false, order = 10,
      note = Some("Kino Apollo Children's-Day prefix")),
    TitleRule("apollo-przedpremierowy", PerCinema, Some("kino-apollo"),
      """ - seans przedpremierowy\b.*$""", "", applyAll = false, order = 20,
      note = Some("Kino Apollo pre-premiere suffix (+ any trailing words, e.g. 'w rocznicę koncertu')")),
    // Kino Wybrzeże (Kołobrzeg) appends its own venue name to every listing
    // ("Dzień objawienia-kino wybrzeże" / all-caps in the raw), which sanitises
    // to a different key and splits the film off its canonical row. Strip the
    // trailing "-kino wybrzeże" (any case) so it keys like every other cinema.
    // `(?iu)` not `(?i)`: Unicode case folding so the all-caps raw "…WYBRZEŻE"
    // folds (plain `(?i)` is ASCII-only and won't match Ż↔ż).
    TitleRule("wybrzeze-venue-suffix", PerCinema, Some("wybrzeze"),
      """(?iu)\s*-\s*kino\s+wybrzeże\s*$""", "", applyAll = false, order = 10,
      note = Some("Kino Wybrzeże trailing venue-name suffix"))
    // MSI format-tag stripping is NOT a per-cinema rule: it's a cross-client
    // concern shared across the portal clients via ScraperParse.stripFormatTags.
  )

  // Helios (chain) — ordered event/format suffix tags peeled off the end. Order
  // matters (matches HeliosNuxt's foldLeft): the event-source tag is peeled so a
  // preceding dubbing/napisy tag becomes the new suffix in the same pass. None of
  // these contain regex metacharacters, so `<suffix>$` is a safe literal match.
  private val heliosSuffixes = Seq(
    " w Helios RePlay", " w Helios Anime", " w Helios na Scenie", " w HnS",
    " - Salon Kultury Helios", " - KNTJ", " - KNT", " - Kino Kobiet",
    " - Kino Konesera", " - seanse z konkursami HDD", " - Event projekt",
    " - dubbing", " - Dubbing", " - napisy", " - NAP", " - DUB", " - AF")
  private val heliosRules: Seq[TitleRule] = heliosSuffixes.zipWithIndex.map { case (sfx, i) =>
    TitleRule(s"helios-suffix-${i + 1}", PerCinema, Some("helios"),
      sfx + "$", "", applyAll = false, order = (i + 1) * 10,
      note = Some(s"Helios '${sfx.trim}' tag"))
  }

  // Clients whose legacy cleanTitle ended with `.trim`. The others (Cinema City,
  // Multikino, Kino Pałacowe, Kino Apollo, Helios) deliberately did NOT trim, so
  // they get no trim rule and trailing whitespace is preserved as before.
  private val trimmingCinemas = Seq("kino-muza", "kino-alternatywy", "kino-kinematograf", "bok")
  private val trimRules: Seq[TitleRule] = trimmingCinemas.map { key =>
    TitleRule(s"$key-trim", PerCinema, Some(key), """^\s+|\s+$""", "", applyAll = true, order = 1000,
      note = Some("Trim leading/trailing whitespace"))
  }

  val all: Seq[TitleRule] = structural ++ canonical ++ perCinema ++ heliosRules ++ trimRules

  val ruleSet: TitleRuleSet = TitleRuleSet(all)
}
