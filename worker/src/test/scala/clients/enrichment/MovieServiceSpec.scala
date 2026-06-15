package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieService
import tools.RealHttpFetch

class MovieServiceSpec extends AnyFlatSpec with Matchers {

  // `normalize` is the stable documentId rule — it delegates to
  // `TitleNormalizer.sanitize`, which applies Arabic→Roman, strips display
  // decoration (anniversary, Cykl, wersja), folds " & " → " i " and the
  // "Gwiezdne Wojny:" prefix, then collapses every non-alphanumeric char.
  // Output is lowercased, diacritic-stripped, Polish ł → l, and contains no
  // whitespace or punctuation.

  "normalize" should "lowercase the input and remove whitespace/punctuation" in {
    MovieService.normalize("Drzewo Magii") shouldBe "drzewomagii"
  }

  it should "strip Polish diacritics so two spellings hit the same key" in {
    MovieService.normalize("Łzy Morza")    shouldBe "lzymorza"
    MovieService.normalize("Łzy Morza")    shouldBe MovieService.normalize("lzy morza")
    MovieService.normalize("Diabeł")       shouldBe "diabel"
    MovieService.normalize("Sprawiedliwość owiec") shouldBe "sprawiedliwoscowiec"
  }

  it should "collapse runs of whitespace and trim" in {
    MovieService.normalize("  Drzewo   Magii  ") shouldBe "drzewomagii"
  }

  it should "preserve Cyrillic letters but drop the Cyrillic-side whitespace + Arabic→Roman fold" in {
    // Non-Latin scripts keep their letters (so the row's documentId isn't empty);
    // the Arabic '2' is folded to Roman via TitleNormalizer.normalize before
    // the strip.
    MovieService.normalize("ДИЯВОЛ НОСИТЬ ПРАДА 2") shouldBe "дияволноситьпрадаii"
  }

  it should "fold colon/space punctuation differences to the same key" in {
    // Phase 2 of the MovieCache transition: this is what gives "Prady 2" and
    // "Prady II" the same documentId, fixing the bug where the display title's
    // Arabic→Roman folding produced a key that didn't match Mongo storage.
    MovieService.normalize("Top Gun Maverick")  shouldBe "topgunmaverick"
    MovieService.normalize("Top Gun: Maverick") shouldBe "topgunmaverick"
    MovieService.normalize("Mortal Kombat 2")   shouldBe "mortalkombatii"
    MovieService.normalize("Mortal Kombat II")  shouldBe "mortalkombatii"
  }

  it should "fold the '& vs i' / 'Gwiezdne Wojny:' display-merge rules into the key" in {
    val k1 = MovieService.normalize("Mandalorian & Grogu")
    val k2 = MovieService.normalize("Mandalorian i Grogu")
    val k3 = MovieService.normalize("Gwiezdne Wojny: Mandalorian i Grogu")
    k1 shouldBe k2
    k2 shouldBe k3
  }

  // ── verifyByDirector name matching ───────────────────────────────────────────
  // A cinema reporting a Chinese/Hungarian/Korean director given-name-first must
  // still verify against TMDB's native family-first credit. The old collapsed
  // substring test ("yimouzhang".contains("zhangyimou")) rejected it, stranding
  // the row as a no-match — the order-dependent root cause of the "Zawieście
  // czerwone latarnie" (Zhang Yimou) snapshot fragmentation.
  "directorNameMatches" should "match the same name in swapped token order (Yimou Zhang ↔ Zhang Yimou)" in {
    assert(MovieService.directorNameMatches("Yimou Zhang", "Zhang Yimou"))
    assert(MovieService.directorNameMatches("Zhang Yimou", "Yimou Zhang"))
  }

  it should "tolerate diacritics and a comma-free single surname (partial name)" in {
    assert(MovieService.directorNameMatches("Helgestad", "Asgeir Helgestad"))
    assert(MovieService.directorNameMatches("Pedro Almodovar", "Pedro Almodóvar"))
  }

  it should "reject two genuinely different directors" in {
    assert(!MovieService.directorNameMatches("Christopher Nolan", "Steven Spielberg"))
    // Shares one token but not the other — not the same person.
    assert(!MovieService.directorNameMatches("John Smith", "John Doe"))
  }

  // ── Cross-variant lookups via the stable documentId ──────────────────────────
  //
  // Phase 2.3 made the documentId corpus-independent and aggressive enough that
  // every variant of a film resolves to the same key. The variant-tolerant
  // `getForMerge` fallback that existed in phase 1 is no longer necessary —
  // a plain `get` with any variant finds the row.

  import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
  import services.events.InProcessEventBus
  import clients.TmdbClient
  import models.{MovieRecord, Source, SourceData, Tmdb}

  private def service(seed: (String, Option[Int], MovieRecord)*): MovieService = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(seed))
    new MovieService(cache, new InProcessEventBus(), new TmdbClient(new RealHttpFetch, apiKey = None))
  }

  private val pradyEnrichment = MovieRecord(
    imdbId        = Some("tt33612209"),
    imdbRating    = Some(6.7),
    metascore     = Some(62),
    tmdbId        = Some(1314481),
    data          = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("The Devil Wears Prada 2")))
  )

  // Regression: cinemas report "Diabeł ubiera się u Prady 2" with an Arabic
  // numeral; merged display title goes through Arabic→Roman → "Prady II".
  // Under the new documentId rule both produce the same key, so `get` works
  // regardless of which form is asked for.
  "get" should "find a row regardless of Arabic vs Roman variant of the title" in {
    val s = service(("Diabeł ubiera się u Prady 2", Some(2026), pradyEnrichment))
    s.get("Diabeł ubiera się u Prady II", Some(2026)).flatMap(_.imdbId) shouldBe Some("tt33612209")
    s.get("Diabeł ubiera się u Prady 2",  Some(2026)).flatMap(_.imdbId) shouldBe Some("tt33612209")
  }

  it should "find a row regardless of colon-or-not punctuation" in {
    val s = service(("Top Gun Maverick", Some(2022), pradyEnrichment.copy(imdbId = Some("tt1745960"))))
    s.get("Top Gun: Maverick", Some(2022)).flatMap(_.imdbId) shouldBe Some("tt1745960")
    s.get("Top Gun Maverick",  Some(2022)).flatMap(_.imdbId) shouldBe Some("tt1745960")
  }

  "apiQuery" should "strip a Kino Apollo Cykl prefix with straight quotes" in {
    MovieService.apiQuery("""Cykl "Kultowa klasyka" - Zawieście czerwone latarnie""") shouldBe
      "Zawieście czerwone latarnie"
  }

  it should "strip a Cykl prefix with Polish curly quotes" in {
    MovieService.apiQuery("""Cykl „Wajda: re-wizje" - Człowiek z marmuru / Man of Marble (1977)""") shouldBe
      "Człowiek z marmuru"
  }

  it should "strip a bilingual ' / English Title (year)' suffix" in {
    MovieService.apiQuery("Bez znieczulenia / Rough Treatment (1978)") shouldBe "Bez znieczulenia"
  }

  it should "strip a ' + prelekcja…' event suffix for upstream lookups" in {
    // The "+ <event>" suffix marks a screening with an associated event (a
    // lecture + meeting). The display row keeps it (sanitize keeps the suffix)
    // so it stays its own card, but the external-API query drops it so both
    // rows enrich off the same base title. See TitleNormalizerSpec for the rationale.
    MovieService.apiQuery("Znaki Pana Śliwki + prelekcja i spotkanie z Damianem Dudkiem") shouldBe
      "Znaki Pana Śliwki"
  }

  // Regression: the previous `\s+\+\s+.+$` pattern truncated mathematical
  // titles to before the first `+`, so "Orwell: 2 + 2 = 5" became "Orwell: 2"
  // and TMDB found a different film. Require a letter after the `+`.
  it should "leave 'Orwell: 2 + 2 = 5' intact (the + is part of the title, not an event suffix)" in {
    MovieService.apiQuery("Orwell: 2 + 2 = 5") shouldBe "Orwell: 2 + 2 = 5"
  }

  it should "leave clean titles untouched" in {
    MovieService.apiQuery("Drzewo Magii") shouldBe "Drzewo Magii"
    MovieService.apiQuery("Mortal Kombat II") shouldBe "Mortal Kombat II"
  }

  it should "leave dashes inside the title alone (e.g. 're-wizje' inside the cycle name)" in {
    // The Cykl regex requires spaces around the dash separator, so a dash
    // inside the cycle name's quoted text doesn't trigger an early cut.
    MovieService.apiQuery("""Cykl „Wajda: re-wizje" - Brzezina / The Birch Wood (1970)""") shouldBe
      "Brzezina"
  }

  // ── Anniversary / rerelease decoration ───────────────────────────────────
  //
  // Cinemas dress up rereleases with anniversary markers ("Top Gun 40th
  // Anniversary", "Kosmiczny mecz. 30. Rocznica"). TMDB only indexes them
  // under the original film, so we strip the decoration for the lookup key.

  it should "strip an English anniversary suffix" in {
    MovieService.apiQuery("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  it should "strip a Polish 'rocznica' suffix with a pipe separator" in {
    MovieService.apiQuery("Top gun | 40 rocznica") shouldBe "Top gun"
  }

  it should "strip a Polish 'Rocznica' suffix with dot separators" in {
    MovieService.apiQuery("Kosmiczny mecz. 30. Rocznica") shouldBe "Kosmiczny mecz"
  }

  it should "leave a standalone 'Rocznica' title untouched (it's a real Polish film)" in {
    MovieService.apiQuery("Rocznica") shouldBe "Rocznica"
  }

  it should "leave 'Top Gun: Maverick' untouched (it's a sequel, not an anniversary)" in {
    MovieService.apiQuery("Top Gun: Maverick") shouldBe "Top Gun: Maverick"
  }

  // ── Restoration / remaster decoration ─────────────────────────────────────

  it should "strip a Polish remaster suffix with a period separator" in {
    MovieService.apiQuery("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana") shouldBe
      "Żywot Briana Grupy Monty Pythona"
  }

  it should "strip a Polish 'wersja oryginalna' suffix with an en-dash separator" in {
    MovieService.apiQuery("Moulin Rouge! – wersja oryginalna") shouldBe "Moulin Rouge!"
    MovieService.apiQuery("Romeo i Julia – wersja oryginalna") shouldBe "Romeo i Julia"
  }

  it should "strip a hypothetical English '4K Restored' suffix" in {
    MovieService.apiQuery("Drama 4K Restored")   shouldBe "Drama"
    MovieService.apiQuery("Blade Runner 4K Remaster") shouldBe "Blade Runner"
  }

  it should "leave 'X-Men 2' / 'Mortal Kombat II' untouched (numeric sequels, not anniversaries)" in {
    MovieService.apiQuery("X-Men 2")           shouldBe "X-Men 2"
    MovieService.apiQuery("Mortal Kombat II")  shouldBe "Mortal Kombat II"
  }
}
