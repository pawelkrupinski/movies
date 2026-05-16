package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieService

class MovieServiceSpec extends AnyFlatSpec with Matchers {

  // `normalize` is the stable docId rule — it delegates to
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
    // Non-Latin scripts keep their letters (so the row's docId isn't empty);
    // the Arabic '2' is folded to Roman via TitleNormalizer.normalize before
    // the strip.
    MovieService.normalize("ДИЯВОЛ НОСИТЬ ПРАДА 2") shouldBe "дияволноситьпрадаii"
  }

  it should "fold colon/space punctuation differences to the same key" in {
    // Phase 2 of the MovieCache transition: this is what gives "Prady 2" and
    // "Prady II" the same docId, fixing the bug where the display title's
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

  // ── Cross-variant lookups via the stable docId ──────────────────────────
  //
  // Phase 2.3 made the docId corpus-independent and aggressive enough that
  // every variant of a film resolves to the same key. The variant-tolerant
  // `getForMerge` fallback that existed in phase 1 is no longer necessary —
  // a plain `get` with any variant finds the row.

  import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}
  import services.events.EventBus
  import clients.TmdbClient
  import models.MovieRecord

  private def svc(seed: (String, Option[Int], MovieRecord)*): MovieService = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo(seed))
    new MovieService(cache, new EventBus(), new TmdbClient(apiKey = None))
  }

  private val pradyEnrichment = MovieRecord(
    imdbId        = Some("tt33612209"),
    imdbRating    = Some(6.7),
    metascore     = Some(62),
    originalTitle = Some("The Devil Wears Prada 2"),
    tmdbId        = Some(1314481)
  )

  // Regression: cinemas report "Diabeł ubiera się u Prady 2" with an Arabic
  // numeral; merged display title goes through Arabic→Roman → "Prady II".
  // Under the new docId rule both produce the same key, so `get` works
  // regardless of which form is asked for.
  "get" should "find a row regardless of Arabic vs Roman variant of the title" in {
    val s = svc(("Diabeł ubiera się u Prady 2", Some(2026), pradyEnrichment))
    s.get("Diabeł ubiera się u Prady II", Some(2026)).flatMap(_.imdbId) shouldBe Some("tt33612209")
    s.get("Diabeł ubiera się u Prady 2",  Some(2026)).flatMap(_.imdbId) shouldBe Some("tt33612209")
  }

  it should "find a row regardless of colon-or-not punctuation" in {
    val s = svc(("Top Gun Maverick", Some(2022), pradyEnrichment.copy(imdbId = Some("tt1745960"))))
    s.get("Top Gun: Maverick", Some(2022)).flatMap(_.imdbId) shouldBe Some("tt1745960")
    s.get("Top Gun Maverick",  Some(2022)).flatMap(_.imdbId) shouldBe Some("tt1745960")
  }

  "searchTitle" should "strip a Kino Apollo Cykl prefix with straight quotes" in {
    MovieService.searchTitle("""Cykl "Kultowa klasyka" - Zawieście czerwone latarnie""") shouldBe
      "Zawieście czerwone latarnie"
  }

  it should "strip a Cykl prefix with Polish curly quotes" in {
    MovieService.searchTitle("""Cykl „Wajda: re-wizje" - Człowiek z marmuru / Man of Marble (1977)""") shouldBe
      "Człowiek z marmuru"
  }

  it should "strip a bilingual ' / English Title (year)' suffix" in {
    MovieService.searchTitle("Bez znieczulenia / Rough Treatment (1978)") shouldBe "Bez znieczulenia"
  }

  it should "strip a ' + prelekcja…' event suffix" in {
    MovieService.searchTitle("Znaki Pana Śliwki + prelekcja i spotkanie z Damianem Dudkiem") shouldBe
      "Znaki Pana Śliwki"
  }

  // Regression: the previous `\s+\+\s+.+$` pattern truncated mathematical
  // titles to before the first `+`, so "Orwell: 2 + 2 = 5" became "Orwell: 2"
  // and TMDB found a different film. Require a letter after the `+`.
  it should "leave 'Orwell: 2 + 2 = 5' intact (the + is part of the title, not an event suffix)" in {
    MovieService.searchTitle("Orwell: 2 + 2 = 5") shouldBe "Orwell: 2 + 2 = 5"
  }

  it should "leave clean titles untouched" in {
    MovieService.searchTitle("Drzewo Magii") shouldBe "Drzewo Magii"
    MovieService.searchTitle("Mortal Kombat II") shouldBe "Mortal Kombat II"
  }

  it should "leave dashes inside the title alone (e.g. 're-wizje' inside the cycle name)" in {
    // The Cykl regex requires spaces around the dash separator, so a dash
    // inside the cycle name's quoted text doesn't trigger an early cut.
    MovieService.searchTitle("""Cykl „Wajda: re-wizje" - Brzezina / The Birch Wood (1970)""") shouldBe
      "Brzezina"
  }

  // ── Anniversary / rerelease decoration ───────────────────────────────────
  //
  // Cinemas dress up rereleases with anniversary markers ("Top Gun 40th
  // Anniversary", "Kosmiczny mecz. 30. Rocznica"). TMDB only indexes them
  // under the original film, so we strip the decoration for the lookup key.

  it should "strip an English anniversary suffix" in {
    MovieService.searchTitle("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  it should "strip a Polish 'rocznica' suffix with a pipe separator" in {
    MovieService.searchTitle("Top gun | 40 rocznica") shouldBe "Top gun"
  }

  it should "strip a Polish 'Rocznica' suffix with dot separators" in {
    MovieService.searchTitle("Kosmiczny mecz. 30. Rocznica") shouldBe "Kosmiczny mecz"
  }

  it should "leave a standalone 'Rocznica' title untouched (it's a real Polish film)" in {
    MovieService.searchTitle("Rocznica") shouldBe "Rocznica"
  }

  it should "leave 'Top Gun: Maverick' untouched (it's a sequel, not an anniversary)" in {
    MovieService.searchTitle("Top Gun: Maverick") shouldBe "Top Gun: Maverick"
  }

  // ── Restoration / remaster decoration ─────────────────────────────────────

  it should "strip a Polish remaster suffix with a period separator" in {
    MovieService.searchTitle("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana") shouldBe
      "Żywot Briana Grupy Monty Pythona"
  }

  it should "strip a Polish 'wersja oryginalna' suffix with an en-dash separator" in {
    MovieService.searchTitle("Moulin Rouge! – wersja oryginalna") shouldBe "Moulin Rouge!"
    MovieService.searchTitle("Romeo i Julia – wersja oryginalna") shouldBe "Romeo i Julia"
  }

  it should "strip a hypothetical English '4K Restored' suffix" in {
    MovieService.searchTitle("Drama 4K Restored")   shouldBe "Drama"
    MovieService.searchTitle("Blade Runner 4K Remaster") shouldBe "Blade Runner"
  }

  it should "leave 'X-Men 2' / 'Mortal Kombat II' untouched (numeric sequels, not anniversaries)" in {
    MovieService.searchTitle("X-Men 2")           shouldBe "X-Men 2"
    MovieService.searchTitle("Mortal Kombat II")  shouldBe "Mortal Kombat II"
  }
}
