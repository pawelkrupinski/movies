package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieService
import tools.RealHttpFetch
import services.movies.SingleCountryNormalizer.titleNormalizer

class MovieServiceSpec extends AnyFlatSpec with Matchers {

  // `normalize` is the stable documentId rule — it delegates to
  // `TitleNormalizer.sanitize`, which applies Arabic→Roman, strips display
  // decoration (anniversary, Cykl, wersja), folds " & " → " i " and the
  // "Gwiezdne Wojny:" prefix, then collapses every non-alphanumeric char.
  // Output is lowercased, diacritic-stripped, Polish ł → l, and contains no
  // whitespace or punctuation.

  "normalize" should "lowercase the input and remove whitespace/punctuation" in {
    titleNormalizer.sanitize("Drzewo Magii") shouldBe "drzewomagii"
  }

  it should "strip Polish diacritics so two spellings hit the same key" in {
    titleNormalizer.sanitize("Łzy Morza")    shouldBe "lzymorza"
    titleNormalizer.sanitize("Łzy Morza")    shouldBe titleNormalizer.sanitize("lzy morza")
    titleNormalizer.sanitize("Diabeł")       shouldBe "diabel"
    titleNormalizer.sanitize("Sprawiedliwość owiec") shouldBe "sprawiedliwoscowiec"
  }

  it should "collapse runs of whitespace and trim" in {
    titleNormalizer.sanitize("  Drzewo   Magii  ") shouldBe "drzewomagii"
  }

  it should "preserve Cyrillic letters but drop the Cyrillic-side whitespace, keying the numeral in Arabic" in {
    // Non-Latin scripts keep their letters (so the row's documentId isn't empty);
    // the Arabic '2' is kept as Arabic (Roman folds onto it, not the reverse).
    titleNormalizer.sanitize("ДИЯВОЛ НОСИТЬ ПРАДА 2") shouldBe "дияволноситьпрада2"
  }

  it should "fold colon/space punctuation differences to the same key" in {
    // This is what gives "Prady 2" and "Prady II" the same documentId: the Roman
    // form folds onto the Arabic one, so the key matches the spelling cinemas use.
    titleNormalizer.sanitize("Top Gun Maverick")  shouldBe "topgunmaverick"
    titleNormalizer.sanitize("Top Gun: Maverick") shouldBe "topgunmaverick"
    titleNormalizer.sanitize("Mortal Kombat 2")   shouldBe "mortalkombat2"
    titleNormalizer.sanitize("Mortal Kombat II")  shouldBe "mortalkombat2"
  }

  it should "fold the '& vs i' / 'Gwiezdne Wojny:' display-merge rules into the key" in {
    val k1 = titleNormalizer.sanitize("Mandalorian & Grogu")
    val k2 = titleNormalizer.sanitize("Mandalorian i Grogu")
    val k3 = titleNormalizer.sanitize("Gwiezdne Wojny: Mandalorian i Grogu")
    k1 shouldBe k2
    k2 shouldBe k3
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(seed, normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    new MovieService(cache, new InProcessEventBus(), new TmdbClient(new RealHttpFetch, apiKey = None), clock = _root_.tools.SpecClock.Pinned)
  }

  // The projection fetches a film's TMDB details once — while its record lacks them. A 5xx on the
  // film's `external_ids` used to be read as "no IMDb id": the details landed without one, so the
  // record never lacked them again, and the film stayed unrated after any TMDB blip.
  "withFilmDetails" should "apply nothing while TMDB fails to answer the film's ids, so the next projection asks again" in {
    val failing = new java.util.concurrent.atomic.AtomicBoolean(true)
    val fixture = new clients.tools.FakeHttpFetch("08-06-2026")
    val fetch = new tools.HttpFetch {
      private def check(url: String): Unit =
        if (failing.get && url.contains("/external_ids")) throw new tools.HttpStatusException(503, "GET", url, retryAfter = None)
      override def get(url: String): String = { check(url); fixture.get(url) }
      override def get(url: String, headers: Map[String, String]): String = { check(url); fixture.get(url, headers) }
      override def post(url: String, body: String, contentType: String): String = fixture.post(url, body, contentType)
    }
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val svc = new MovieService(cache, new InProcessEventBus(),
      new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("test-key")), retrySleep = _ => ()), clock = _root_.tools.SpecClock.Pinned)
    svc.withFilmDetails(MovieRecord(), 1018) shouldBe None
    failing.set(false)
    svc.withFilmDetails(MovieRecord(), 1018).flatMap(_.imdbId) shouldBe Some("tt0166924")
  }

  // TMDB keeps a runtime per translation: "Once Upon a Time in America" is 229 minutes in pl-PL — the cut Polish cinemas
  // screen — and 139 (the US theatrical cut) in en-US. A Polish deployment's TMDB slot shows its own market's.
  it should "carry the runtime of the deployment language's translation, not English's" in {
    def recorded(name: String) = scala.io.Source.fromResource(s"fixtures/tmdb/$name")(using scala.io.Codec.UTF8).mkString
    val fetch = tools.RoutingHttpFetch.getOnly(Map(
      "/movie/311/external_ids"     -> """{"imdb_id":"tt0087843"}""",
      "/movie/311/images"           -> """{"posters":[]}""",
      "/movie/311?language=pl-PL"   -> recorded("movie_311_pl.json"),
      "/movie/311?language=en-US"   -> recorded("movie_311_en.json")))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val svc = new MovieService(cache, new InProcessEventBus(),
      new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("test-key")), retrySleep = _ => ()), clock = _root_.tools.SpecClock.Pinned)
    svc.withFilmDetails(MovieRecord(), 311).flatMap(_.runtimeMinutes) shouldBe Some(229)
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
    titleNormalizer.apiQuery("""Cykl "Kultowa klasyka" - Zawieście czerwone latarnie""") shouldBe
      "Zawieście czerwone latarnie"
  }

  it should "strip a Cykl prefix with Polish curly quotes" in {
    titleNormalizer.apiQuery("""Cykl „Wajda: re-wizje" - Człowiek z marmuru / Man of Marble (1977)""") shouldBe
      "Człowiek z marmuru"
  }

  it should "strip a bilingual ' / English Title (year)' suffix" in {
    titleNormalizer.apiQuery("Bez znieczulenia / Rough Treatment (1978)") shouldBe "Bez znieczulenia"
  }

  it should "strip a 'z autorską narracją <person>' narration-event suffix (en-dash or bare)" in {
    // A live-narrated special screening: the suffix must not reach TMDB, or the row
    // resolves order-dependently (only when a sibling cinema's bare-title hint had
    // already merged in) — the Klątwa doliny węży split.
    titleNormalizer.apiQuery("Klątwa doliny węży – z autorską narracją Łony") shouldBe "Klątwa doliny węży"
    titleNormalizer.apiQuery("\"Klątwa doliny węży\" z autorską narracją Łony") shouldBe "\"Klątwa doliny węży\""
  }

  it should "strip a 'z prelekcją' lecture-event suffix" in {
    titleNormalizer.apiQuery("Człowiek z marmuru z prelekcją filmoznawcy") shouldBe "Człowiek z marmuru"
  }

  it should "strip a ' + prelekcja…' event suffix for upstream lookups" in {
    // The "+ <event>" suffix marks a screening with an associated event (a
    // lecture + meeting). The display row keeps it (sanitize keeps the suffix)
    // so it stays its own card, but the external-API query drops it so both
    // rows enrich off the same base title. See TitleNormalizerSpec for the rationale.
    titleNormalizer.apiQuery("Znaki Pana Śliwki + prelekcja i spotkanie z Damianem Dudkiem") shouldBe
      "Znaki Pana Śliwki"
  }

  // Regression: the previous `\s+\+\s+.+$` pattern truncated mathematical
  // titles to before the first `+`, so "Orwell: 2 + 2 = 5" became "Orwell: 2"
  // and TMDB found a different film. Require a letter after the `+`.
  it should "leave 'Orwell: 2 + 2 = 5' intact (the + is part of the title, not an event suffix)" in {
    titleNormalizer.apiQuery("Orwell: 2 + 2 = 5") shouldBe "Orwell: 2 + 2 = 5"
  }

  it should "leave clean titles untouched" in {
    titleNormalizer.apiQuery("Drzewo Magii") shouldBe "Drzewo Magii"
    titleNormalizer.apiQuery("Mortal Kombat II") shouldBe "Mortal Kombat II"
  }

  it should "leave dashes inside the title alone (e.g. 're-wizje' inside the cycle name)" in {
    // The Cykl regex requires spaces around the dash separator, so a dash
    // inside the cycle name's quoted text doesn't trigger an early cut.
    titleNormalizer.apiQuery("""Cykl „Wajda: re-wizje" - Brzezina / The Birch Wood (1970)""") shouldBe
      "Brzezina"
  }

  // ── Anniversary / rerelease decoration ───────────────────────────────────
  //
  // Cinemas dress up rereleases with anniversary markers ("Top Gun 40th
  // Anniversary", "Kosmiczny mecz. 30. Rocznica"). TMDB only indexes them
  // under the original film, so we strip the decoration for the lookup key.

  it should "strip an English anniversary suffix" in {
    titleNormalizer.apiQuery("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  it should "strip a Polish 'rocznica' suffix with a pipe separator" in {
    titleNormalizer.apiQuery("Top gun | 40 rocznica") shouldBe "Top gun"
  }

  it should "strip a Polish 'Rocznica' suffix with dot separators" in {
    titleNormalizer.apiQuery("Kosmiczny mecz. 30. Rocznica") shouldBe "Kosmiczny mecz"
  }

  it should "leave a standalone 'Rocznica' title untouched (it's a real Polish film)" in {
    titleNormalizer.apiQuery("Rocznica") shouldBe "Rocznica"
  }

  it should "leave 'Top Gun: Maverick' untouched (it's a sequel, not an anniversary)" in {
    titleNormalizer.apiQuery("Top Gun: Maverick") shouldBe "Top Gun: Maverick"
  }

  // ── Restoration / remaster decoration ─────────────────────────────────────

  it should "strip a Polish remaster suffix with a period separator" in {
    titleNormalizer.apiQuery("Rejs. Wersja zremasterowana") shouldBe "Rejs"
    // For the real Multikino 'Żywot Briana' rows the studio-attribution rule
    // (xtra-zywot-briana-monty-suffix) folds the query further to the bare film
    // (TMDB 583, unique), so apiQuery resolves the whole decorated title at once.
    titleNormalizer.apiQuery("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana") shouldBe
      "Żywot Briana"
  }

  it should "strip a Polish 'wersja oryginalna' suffix with an en-dash separator" in {
    titleNormalizer.apiQuery("Moulin Rouge! – wersja oryginalna") shouldBe "Moulin Rouge!"
    titleNormalizer.apiQuery("Romeo i Julia – wersja oryginalna") shouldBe "Romeo i Julia"
  }

  it should "strip a hypothetical English '4K Restored' suffix" in {
    titleNormalizer.apiQuery("Drama 4K Restored")   shouldBe "Drama"
    titleNormalizer.apiQuery("Blade Runner 4K Remaster") shouldBe "Blade Runner"
  }

  it should "leave 'X-Men 2' / 'Mortal Kombat II' untouched (numeric sequels, not anniversaries)" in {
    titleNormalizer.apiQuery("X-Men 2")           shouldBe "X-Men 2"
    titleNormalizer.apiQuery("Mortal Kombat II")  shouldBe "Mortal Kombat II"
  }
}
