package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.EnrichmentService

class EnrichmentServiceSpec extends AnyFlatSpec with Matchers {

  "normalize" should "lowercase the input" in {
    EnrichmentService.normalize("Drzewo Magii") shouldBe "drzewo magii"
  }

  it should "strip Polish diacritics so two spellings hit the same key" in {
    EnrichmentService.normalize("Łzy Morza")    shouldBe "lzy morza"
    EnrichmentService.normalize("Łzy Morza")    shouldBe EnrichmentService.normalize("lzy morza")
    EnrichmentService.normalize("Diabeł")       shouldBe "diabel"
    EnrichmentService.normalize("Sprawiedliwość owiec") shouldBe "sprawiedliwosc owiec"
  }

  it should "collapse runs of whitespace and trim" in {
    EnrichmentService.normalize("  Drzewo   Magii  ") shouldBe "drzewo magii"
  }

  it should "leave non-Latin scripts intact (Cyrillic, etc.)" in {
    // We don't try to transliterate — Cyrillic stays Cyrillic. (TMDB will fail
    // to match these, which is fine; they end up in the negative cache.)
    EnrichmentService.normalize("ДИЯВОЛ НОСИТЬ ПРАДА 2") shouldBe "диявол носить прада 2"
  }

  // ── getForMerge: variant-tolerant lookup ──────────────────────────────────
  //
  // Regression: cinemas report "Diabeł ubiera się u Prady 2" with an Arabic
  // numeral, the enrichment row is stored under that exact docId, but the
  // merged card's displayTitle goes through TitleNormalizer.normalize which
  // converts Arabic→Roman → "Diabeł ubiera się u Prady II". The plain
  // `get(displayTitle, year)` lookup misses; `getForMerge` falls back to the
  // raw cinema titles that fed the merge so the row gets found.

  import services.enrichment.{EnrichmentCache, EnrichmentRepo}
  import services.events.EventBus
  import clients.TmdbClient
  import models.Enrichment
  import scala.collection.mutable

  private class InMemRepo(seed: Seq[(String, Option[Int], Enrichment)] = Seq.empty) extends EnrichmentRepo {
    private val store = mutable.LinkedHashMap.empty[(String, Option[Int]), Enrichment]
    seed.foreach { case (t, y, e) => store.put((t, y), e) }
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], Enrichment)] =
      store.iterator.map { case ((t, y), e) => (t, y, e) }.toSeq
    override def upsert(t: String, y: Option[Int], e: Enrichment): Unit = { store.put((t, y), e); () }
    override def delete(t: String, y: Option[Int]): Unit = { store.remove((t, y)); () }
  }

  private def svc(seed: (String, Option[Int], Enrichment)*): EnrichmentService = {
    val cache = new EnrichmentCache(new InMemRepo(seed))
    new EnrichmentService(cache, new EventBus(), new TmdbClient(apiKey = None))
  }

  private val pradyEnrichment = Enrichment(
    imdbId        = Some("tt33612209"),
    imdbRating    = Some(6.7),
    metascore     = Some(62),
    originalTitle = Some("The Devil Wears Prada 2"),
    tmdbId        = Some(1314481)
  )

  "getForMerge" should "find a row stored under 'Prady 2' when the merged displayTitle is 'Prady II'" in {
    val s = svc(("Diabeł ubiera się u Prady 2", Some(2026), pradyEnrichment))
    s.getForMerge(
      displayTitle    = "Diabeł ubiera się u Prady II",
      candidateTitles = Seq("Diabeł ubiera się u Prady 2"),
      year            = Some(2026)
    ).flatMap(_.imdbId) shouldBe Some("tt33612209")
  }

  it should "prefer the displayTitle hit when it exists (don't shadow with a candidate)" in {
    val s = svc(
      ("Mortal Kombat II", Some(2026), pradyEnrichment.copy(imdbId = Some("tt-roman"))),
      ("Mortal Kombat 2",  Some(2026), pradyEnrichment.copy(imdbId = Some("tt-arabic")))
    )
    // displayTitle matches the Roman row directly — that wins; the Arabic
    // candidate is a fallback only.
    s.getForMerge(
      displayTitle    = "Mortal Kombat II",
      candidateTitles = Seq("Mortal Kombat 2"),
      year            = Some(2026)
    ).flatMap(_.imdbId) shouldBe Some("tt-roman")
  }

  it should "return None when neither displayTitle nor any candidate has a row" in {
    val s = svc()
    s.getForMerge("Anything", Seq("Anything else"), Some(2026)) shouldBe None
  }

  "searchTitle" should "strip a Kino Apollo Cykl prefix with straight quotes" in {
    EnrichmentService.searchTitle("""Cykl "Kultowa klasyka" - Zawieście czerwone latarnie""") shouldBe
      "Zawieście czerwone latarnie"
  }

  it should "strip a Cykl prefix with Polish curly quotes" in {
    EnrichmentService.searchTitle("""Cykl „Wajda: re-wizje" - Człowiek z marmuru / Man of Marble (1977)""") shouldBe
      "Człowiek z marmuru"
  }

  it should "strip a bilingual ' / English Title (year)' suffix" in {
    EnrichmentService.searchTitle("Bez znieczulenia / Rough Treatment (1978)") shouldBe "Bez znieczulenia"
  }

  it should "strip a ' + prelekcja…' event suffix" in {
    EnrichmentService.searchTitle("Znaki Pana Śliwki + prelekcja i spotkanie z Damianem Dudkiem") shouldBe
      "Znaki Pana Śliwki"
  }

  // Regression: the previous `\s+\+\s+.+$` pattern truncated mathematical
  // titles to before the first `+`, so "Orwell: 2 + 2 = 5" became "Orwell: 2"
  // and TMDB found a different film. Require a letter after the `+`.
  it should "leave 'Orwell: 2 + 2 = 5' intact (the + is part of the title, not an event suffix)" in {
    EnrichmentService.searchTitle("Orwell: 2 + 2 = 5") shouldBe "Orwell: 2 + 2 = 5"
  }

  it should "leave clean titles untouched" in {
    EnrichmentService.searchTitle("Drzewo Magii") shouldBe "Drzewo Magii"
    EnrichmentService.searchTitle("Mortal Kombat II") shouldBe "Mortal Kombat II"
  }

  it should "leave dashes inside the title alone (e.g. 're-wizje' inside the cycle name)" in {
    // The Cykl regex requires spaces around the dash separator, so a dash
    // inside the cycle name's quoted text doesn't trigger an early cut.
    EnrichmentService.searchTitle("""Cykl „Wajda: re-wizje" - Brzezina / The Birch Wood (1970)""") shouldBe
      "Brzezina"
  }

  // ── Anniversary / rerelease decoration ───────────────────────────────────
  //
  // Cinemas dress up rereleases with anniversary markers ("Top Gun 40th
  // Anniversary", "Kosmiczny mecz. 30. Rocznica"). TMDB only indexes them
  // under the original film, so we strip the decoration for the lookup key.

  it should "strip an English anniversary suffix" in {
    EnrichmentService.searchTitle("Top Gun 40th Anniversary") shouldBe "Top Gun"
  }

  it should "strip a Polish 'rocznica' suffix with a pipe separator" in {
    EnrichmentService.searchTitle("Top gun | 40 rocznica") shouldBe "Top gun"
  }

  it should "strip a Polish 'Rocznica' suffix with dot separators" in {
    EnrichmentService.searchTitle("Kosmiczny mecz. 30. Rocznica") shouldBe "Kosmiczny mecz"
  }

  it should "leave a standalone 'Rocznica' title untouched (it's a real Polish film)" in {
    EnrichmentService.searchTitle("Rocznica") shouldBe "Rocznica"
  }

  it should "leave 'Top Gun: Maverick' untouched (it's a sequel, not an anniversary)" in {
    EnrichmentService.searchTitle("Top Gun: Maverick") shouldBe "Top Gun: Maverick"
  }

  // ── Restoration / remaster decoration ─────────────────────────────────────

  it should "strip a Polish remaster suffix with a period separator" in {
    EnrichmentService.searchTitle("Żywot Briana Grupy Monty Pythona. Wersja zremasterowana") shouldBe
      "Żywot Briana Grupy Monty Pythona"
  }

  it should "strip a Polish 'wersja oryginalna' suffix with an en-dash separator" in {
    EnrichmentService.searchTitle("Moulin Rouge! – wersja oryginalna") shouldBe "Moulin Rouge!"
    EnrichmentService.searchTitle("Romeo i Julia – wersja oryginalna") shouldBe "Romeo i Julia"
  }

  it should "strip a hypothetical English '4K Restored' suffix" in {
    EnrichmentService.searchTitle("Drama 4K Restored")   shouldBe "Drama"
    EnrichmentService.searchTitle("Blade Runner 4K Remaster") shouldBe "Blade Runner"
  }

  it should "leave 'X-Men 2' / 'Mortal Kombat II' untouched (numeric sequels, not anniversaries)" in {
    EnrichmentService.searchTitle("X-Men 2")           shouldBe "X-Men 2"
    EnrichmentService.searchTitle("Mortal Kombat II")  shouldBe "Mortal Kombat II"
  }
}
