package services.movies

import models.{CinemaCityChain, CinemaCityKinepolis, MovieRecord, Multikino, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRuleSet}

/**
 * Drives the rebuilder against a cache seeded into a STALE state: a Cinema City
 * row stored under its un-stripped key ("Ladies Night - Anora") next to a plain
 * "Anora" row — the shape you'd get if the rows were scraped before the
 * cinema-city rule existed. Under the (default) rules both raw titles now map to
 * the same key, so a rebuild must collapse them into one row. No global rule
 * mutation: the default rule set already strips "Ladies Night - ".
 */
class NormalizationRebuilderSpec extends AnyFlatSpec with Matchers {

  private val disabledRepository = new MovieRepository {
    def enabled = false
    def findAll() = Seq.empty
    def delete(title: String, year: Option[Int]) = ()
    def deleteById(id: String) = ()
    def upsert(title: String, year: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private def ccSlot   = SourceData(title = Some("Ladies Night - Anora"), rawTitle = Some("Ladies Night - Anora"), releaseYear = Some(2024))
  private def mkSlot   = SourceData(title = Some("Anora"),                rawTitle = Some("Anora"),                releaseYear = Some(2024))

  private def staleCache(): CaffeineMovieCache = {
    val cache = new CaffeineMovieCache(disabledRepository)
    // Row A: scraped before the rule — stored under the un-stripped key.
    cache.put(cache.keyOf("Ladies Night - Anora", Some(2024)),
      MovieRecord(data = Map[Source, SourceData](CinemaCityKinepolis -> ccSlot)))
    // Row B: the plain title.
    cache.put(cache.keyOf("Anora", Some(2024)),
      MovieRecord(data = Map[Source, SourceData](Multikino -> mkSlot)))
    cache
  }

  "rebuild" should "collapse the stale decorated row onto the plain one (merge)" in {
    val cache = staleCache()
    cache.entries should have size 2

    val result = new NormalizationRebuilder(cache).rebuild()

    cache.entries should have size 1
    val merged = cache.entries.head._2
    merged.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino)
    result.merges should have size 1
    result.merges.head.mergedTitles should contain allOf ("Anora", "Ladies Night - Anora")
    result.splits shouldBe empty
  }

  it should "be a no-op on an already-consistent cache" in {
    val cache = staleCache()
    new NormalizationRebuilder(cache).rebuild()        // first run collapses
    val second = new NormalizationRebuilder(cache).rebuild()
    second.merges shouldBe empty
    second.splits shouldBe empty
    second.changed shouldBe 0
    cache.entries should have size 1
  }

  "rebuild" should "split a row whose cinema slots now map to different keys (un-merge)" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    // A single row that bundles two genuinely different films (the state left
    // behind after a too-broad rule that merged them is deleted), enriched.
    val bundled = MovieRecord(
      tmdbId = Some(111),
      data   = Map[Source, SourceData](
        CinemaCityKinepolis -> SourceData(title = Some("Anora"),      rawTitle = Some("Anora")),
        Multikino           -> SourceData(title = Some("Other Film"), rawTitle = Some("Other Film"))))
    cache.put(cache.keyOf("Anora", None), bundled)
    cache.entries should have size 1

    var splitOffs = List.empty[String]
    val result = new NormalizationRebuilder(cache, onSplitOff = (title, _) => splitOffs ::= title).rebuild()

    cache.entries should have size 2
    val byTitle = cache.entries.map { case (k, r) => k.cleanTitle -> r }.toMap
    byTitle.keySet shouldBe Set("Anora", "Other Film")
    byTitle("Anora").tmdbId shouldBe Some(111)   // remnant keeps the enrichment
    byTitle("Other Film").tmdbId shouldBe None    // split-off is fresh
    result.splits should have size 1
    result.splits.head.into should contain allOf ("Anora", "Other Film")
    splitOffs should contain ("Other Film")       // handed off for re-resolution
  }

  it should "not spawn a phantom empty-titled row from a slot whose rawTitle cleans to empty" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    // A healthy row whose two cinemas both screen "Dobry chłopiec", but one
    // slot's verbatim rawTitle is blank (e.g. a scraper that stored an empty
    // raw string, or a rule that strips it to nothing). The blank raw must NOT
    // re-key onto an empty merge key — the slot stays on the real row.
    val row = MovieRecord(
      tmdbId = Some(222),
      data   = Map[Source, SourceData](
        CinemaCityKinepolis -> SourceData(title = Some("Dobry chłopiec"), rawTitle = Some("Dobry chłopiec")),
        Multikino           -> SourceData(title = Some("Dobry chłopiec"), rawTitle = Some(""))))
    cache.put(cache.keyOf("Dobry chłopiec", None), row)
    cache.entries should have size 1

    var splitOffs = List.empty[String]
    val result = new NormalizationRebuilder(cache, onSplitOff = (title, _) => splitOffs ::= title).rebuild()

    // One row, no empty-titled phantom, both cinemas retained, no spurious split.
    cache.entries should have size 1
    cache.entries.map(_._1.cleanTitle).foreach(_.trim should not be empty)
    cache.entries.head._2.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino)
    result.merges shouldBe empty
    result.splits shouldBe empty
    splitOffs shouldBe empty
  }

  it should "prune a pre-existing empty-titled phantom row (all-blank slot)" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    // The junk left behind by the old bug: a row keyed by the empty string,
    // holding a slot with no usable title at all. The rebuild must drop it.
    cache.put(CacheKey("", Some(1999)),
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = None, rawTitle = Some("")))))
    cache.put(cache.keyOf("Real Film", None),
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Real Film"), rawTitle = Some("Real Film")))))
    cache.entries should have size 2

    new NormalizationRebuilder(cache).rebuild()

    cache.entries.map(_._1.cleanTitle) shouldBe Seq("Real Film")
  }

  it should "not split a row over a titleless detail-only slot (CinemaCityChain)" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    // The real shape behind the spurious "Michael ⟶ Michael" split: a healthy
    // row whose Cinema City detail is shared per-network into a synthetic
    // `CinemaCityChain` slot that carries enrichment but NO title/rawTitle.
    // That titleless slot must stay on the row, not split it off.
    val row = MovieRecord(
      tmdbId = Some(333),
      data   = Map[Source, SourceData](
        CinemaCityKinepolis -> SourceData(title = Some("Michael"), rawTitle = Some("Michael")),
        Multikino           -> SourceData(title = Some("Michael"), rawTitle = Some("Michael")),
        CinemaCityChain     -> SourceData(title = None,            rawTitle = None, synopsis = Some("A film."))))
    cache.put(cache.keyOf("Michael", Some(2026)), row)
    cache.entries should have size 1

    val result = new NormalizationRebuilder(cache).rebuild()

    cache.entries should have size 1
    cache.entries.head._2.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino, CinemaCityChain)
    result.splits shouldBe empty
    result.merges shouldBe empty
  }

  it should "not split a row whose slots carry mixed per-slot release years" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    // The real shape behind "Obsesja ⟶ Obsesja": one row, same title everywhere,
    // but the cinemas disagree on the release year (some 2025, some 2026, some
    // none). keyOfSlot must key every slot off the ROW's year, not the slot's,
    // or the row shatters into one phantom per distinct year.
    val row = MovieRecord(
      tmdbId = Some(444),
      data   = Map[Source, SourceData](
        CinemaCityKinepolis -> SourceData(title = Some("Obsesja"), rawTitle = Some("Obsesja"), releaseYear = Some(2025)),
        Multikino           -> SourceData(title = Some("Obsesja"), rawTitle = Some("Obsesja"), releaseYear = Some(2026)),
        CinemaCityChain     -> SourceData(title = Some("Obsesja"), rawTitle = Some("Obsesja"), releaseYear = None)))
    cache.put(cache.keyOf("Obsesja", Some(2025)), row)
    cache.entries should have size 1

    val result = new NormalizationRebuilder(cache).rebuild()

    cache.entries should have size 1
    result.splits shouldBe empty
  }

  it should "not split a row when only rawTitle keeps a client-stripped decoration (DUB/2D DUBBING)" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    // The Władcy Wszechświata shape: the cinema CLIENT strips a format/language
    // tag ("DUB", "2D DUBBING") into a clean `title`, but `rawTitle` keeps it.
    // keyOfSlot must key off the cleaned title (as a fresh scrape does), not the
    // raw — else the row fragments into one phantom per decoration every backfill.
    val row = MovieRecord(
      tmdbId = Some(555),
      data   = Map[Source, SourceData](
        Multikino           -> SourceData(title = Some("Władcy wszechświata"), rawTitle = Some("Władcy wszechświata")),
        CinemaCityKinepolis -> SourceData(title = Some("Władcy wszechświata"), rawTitle = Some("WŁADCY WSZECHŚWIATA 2D DUBBING")),
        CinemaCityChain     -> SourceData(title = Some("Władcy wszechświata"), rawTitle = Some("WŁADCY WSZECHŚWIATA DUB"))))
    cache.put(cache.keyOf("Władcy wszechświata", Some(2026)), row)
    cache.entries should have size 1

    val result = new NormalizationRebuilder(cache).rebuild()

    cache.entries should have size 1
    cache.entries.head._2.cinemaData.keySet shouldBe Set(Multikino, CinemaCityKinepolis, CinemaCityChain)
    result.splits shouldBe empty
    result.merges shouldBe empty
  }

  "reEnrichSearchChanges" should "re-resolve only rows whose apiQuery changed" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    // Row keyed by its display title incl. the programme prefix (search-tier
    // strips it; structural/key keeps it).
    cache.put(cache.keyOf("Kino bez barier: Freak Show", None),
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Kino bez barier: Freak Show")))))
    cache.put(cache.keyOf("Plain Film", None),
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Plain Film")))))

    // Old set strips the programme prefix; new set drops that rule.
    val progRule = TitleRule("search-prog", RuleScope.GlobalStructural, None,
      "(?i)^Kino bez barier: ", "", applyAll = false, order = 10)
    val oldRules = TitleRuleSet(Seq(progRule))
    val newRules = TitleRuleSet.empty

    var reEnriched = List.empty[String]
    val n = new NormalizationRebuilder(cache)
      .reEnrichSearchChanges(oldRules, newRules, (t, _) => reEnriched ::= t)

    n shouldBe 1                                   // only the programme-prefixed row
    reEnriched should contain ("Kino bez barier: Freak Show")
    reEnriched should not contain ("Plain Film")
  }

  it should "re-resolve when toggling a rule's `last` flag reorders the search tier" in {
    val cache = new CaffeineMovieCache(disabledRepository)
    val aabKey = cache.keyOf("aab", None)
    cache.put(aabKey, MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("aab")))))
    val zzzKey = cache.keyOf("zzz", None)
    cache.put(zzzKey, MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("zzz")))))

    // Two order-dependent search rules: strip "ab", strip "a". On "aab",
    // [stripAb, stripA] → "" but [stripA, stripAb] → "b".
    val stripAb = TitleRule("strip-ab", RuleScope.GlobalStructural, None, "ab", "", applyAll = true, order = 0)
    val stripA  = TitleRule("strip-a",  RuleScope.GlobalStructural, None, "a",  "", applyAll = true, order = 1)
    val before  = TitleRuleSet(Seq(stripAb, stripA))                  // tier order [stripAb, stripA]
    val after   = TitleRuleSet(Seq(stripAb.copy(last = true), stripA)) // `last` sinks stripAb → [stripA, stripAb]

    // The reorder genuinely changes the query for the "aab" row, not the "zzz" one.
    before.search(aabKey.cleanTitle) should not be after.search(aabKey.cleanTitle)
    before.search(zzzKey.cleanTitle) shouldBe after.search(zzzKey.cleanTitle)

    var reEnriched = List.empty[String]
    val n = new NormalizationRebuilder(cache)
      .reEnrichSearchChanges(before, after, (t, _) => reEnriched ::= t)

    n shouldBe 1
    reEnriched should contain (aabKey.cleanTitle)
    reEnriched should not contain (zzzKey.cleanTitle)
  }

  /** Records every metric increment so a rebuild's merge/split counts can be
   *  asserted — the same victims/fragments convention the runtime folds use. */
  private class RecordingMetrics extends MergeMetrics with SplitMetrics {
    var merges = List.empty[(MergeReason, Int)]
    var splits = List.empty[Int]
    def recordMerge(reason: MergeReason, victims: Int): Unit = merges ::= (reason -> victims)
    def recordSplit(fragments: Int): Unit                    = splits ::= fragments
  }

  "rebuild" should "record a normalize-rebuild merge of one victim when two rows fold together" in {
    val metrics = new RecordingMetrics
    val cache   = staleCache()  // two rows that now key the same

    new NormalizationRebuilder(cache, mergeMetrics = metrics, splitMetrics = metrics).rebuild()

    metrics.merges shouldBe List(MergeReason.NormalizeRebuild -> 1)
    metrics.splits.filter(_ > 0) shouldBe empty
  }

  it should "record a split of one fragment when a bundled row un-merges into two" in {
    val metrics = new RecordingMetrics
    val cache   = new CaffeineMovieCache(disabledRepository)
    cache.put(cache.keyOf("Anora", None), MovieRecord(
      tmdbId = Some(111),
      data   = Map[Source, SourceData](
        CinemaCityKinepolis -> SourceData(title = Some("Anora"),      rawTitle = Some("Anora")),
        Multikino           -> SourceData(title = Some("Other Film"), rawTitle = Some("Other Film")))))

    new NormalizationRebuilder(cache, mergeMetrics = metrics, splitMetrics = metrics).rebuild()

    metrics.splits shouldBe List(1)               // 1→2 split = one new row
    metrics.merges.filter(_._2 > 0) shouldBe empty
  }
}
