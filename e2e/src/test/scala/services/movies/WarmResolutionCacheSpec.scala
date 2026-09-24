package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.resolution.{InMemoryResolutionStore, ResolutionCache, UnresolvedPolicy, WriteThroughResolutionCache}
import tools.{FixpointPass, FixtureTestWiring}

/**
 * The fixture pipeline with production's MEMOISING resolution caches, against the same
 * pipeline with the passthroughs every other spec runs.
 *
 * Every harness wires `ResolutionCache.passthrough`, which resolves live on every call — so
 * no spec had ever executed a cache HIT, and the branch that runs on one was untested
 * everywhere. That branch shipped a churn loop (79d5b30f2): an id answered from memory
 * carries no search, the row was stamped with the weakest basis in its place,
 * `resolvedOnWeakerEvidenceThanAvailable` read that as "re-resolve me", and the re-resolve
 * was answered from the same memory. Four attempts to reproduce it against the passthrough
 * failed, because the passthrough cannot reach it.
 *
 * So: boot one city's venues twice — once cold (passthrough), once with production's
 * `WriteThroughResolutionCache` over an in-memory store in place of Mongo — drive each
 * through a full tick, and require the same corpus. Then run one more tick on the warm
 * wiring, where every lookup a row has made before is answered from memory, and require it
 * to do no work at all.
 *
 * One city, not the corpus: two boots of the whole recorded corpus would double the heaviest
 * spec in the module, and the question is the cache's, not the corpus's — Poznań carries
 * every venue shape (chain, single-screen, deferred detail, a dub variant sharing its base
 * film's hints) that makes two rows ask the same question.
 */
class WarmResolutionCacheSpec extends AnyFlatSpec with Matchers {

  private val Fixture = "08-06-2026"

  /** Enrichment pinned to the calling thread. The spec compares TWO independent runs of the
   *  pipeline, which is only a comparison of the caches when nothing else differs between them —
   *  and on the default parallel budget something does: the order concurrent resolutions land in.
   *  Two spellings of one film that resolve to the same tmdbId ('Nowa fala' and 'Unlimited Show -
   *  Nowa Fala') then fold onto whichever landed first, so the two runs kept different ids for
   *  the same film about one run in five, and the spec blamed the cache for it. The same pin the
   *  determinism specs use (`ScrapeOrderDeterminismSpec`), for the same reason. */
  private class CitySlice extends FixtureTestWiring(Fixture) {
    override def scrapeCities: Set[String] = Set("poznan")
    override lazy val backgroundBudget: tools.ExecutionBudget = new tools.SameThreadExecutionBudget
  }

  /** Production's cache, with an in-memory store where production keeps a Mongo collection.
   *  The store is the infrastructure boundary; the memoisation, the TTL and the
   *  unresolved-policy handling above it are the production class's own. */
  private class WarmCitySlice extends CitySlice {
    override protected def resolutionCache(collection: String, unresolved: UnresolvedPolicy): ResolutionCache =
      new WriteThroughResolutionCache(new InMemoryResolutionStore(clock, titleNormalizer), unresolved = unresolved)
  }

  /** Boot, then one full tick with the projector on the change stream — the shape the
   *  fixpoint pass measures from. */
  private def settle(w: FixtureTestWiring): FixtureTestWiring = {
    w.bootStartup()
    FixpointPass.attachProjector(w)
    FixpointPass.run(w)
    w
  }

  private def corpus(w: FixtureTestWiring): Seq[StoredMovieRecord] =
    w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))

  /** Every field that moved, per film — `CorpusDiff.records` compares only the fields a
   *  scrape moves, and what a resolution writes (its basis, its attempt) is not among them. */
  private def differences(a: Seq[StoredMovieRecord], b: Seq[StoredMovieRecord]): String = {
    val after = b.map(r => r.id -> r).toMap
    a.flatMap(r => after.get(r.id) match {
      case None                          => Some(s"  ${r.title} (${r.year}): gone")
      case Some(o) if o.record != r.record => Some(s"  ${r.title} (${r.year}): ${MovieRecordDiff.describe(r.record, o.record, 1)}")
      case Some(o) if o != r             => Some(s"  ${r.title} (${r.year}): stored as ${o.title} (${o.year})")
      case _                             => None
    }).take(12).mkString("\n")
  }

  private lazy val cold = settle(new CitySlice)
  private lazy val warm = settle(new WarmCitySlice)

  "the pipeline with production's memoising resolution caches" should
    "settle exactly the corpus the passthrough pipeline does" in {
    val (coldCorpus, warmCorpus) = (corpus(cold), corpus(warm))
    coldCorpus should not be empty
    withClue(s"memoising the resolutions changed what the pipeline concluded:\n" +
             s"${CorpusDiff.records(coldCorpus, warmCorpus, "passthrough", "memoised")}\n" +
             // …and by id: `CorpusDiff` compares only the fields a scrape moves, so a film that is
             // simply ABSENT from one side, or differs in its resolution, printed nothing at all.
             s"${differences(coldCorpus, warmCorpus)}\n${differences(warmCorpus, coldCorpus)}\n") {
      warmCorpus shouldBe coldCorpus
    }
    withClue("memoising the resolutions changed the read model the site serves: ") {
      warm.readModelRepository.findAllMovies().sortBy(_._id) shouldBe cold.readModelRepository.findAllMovies().sortBy(_._id)
    }
  }

  // A booted corpus has asked each question once, so a tick alone never reaches a HIT: a
  // resolved row is not re-resolved and a fresh rating is not re-fetched. Production re-asks
  // them anyway — a repair script strips a resolution, a merge re-kicks a film's ratings — and
  // each re-ask is answered from memory. So re-ask EVERY question the corpus has settled:
  // once against empty caches, which searches and fills them, and once more, which is
  // answered from them. The two must conclude the same — the same film, on the same evidence
  // basis, with the same ratings — and the first must match the passthrough doing the same.
  //
  // Emptied first because the boot's own answers came through the staging graduation, which
  // records no `tmdbBasis`: a hit on one of those keeps "unknown" (by design — see
  // 79d5b30f2), so comparing it against a fresh search would measure the staging path, not
  // the cache.
  it should "conclude from a warm cache exactly what it concluded by searching" in {
    val searched = reask(cold)
    Seq(warm.tmdbIdCache, warm.imdbIdCache, warm.rtLinkCache, warm.mcLinkCache, warm.filmwebLinkCache).foreach(_.forgetAll())
    val filled = reask(warm)
    withClue(s"with its caches empty the memoising wiring concluded differently from the passthrough:\n" +
             s"${differences(searched, filled)}\n") {
      filled shouldBe searched
    }
    val fromMemory = reask(warm)
    withClue(s"answering from the warm cache concluded differently from the search that filled it:\n" +
             s"${differences(filled, fromMemory)}\n") {
      fromMemory shouldBe filled
    }
  }

  /** Strip every resolution and run the row's whole enrichment again — what a repair script
   *  and a merge re-kick do to a settled row in production. */
  private def reask(w: FixtureTestWiring): Seq[StoredMovieRecord] = {
    w.movieCache.snapshot().filter(_.record.tmdbId.isDefined).foreach { row =>
      w.movieCache.putIfPresent(w.movieCache.keyOf(row.title, row.year),
        r => r.copy(tmdbId = None, data = r.data.filterNot { case (source, _) => source == models.Tmdb }))
      w.fullySyncOne(row.title, row.year)
    }
    w.drainServices()
    corpus(w)
  }

  // Its OWN wiring, not the shared `warm`: the re-ask above strips and re-resolves every row of
  // that one, so measuring it here measured whatever the re-ask left behind — and only when the
  // specs ran in file order. Run alone (`-z`), the same test measured a different pass.
  it should "do no work at all on a second tick, every lookup answered from its warm cache" in {
    val settled = settle(new WarmCitySlice)
    FixpointPass.ledger(settled).assertNoChurn("a tick over unchanged input with every resolution cache warm")(
      FixpointPass.run(settled))
  }
}
