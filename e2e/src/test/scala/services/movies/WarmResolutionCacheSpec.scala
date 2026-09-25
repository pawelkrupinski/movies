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

  /** Poznań's venues alone — see the class doc. Enrichment stays on the default parallel
   *  budget, so the two runs may land concurrent resolutions in different orders; `corpus`
   *  and `readModel` compare films by key rather than id for exactly that reason. */
  private class CitySlice extends FixtureTestWiring(Fixture) {
    override def scrapeCities: Set[String] = Set("poznan")
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

  /** Every row, with its id replaced by its stored KEY. A film's id is opaque and permanent —
   *  it is minted from whichever spelling is seen FIRST and a fold keeps it (see `FilmId`) —
   *  so two runs whose parallel enrichment lands in different orders legitimately id the same
   *  film differently ('Nowa fala' vs 'Unlimited Show - Nowa Fala', tmdbId 1254808:
   *  `SharedTmdbIdArrivalOrderSpec`). Comparing ids made this spec fail one run in five on
   *  that alone; the key and everything else must still agree exactly. */
  private def corpus(w: FixtureTestWiring): Seq[StoredMovieRecord] =
    w.movieRepository.findAll().map(r => r.copy(id = FilmId(r.key(w.movieRepository.normalizer))))
      .sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))

  /** The read model's cards, re-addressed from film id to stored key the same way. */
  private def readModel(w: FixtureTestWiring) = {
    val keyOf = w.movieRepository.findAll().map(r => r.id.value -> r.key(w.movieRepository.normalizer)).toMap
    w.readModelRepository.findAllMovies().map { m =>
      val (film, variant) = m._id.span(_ != '~')
      m.copy(_id = keyOf.getOrElse(film, film) + variant)
    }.sortBy(_._id)
  }

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
      readModel(warm) shouldBe readModel(cold)
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
