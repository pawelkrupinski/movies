package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.util.Random

/** The incremental model taken up from its store after a restart: whatever moved while it was
 *  down — listings, answers — it decides as a resolve of what it holds, re-resolving only the
 *  families that moved. */
class IncrementalResolverStoreSpec extends AnyFlatSpec with Matchers {

  private val normalizer  = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.fromResource("services/identity/test-calibration.json").get
  private val Seeds       = 1L to 24L


  private def apply(model: IncrementalResolver, event: IdentityEvent): Unit = event match {
    case IdentityEvent.Seen(listings) => model.listingsSeen(listings)
    case IdentityEvent.Gone(keys)     => model.listingsGone(keys)
    case IdentityEvent.Answered(c)    => model.answersChanged(c)
  }

  /** A model runs the first half of a sequence into `store`; the second half happens while it is
   *  down; a new model restores from the store over what is held then. What it decides then, and
   *  how many families it re-resolved against how many it holds. */
  private def restart(seed: Long, mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None): (Boolean, Int, Int) = {
    val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
    val lookups = new FillingLookups(corpus.lookups, new Random(seed * 31))
    val store   = new InMemoryIdentityModelStore
    val random  = new RandomIdentityEvents(corpus.listings, lookups, seed, steps = 60)
    val events  = random.events
    val first   = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store)
    events.take(30).foreach(apply(first, _))
    events.foreach(_ => ())                                   // the rest happens while no model runs
    val restored = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store, mutation = mutation)
    restored.restore(random.held.values.toSeq)
    val expected = IdentityResolver.resolveWith(random.held.values, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    (ResolutionSignature.of(restored) == ResolutionSignature.of(expected), restored.familiesResolved, expected.families)
  }

  /** A model takes the whole corpus into `store`; while it is down only the fill runs — gaps close,
   *  no listing moves; a new model restores from the store. What it decides then, and how many
   *  families it re-resolved against how many there are. */
  private def answeredWhileDown(seed: Long, mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None): (Boolean, Int, Int) = {
    val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
    val lookups = new FillingLookups(corpus.lookups, new Random(seed * 31))
    val store   = new InMemoryIdentityModelStore
    new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store).seed(corpus.listings)
    lookups.answer(); lookups.answer()
    val restored = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store, mutation = mutation)
    restored.restore(corpus.listings)
    val expected = IdentityResolver.resolveWith(corpus.listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    (ResolutionSignature.of(restored) == ResolutionSignature.of(expected), restored.familiesResolved, expected.families)
  }

  "a restored model" should "decide as a resolve of what it holds, whatever moved while it was down" in {
    Seeds.map(seed => seed -> restart(seed)).collect { case (seed, (false, _, _)) => seed } shouldBe empty
  }

  it should "decide as a resolve when only answers came in while it was down" in {
    Seeds.map(seed => seed -> answeredWhileDown(seed)).collect { case (seed, (false, _, _)) => seed } shouldBe empty
  }

  it should "re-resolve fewer families than there are when only answers came in, reusing the ones they did not reach" in {
    val (resolved, held) = Seeds.map(answeredWhileDown(_)).foldLeft((0, 0)) { case ((r, h), (_, resolves, families)) => (r + resolves, h + families) }
    resolved should be < held
  }

  it should "re-resolve nothing after a restart with nothing moved" in {
    val corpus  = GeneratedIdentityCorpus.generate(5L, normalizer, films = 12, listings = 48)
    val store   = new InMemoryIdentityModelStore
    val first   = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store)
    first.seed(corpus.listings)
    val again   = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store)
    again.restore(corpus.listings)
    again.familiesResolved shouldBe 0
    ResolutionSignature.of(again) shouldBe ResolutionSignature.of(first)
  }

  it should "be caught when it trusts a stored family whose corpus facts moved (the teeth)" in {
    Seeds.forall(seed => answeredWhileDown(seed, IncrementalResolver.Mutation.TrustStored)._1) shouldBe false
  }

  it should "re-resolve a listing two stored families both claim" in {
    val corpus = GeneratedIdentityCorpus.generate(9L, normalizer, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store).seed(corpus.listings)
    val some   = store.families().head
    store.replace(Set.empty, Seq(some.copy(id = some.id + "-copy")))       // a crash between two writes
    val again  = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store)
    again.restore(corpus.listings)
    ResolutionSignature.of(again) shouldBe
      ResolutionSignature.of(IdentityResolver.resolveWith(corpus.listings, corpus.lookups, normalizer, calibration, IdentityResolver.Mutation.None))
    store.families().map(_.id) should not contain (some.id + "-copy")
  }

  it should "re-resolve every stored family when the rules it was decided under changed" in {
    val corpus = GeneratedIdentityCorpus.generate(13L, normalizer, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store, rules = "v1")
      .seed(corpus.listings)
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store, rules = "v1")
      .restore(corpus.listings)
    store.rulesVersion shouldBe Some("v1")
    val next   = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store, rules = "v2")
    next.restore(corpus.listings)
    val whole  = IdentityResolver.resolveWith(corpus.listings, corpus.lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    next.familiesResolved should be >= whole.families
    ResolutionSignature.of(next) shouldBe ResolutionSignature.of(whole)
    store.rulesVersion shouldBe Some("v2")
  }

  "the resolver's code version" should "be the build's digest of what the resolver is built from" in {
    IdentityRules.codeVersion should fullyMatch regex "[0-9a-f]{64}"
  }

  "a stored family" should "read back from its BSON as it was written" in {
    val corpus = GeneratedIdentityCorpus.generate(11L, normalizer, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store).seed(corpus.listings)
    store.families() should not be empty
    store.families().foreach(family => MongoIdentityModelStore.decode(MongoIdentityModelStore.encode(family)) shouldBe family)
  }

  // A restore decodes every stored family: its listing keys were read twice (the `listings` array and
  // the `nodes` pairs) and each listing's node text on its own, so equal values were separate objects
  // for the model's lifetime — worker-us held the node texts ("It Follows\0It Follows\0…") once per
  // listing, and a second copy of every store-decoded listing key.
  it should "read back sharing one instance of each listing key and each node text" in {
    val corpus = GeneratedIdentityCorpus.generate(11L, normalizer, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store).seed(corpus.listings)
    val decoded = store.families().map(family => MongoIdentityModelStore.decode(MongoIdentityModelStore.encode(family)).family)
    decoded.exists(_.nodeKeys.size > 1) shouldBe true
    decoded.foreach { family =>
      val listed = family.listings.toSeq
      family.nodeKeys.keys.foreach(key => withClue(s"$key: ")(listed.exists(_ eq key) shouldBe true))
      family.nodeKeys.values.groupBy(identity).values.foreach(same => withClue("node texts: ")(same.forall(_ eq same.head) shouldBe true))
    }
  }

  "a node's key as text" should "be its evidence's own key when no pin blocks it" in {
    val evidenceKey = new String("It Follows\u0000It Follows\u0000\u0000")
    (CandidateGeneration.nodeKeyText((evidenceKey, Set.empty)) eq evidenceKey) shouldBe true
  }
}
