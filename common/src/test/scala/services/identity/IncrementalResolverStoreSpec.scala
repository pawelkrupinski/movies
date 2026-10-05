package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}
import tools.IndependentCases

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

  /** Every seed's answers-while-down outcome, computed once: two properties read it. */
  private lazy val answeredWhileDownBySeed: Seq[(Long, (Boolean, Int, Int))] =
    IndependentCases.map(Seeds)(seed => seed -> answeredWhileDown(seed))

  "a restored model" should "decide as a resolve of what it holds, whatever moved while it was down" in {
    IndependentCases.map(Seeds)(seed => seed -> restart(seed)).collect { case (seed, (false, _, _)) => seed } shouldBe empty
  }

  it should "decide as a resolve when only answers came in while it was down" in {
    answeredWhileDownBySeed.collect { case (seed, (false, _, _)) => seed } shouldBe empty
  }

  it should "re-resolve fewer families than there are when only answers came in, reusing the ones they did not reach" in {
    val (resolved, held) = answeredWhileDownBySeed.map(_._2).foldLeft((0, 0)) { case ((r, h), (_, resolves, families)) => (r + resolves, h + families) }
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

  /** A model store and a trace store that remember every id a replace dropped. */
  private final class RecordingStores {
    val familiesDropped = scala.collection.mutable.ListBuffer.empty[Set[String]]
    val tracesDropped   = scala.collection.mutable.ListBuffer.empty[Set[String]]
    private val keptFamilies = new InMemoryIdentityModelStore
    private val keptTraces   = new InMemoryIdentityTraceStore
    val families: IdentityModelStore = new IdentityModelStore {
      def families(): Seq[StoredFamily] = keptFamilies.families()
      def replace(removed: Set[String], added: Seq[StoredFamily]): Unit = { familiesDropped += removed; keptFamilies.replace(removed, added) }
      def rulesVersion: Option[String] = keptFamilies.rulesVersion
      def recordRulesVersion(version: String): Unit = keptFamilies.recordRulesVersion(version)
    }
    val traces: IdentityTraceStore = (removed: Set[String], added: Seq[FamilyTraces]) => {
      tracesDropped += removed; keptTraces.replace(removed, added)
    }
    def clear(): Unit = { familiesDropped.clear(); tracesDropped.clear() }
  }

  // A rules change re-resolves every family, and most decide what they decided before: dropping every
  // stored family and trace FIRST left nothing for the re-resolve to compare with, so all ~165k US
  // traces and every family were deleted and written again on each such boot, for unchanged decisions.
  it should "drop, after a rules change, only the families and traces it did not decide again" in {
    val corpus = GeneratedIdentityCorpus.generate(13L, normalizer, films = 12, listings = 48)
    val stores = new RecordingStores
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = stores.families,
      rules = "v1", traces = stores.traces).seed(corpus.listings)
    stores.clear()
    val next = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = stores.families,
      rules = "v2", traces = stores.traces)
    next.restore(corpus.listings)
    val decided = stores.families.families().map(_.id).toSet
    decided should not be empty
    stores.familiesDropped.flatten.toSet intersect decided shouldBe empty
    stores.tracesDropped.flatten.toSet intersect decided shouldBe empty
    ResolutionSignature.of(next) shouldBe
      ResolutionSignature.of(IdentityResolver.resolveWith(corpus.listings, corpus.lookups, normalizer, calibration, IdentityResolver.Mutation.None))
  }

  it should "drop, when answers moved while it was down, only the families and traces it did not decide again" in {
    val corpus  = GeneratedIdentityCorpus.generate(7L, normalizer, films = 12, listings = 48)
    val lookups = new FillingLookups(corpus.lookups, new Random(7L * 31))
    val stores  = new RecordingStores
    new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = stores.families,
      traces = stores.traces).seed(corpus.listings)
    lookups.answer(); lookups.answer()
    stores.clear()
    new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = stores.families,
      traces = stores.traces).restore(corpus.listings)
    val decided = stores.families.families().map(_.id).toSet
    stores.familiesDropped.flatten.toSet intersect decided shouldBe empty
    stores.tracesDropped.flatten.toSet intersect decided shouldBe empty
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

  /** `family` as it comes off the wire: its BSON bytes, with the content digest the store writes beside it. */
  private def wire(family: StoredFamily): java.nio.ByteBuffer = {
    val document = MongoIdentityModelStore.encode(family).append("content", org.bson.BsonInt64(42L))
    java.nio.ByteBuffer.wrap(new org.bson.RawBsonDocument(document, new org.bson.codecs.BsonDocumentCodec).getByteBuffer.array)
  }
  private def readOff(bytes: java.nio.ByteBuffer): StoredFamily =
    MongoIdentityModelStore.StoredFamilyCodec.decode(new org.bson.BsonBinaryReader(bytes.duplicate()), org.bson.codecs.DecoderContext.builder().build())

  it should "read straight off its wire bytes as it was written" in {
    val corpus = GeneratedIdentityCorpus.generate(11L, normalizer, films = 12, listings = 48)
    val store  = new InMemoryIdentityModelStore
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store).seed(corpus.listings)
    store.families().foreach(family => readOff(wire(family)) shouldBe family)
  }

  // A US take-up reads ~2,100 families. Decoded as a BSON tree first, each was a map per embedded document and a
  // string per key, all dropped once the family was built from them — garbage the boot promoted (2026-10-05).
  it should "read off the wire without building the BSON tree of its bytes" in {
    val corpus   = GeneratedIdentityCorpus.generate(11L, normalizer, films = 12, listings = 48)
    val store    = new InMemoryIdentityModelStore
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store).seed(corpus.listings)
    val bytes    = store.families().map(wire)
    val codec    = new org.bson.codecs.BsonDocumentCodec
    val context  = org.bson.codecs.DecoderContext.builder().build()
    def asTree(b: java.nio.ByteBuffer) = codec.decode(new org.bson.BsonBinaryReader(b.duplicate()), context)
    def read()      = (1 to 50).foreach(_ => bytes.foreach(readOff))
    def treeAlone() = (1 to 50).foreach(_ => bytes.foreach(asTree))
    def viaTree()   = (1 to 50).foreach(_ => bytes.foreach(b => MongoIdentityModelStore.decode(asTree(b))))
    read(); treeAlone(); viaTree()                                         // warm all three
    val (_, reading) = tools.ThreadAllocation.of(read())
    val (_, tree)    = tools.ThreadAllocation.of(treeAlone())
    val (_, both)    = tools.ThreadAllocation.of(viaTree())
    // Read straight off the bytes, nothing outlives its field: the tree's maps, and the key strings they hold, were the
    // whole reply's until its families were built. Measured here as the allocation they cost (~25% of the read).
    withClue(s"read $reading bytes; via the tree $both, the tree alone $tree: ")(reading.toDouble should be < both * 0.9)
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

  /** `listing` with a key object of its own, equal to its key: as a scrape or a store read hands it over. */
  private def ownKey(listing: Listing): Listing = listing.copy(key = ListingKeyBson.decode(ListingKeyBson.encode(listing.key)))

  // worker-us held ~2 more key objects per listing than the listings it holds: the families a restore takes up are
  // decoded from the store, their listings, node maps and every decision's members keys of their own beside the held
  // listing's — and a node's text beside the corpus's. One key object per listing is the held listing's.
  "a restored model" should "name every listing by the held listing's own key object, and every node by the corpus's text" in {
    val corpus  = GeneratedIdentityCorpus.generate(11L, normalizer, films = 12, listings = 48)
    val written = new InMemoryIdentityModelStore
    new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = written).seed(corpus.listings)
    val decoded = new InMemoryIdentityModelStore
    written.rulesVersion.foreach(decoded.recordRulesVersion)
    decoded.replace(Set.empty, written.families().map(family => MongoIdentityModelStore.decode(MongoIdentityModelStore.encode(family))))
    val held     = corpus.listings.map(ownKey)
    val restored = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = decoded)
    restored.restore(held)
    restored.familiesResolved shouldBe 0
    val heldKey = held.map(listing => listing.key -> listing.key).toMap
    restored.familyKeys.filterNot(key => heldKey(key) eq key).take(5).toSeq shouldBe empty
    restored.familyOf.keys.filterNot(key => heldKey(key) eq key).take(5).toSeq shouldBe empty
    restored.familyNodeTexts.filterNot { case (key, text) => restored.nodeTextOf(key).exists(_ eq text) }.take(5).toSeq shouldBe empty
  }

  "a model" should "name a listing re-published with other fields by its new key object, everywhere" in {
    val corpus = GeneratedIdentityCorpus.generate(11L, normalizer, films = 12, listings = 48)
    val model  = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.seed(corpus.listings)
    val moved  = ownKey(corpus.listings.head).copy(runtime = Some(321))
    model.listingsSeen(Seq(moved))
    model.familyKeys.filter(_ == moved.key).filterNot(_ eq moved.key).toSeq shouldBe empty
    model.familyOf.keys.filter(_ == moved.key).filterNot(_ eq moved.key).toSeq shouldBe empty
    model.heldAt(moved.venue).filter(_ == moved.key).filterNot(_ eq moved.key) shouldBe empty
  }
}
