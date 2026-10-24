package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.VoterFamily
import services.identity.staging.{ChangeFeed, IdentityStaging, InMemoryStagingDocuments, StagedCluster, StagedQuestion, StagingDoc, StagingDocuments, StagingEvent}
import services.movies.ListingKey
import tools.MutableClock

import java.time.Instant
import scala.concurrent.duration.*

/**
 * The staging screen's publisher: a cluster whose take waits stages, with each question it waits on and what the
 * memory says of its last ask; once it waits on nothing it is resolved — settled with its take, or regrouped — kept
 * for the window and the cap; and a publish writes only what changed.
 */
class IdentityStagingPublisherSpec extends AnyFlatSpec with Matchers {
  private val clock = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
  private def key(venue: String, title: String) = ListingKey.Published(venue, title, None, Nil)
  private val dune   = key("Kino Muza", "Diuna")
  private val dune2  = key("Rialto", "Diuna")
  private val belle  = key("Kino Amok", "Belle")
  private val search = Question.Family(VoterFamily.Imdb, "title|Diuna")
  private val record = Question.Family(VoterFamily.Wiki, "record|Q123")
  private val find   = Question.Find("tt1160419")

  private def decision(keys: Seq[ListingKey], film: Option[Int] = None, basis: ResolverDecision.Basis = ResolverDecision.Basis.NoEvidence,
                       unanswered: Int = 0) = ResolverDecision(keys, film, 0.9, basis, Nil, unanswered = unanswered)()
  private def resolution(ds: ResolverDecision*) =
    Resolution(ds, ds.size, Map.empty, Nil, Nil, 0, 0, 0, 0, 0, Map(438631 -> IdentityMeasures.Film("Dune: Part One")))

  /** What the test's stage waits on and its memory knows, set as each case goes. */
  private var waits: Map[Set[ListingKey], WaitingOn] = Map.empty
  private var outcomes: Map[Question, QuestionMemory.Outcome] = Map.empty
  private var taken: Option[Resolution] = None
  /** What happened, as the projection and the stage tell it: the events the publisher drains. */
  private val feed = new ChangeFeed[StagingEvent](100)
  private def moved(listing: ListingKey, before: Option[Int], after: Option[Int]) = StagingEvent.Changed(TakeChange(listing, Some("film"), before, after))
  private def decidedOn(listing: ListingKey, film: Option[Int]) = StagingEvent.Changed(TakeChange(listing, None, None, film))

  private final class CountingDocuments extends StagingDocuments {
    private val inner = new InMemoryStagingDocuments
    var written = Seq.empty[String]
    var removed = Seq.empty[String]
    var docs    = Seq.empty[StagingDoc]
    def index()                    = inner.index()
    def bodies(ids: Seq[String])   = inner.bodies(ids)
    def put(docs: Seq[StagingDoc]) = { written = docs.map(_.id); this.docs = docs; inner.put(docs) }
    def remove(ids: Seq[String])   = { removed = ids; inner.remove(ids) }
  }

  private def publisher(docs: StagingDocuments, resolvedCap: Int = 200, stagingCap: Int = 500) =
    new IdentityStagingPublisher("pl", new IdentityStaging(docs, clock), () => taken, () => feed.drain(),
      d => waits.getOrElse(d.members.toSet, WaitingOn.model(d)), qs => outcomes.view.filterKeys(qs).toMap, clock, window = 6.hours,
      resolvedCap = resolvedCap, stagingCap = stagingCap)

  /** What the screen reads of the country: its staging clusters and its resolutions. */
  private def shown(docs: StagingDocuments): Seq[StagedCluster] =
    new IdentityStaging(docs, clock).read(Map.empty).filter(_.country == "pl").flatMap(_.changed).sortBy(c => (c.since, c.key))

  "The staging publisher" should "stage each cluster still waiting, with its questions, their outcomes and how many were attempted" in {
    val docs = new InMemoryStagingDocuments
    waits    = Map(Set(dune, dune2) -> WaitingOn(1, Set(search, record, find), onTmdb = false))
    outcomes = Map(search -> QuestionMemory.Outcome.Answered,
      record -> QuestionMemory.Outcome.Failed(2, Instant.parse("2026-10-24T12:10:00Z"), Some("www.wikidata.org")))
    taken    = Some(resolution(decision(Seq(dune, dune2), unanswered = 1), decision(Seq(belle), Some(1), ResolverDecision.Basis.OwnMatch)))
    publisher(docs).publish()

    val Seq(staged) = shown(docs)
    staged.staging shouldBe true
    staged.listings.map(_.venue) shouldBe Seq("Kino Muza", "Rialto")
    (staged.modelGaps, staged.questionCount, staged.attempted) shouldBe ((1, 3, 2))
    staged.take.basis shouldBe "NoEvidence"
    staged.questions.map(q => (q.text, q.outcome, q.attempts, q.host)) shouldBe Seq(
      ("find|tt1160419", StagedQuestion.NotAttempted, 0, None),
      ("wiki|record|Q123", StagedQuestion.Failed, 2, Some("www.wikidata.org")),
      ("imdb|title|Diuna", StagedQuestion.Answered, 0, None))
    staged.questions(1).nextRetryAt shouldBe Some(Instant.parse("2026-10-24T12:10:00Z").toEpochMilli)
    new IdentityStaging(docs, clock).read(Map.empty).head.beat.map(b => (b.staging, b.openByKind)) shouldBe
      Some((1, Map("Find" -> 1, "FamilyRecord" -> 1)))
  }

  it should "resolve a cluster that waits on nothing now, with the take it got, how long it waited, and its questions' last outcomes" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    waits    = Map(Set(dune) -> WaitingOn(0, Set(search, find), onTmdb = false))
    outcomes = Map.empty
    taken    = Some(resolution(decision(Seq(dune))))
    live.publish()

    clock.advance(java.time.Duration.ofMinutes(7))
    waits    = Map.empty
    outcomes = Map(search -> QuestionMemory.Outcome.Answered, find -> QuestionMemory.Outcome.Absent)
    taken    = Some(resolution(decision(Seq(dune), Some(438631), ResolverDecision.Basis.Agreed)))
    live.publish()

    val Seq(resolved) = shown(docs)
    resolved.resolution shouldBe Some(StagedCluster.Settled)
    resolved.take.title shouldBe Some("Dune: Part One")
    resolved.resolvedAt.get - resolved.since shouldBe 7.minutes.toMillis
    resolved.questions.map(q => q.text -> q.outcome).toMap shouldBe
      Map("imdb|title|Diuna" -> StagedQuestion.Answered, "find|tt1160419" -> StagedQuestion.Absent)
  }

  it should "keep why a take was decided: the first lines of its explanation, the rest counted" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    val why  = (1 to IdentityStagingPublisher.ExplanationShown + 2).map(i => s"evidence $i")
    waits    = Map.empty
    taken    = Some(resolution(decision(Seq(belle), Some(1), ResolverDecision.Basis.OwnMatch)))
    live.publish()

    clock.advanceSeconds(20)
    taken = Some(resolution(ResolverDecision(Seq(dune), Some(438631), 0.96, ResolverDecision.Basis.OwnMatch, why)()))
    feed.offer(Seq(decidedOn(dune, Some(438631))))
    live.publish()
    val Seq(decided) = shown(docs)
    decided.take.explanation shouldBe why.take(IdentityStagingPublisher.ExplanationShown) :+ "… and 2 more"
  }

  // Most new films are asked inline and settle inside one projection: no publish ever sees them waiting.
  it should "list a cluster the projection decided as decided, and one whose film it changed as changed, with the film it had" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    waits    = Map.empty
    outcomes = Map(find -> QuestionMemory.Outcome.Absent)
    taken    = Some(resolution(decision(Seq(belle), Some(1), ResolverDecision.Basis.OwnMatch)))
    live.publish()
    shown(docs) shouldBe empty   // no change since: nothing resolved

    clock.advanceSeconds(20)
    waits = Map.empty
    taken = Some(resolution(decision(Seq(belle), Some(2), ResolverDecision.Basis.OwnMatch),
      decision(Seq(dune), Some(438631), ResolverDecision.Basis.Agreed)))
    feed.offer(Seq(moved(belle, Some(1), Some(2)), decidedOn(dune, Some(438631))))
    live.publish()
    shown(docs).map(c => (c.listings.head.rawTitle, c.resolution, c.was, c.take.title)) should contain theSameElementsAs Seq(
      ("Belle", Some(StagedCluster.Changed), Some(1), None), ("Diuna", Some(StagedCluster.Decided), None, Some("Dune: Part One")))

    clock.advanceSeconds(20)
    live.publish()   // nothing moved since: both stay listed, unchanged
    shown(docs).map(_.resolution) should contain theSameElementsAs Seq(Some(StagedCluster.Changed), Some(StagedCluster.Decided))
  }

  it should "call a cluster whose listings no longer form it regrouped, written once for the database to delete after the window" in {
    val docs = new CountingDocuments
    val live = publisher(docs)
    // Rialto's listing alone, then joined by Kino Muza's: a cluster whose smallest listing, its id, is another
    waits = Map(Set(dune2) -> WaitingOn(0, Set(search), onTmdb = false))
    taken = Some(resolution(decision(Seq(dune2))))
    live.publish()

    waits = Map.empty
    taken = Some(resolution(decision(Seq(dune, dune2), Some(438631), ResolverDecision.Basis.OwnMatch)))
    feed.offer(Seq(decidedOn(dune, Some(438631)), moved(dune2, None, Some(438631))))
    live.publish()
    // the cluster it was in, regrouped; the one its listings formed, which gave Rialto's listing a film: moved
    shown(docs).map(_.resolution) should contain theSameElementsAs Seq(Some(StagedCluster.Regrouped), Some(StagedCluster.Gained))

    docs.docs.filter(_.kind == "resolved").map(d => d.expireAt.map(_.toEpochMilli)) shouldBe
      Seq.fill(2)(Some(clock.millis() + 6.hours.toMillis))

    clock.advanceSeconds(20)
    live.publish()
    docs.written shouldBe Seq("pl|beat")   // a resolution is never written again: the database deletes it
  }

  it should "list a cluster whose film moves twice twice" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    waits = Map.empty
    taken = Some(resolution(decision(Seq(belle), Some(1), ResolverDecision.Basis.OwnMatch)))
    live.publish()
    clock.advanceSeconds(20)
    taken = Some(resolution(decision(Seq(belle), Some(2), ResolverDecision.Basis.OwnMatch)))
    feed.offer(Seq(moved(belle, Some(1), Some(2))))
    live.publish()
    clock.advanceSeconds(20)
    taken = Some(resolution(decision(Seq(belle), None, ResolverDecision.Basis.Vetoed)))
    feed.offer(Seq(moved(belle, Some(2), None)))
    live.publish()
    shown(docs).map(c => (c.resolution, c.was, c.take.basis)) should contain theSameElementsAs Seq(
      (Some(StagedCluster.Changed), Some(1), "OwnMatch"), (Some(StagedCluster.Lost), Some(2), "Vetoed"))
  }

  it should "not call a cluster re-keyed by a new listing decided or moved: none of its listings' films changed" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    val early = key("Kino Amok", "A Diuna")   // sorts first: the cluster's id, its key, moves to it
    waits = Map.empty
    taken = Some(resolution(decision(Seq(dune), Some(438631), ResolverDecision.Basis.OwnMatch), decision(Seq(belle))))
    live.publish()
    clock.advanceSeconds(20)
    taken = Some(resolution(decision(Seq(early, dune), Some(438631), ResolverDecision.Basis.OwnMatch), decision(Seq(early, belle))))
    feed.offer(Seq(decidedOn(early, Some(438631))))
    live.publish()
    shown(docs) shouldBe empty
  }

  // A new cluster's families asked inline: it waited, and settled, inside one projection, between two publishes.
  it should "list a cluster whose wait began and ended between two publishes as settled, since its wait began, on what it waited" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    waits    = Map.empty
    outcomes = Map(search -> QuestionMemory.Outcome.Answered)
    taken    = Some(resolution(decision(Seq(dune), Some(438631), ResolverDecision.Basis.Agreed)))
    val began = clock.millis()
    feed.offer(Seq(StagingEvent.Waiting(StoredFamily.idOf(Seq(dune)), began, Set(search)), StagingEvent.Settled(StoredFamily.idOf(Seq(dune)), began + 3000)))
    clock.advanceSeconds(20)
    live.publish()
    val Seq(resolved) = shown(docs)
    (resolved.resolution, resolved.since, resolved.resolvedAt) shouldBe ((Some(StagedCluster.Settled), began, Some(began + 3000)))
    resolved.questions.map(q => q.text -> q.outcome) shouldBe Seq("imdb|title|Diuna" -> StagedQuestion.Answered)
    resolved.take.title shouldBe Some("Dune: Part One")
  }

  it should "stage a cluster since its wait began, not since a publish first saw it" in {
    val docs = new InMemoryStagingDocuments
    waits = Map(Set(dune) -> WaitingOn(0, Set(search), onTmdb = false))
    taken = Some(resolution(decision(Seq(dune))))
    val began = clock.millis()
    feed.offer(Seq(StagingEvent.Waiting(StoredFamily.idOf(Seq(dune)), began, Set(search))))
    clock.advanceSeconds(15)
    publisher(docs).publish()
    shown(docs).map(c => (c.staging, c.since)) shouldBe Seq((true, began))
  }

  it should "write the changes a failed publish drained with the next" in {
    val docs   = new InMemoryStagingDocuments
    var broken = true
    val flaky  = new StagingDocuments {
      def index()                    = docs.index()
      def bodies(ids: Seq[String])   = docs.bodies(ids)
      def put(ds: Seq[StagingDoc])   = if (broken) throw new IllegalStateException("mongo down") else docs.put(ds)
      def remove(ids: Seq[String])   = docs.remove(ids)
    }
    val live = publisher(flaky)
    waits = Map.empty
    taken = Some(resolution(decision(Seq(belle), Some(2), ResolverDecision.Basis.OwnMatch)))
    feed.offer(Seq(moved(belle, Some(1), Some(2))))
    an[IllegalStateException] should be thrownBy live.publish()
    broken = false
    live.publish()
    shown(docs).map(c => (c.resolution, c.was)) shouldBe Seq((Some(StagedCluster.Changed), Some(1)))
  }

  it should "report its takes by basis" in {
    val heldBy  = scala.collection.mutable.Buffer.empty[(Map[String, Int], Map[String, Int])]
    val metrics: IdentityStagingPublisher.TakeMetrics = (clusters, listings) => heldBy += clusters -> listings
    val live = new IdentityStagingPublisher("pl", new IdentityStaging(new InMemoryStagingDocuments, clock), () => taken, () => feed.drain(),
      d => WaitingOn.model(d), _ => Map.empty, clock, takeMetrics = metrics)
    taken = Some(resolution(decision(Seq(belle), None, ResolverDecision.Basis.Vetoed), decision(Seq(dune, dune2), Some(438631), ResolverDecision.Basis.Agreed)))
    live.publish()
    heldBy.last shouldBe ((Map("Vetoed" -> 1, "Agreed" -> 1), Map("Vetoed" -> 1, "Agreed" -> 2)))
  }


  it should "keep the newest resolved clusters up to its cap, and list the longest waiting up to its own" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs, resolvedCap = 1, stagingCap = 1)
    waits = Map(Set(dune) -> WaitingOn(0, Set(search), onTmdb = false))
    taken = Some(resolution(decision(Seq(dune)), decision(Seq(belle))))
    live.publish()
    clock.advanceSeconds(20)
    waits = Map(Set(dune) -> WaitingOn(0, Set(search), onTmdb = false), Set(belle) -> WaitingOn(1, Set.empty, onTmdb = false))
    live.publish()
    shown(docs).map(_.listings.head.rawTitle) shouldBe Seq("Diuna")   // the one waiting longest
    new IdentityStaging(docs, clock).read(Map.empty).head.beat.map(b => (b.staging, b.shown)) shouldBe Some((2, 1))

    waits = Map.empty
    live.publish()
    shown(docs).map(c => (c.listings.head.rawTitle, c.resolution)) shouldBe Seq(("Diuna", Some(StagedCluster.Settled)))
  }

  it should "write only the clusters that changed, and its beat" in {
    val docs = new CountingDocuments
    val live = publisher(docs)
    outcomes = Map.empty
    waits = Map(Set(dune) -> WaitingOn(0, Set(search), onTmdb = false), Set(belle) -> WaitingOn(0, Set(find), onTmdb = false))
    taken = Some(resolution(decision(Seq(dune)), decision(Seq(belle))))
    live.publish()
    docs.written.size shouldBe 3

    clock.advanceSeconds(20)
    live.publish()
    docs.written shouldBe Seq("pl|beat")

    outcomes = Map(find -> QuestionMemory.Outcome.Answered)
    live.publish()
    docs.written.size shouldBe 2
    docs.written should contain ("pl|beat")
  }

  it should "resume from what it published before a restart: a cluster waiting since then keeps its start" in {
    val docs = new InMemoryStagingDocuments
    waits = Map(Set(dune) -> WaitingOn(0, Set(search), onTmdb = false))
    taken = Some(resolution(decision(Seq(dune))))
    publisher(docs).publish()
    val since = shown(docs).head.since

    clock.advanceSeconds(90)
    publisher(docs).publish()
    shown(docs).map(_.since) shouldBe Seq(since)
  }

  it should "publish nothing before the projection wrote its first take" in {
    val docs = new CountingDocuments
    taken = None
    publisher(docs).publish()
    docs.written shouldBe empty
  }
}
