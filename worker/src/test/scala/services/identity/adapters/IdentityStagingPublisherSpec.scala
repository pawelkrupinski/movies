package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.VoterFamily
import services.identity.staging.{IdentityStaging, InMemoryStagingDocuments, StagedCluster, StagedQuestion, StagingDoc, StagingDocuments}
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

  private final class CountingDocuments extends StagingDocuments {
    private val inner = new InMemoryStagingDocuments
    var written = Seq.empty[String]
    var removed = Seq.empty[String]
    def index()                    = inner.index()
    def bodies(ids: Seq[String])   = inner.bodies(ids)
    def put(docs: Seq[StagingDoc]) = { written = docs.map(_.id); inner.put(docs) }
    def remove(ids: Seq[String])   = { removed = ids; inner.remove(ids) }
  }

  private def publisher(docs: StagingDocuments, resolvedCap: Int = 200, stagingCap: Int = 500) =
    new IdentityStagingPublisher("pl", new IdentityStaging(docs, clock), () => taken,
      d => waits.getOrElse(d.members.toSet, WaitingOn.model(d)), qs => outcomes.view.filterKeys(qs).toMap, clock, window = 6.hours,
      resolvedCap = resolvedCap, stagingCap = stagingCap)

  private def shown(docs: StagingDocuments): Seq[StagedCluster] = new IdentityStaging(docs, clock).ofCountry("pl").sortBy(_.since)

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

  it should "call a cluster whose listings no longer form it regrouped, and drop resolved ones past the window" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    // Rialto's listing alone, then joined by Kino Muza's: a cluster whose smallest listing, its id, is another
    waits = Map(Set(dune2) -> WaitingOn(0, Set(search), onTmdb = false))
    taken = Some(resolution(decision(Seq(dune2))))
    live.publish()

    waits = Map.empty
    taken = Some(resolution(decision(Seq(dune, dune2), Some(438631), ResolverDecision.Basis.OwnMatch)))
    live.publish()
    shown(docs).map(_.resolution) shouldBe Seq(Some(StagedCluster.Regrouped))

    clock.advance(java.time.Duration.ofHours(7))
    live.publish()
    shown(docs) shouldBe empty
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
