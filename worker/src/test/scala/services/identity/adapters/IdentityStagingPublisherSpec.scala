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
  private def moved(listing: ListingKey, before: Option[Int], after: Option[Int]) = TakeChange(listing, Some("film"), before, after)
  private def decidedOn(listing: ListingKey, film: Option[Int], joined: Boolean = false) = TakeChange(listing, None, None, film, joined = joined)
  private var projections = 0L
  /** One written projection's change list, told to the feed. */
  private def projected(changes: TakeChange*): Unit = {
    projections += 1
    feed.offer(changes.map(StagingEvent.Changed(_, projections, clock.millis())))
  }

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

  private def publisher(docs: StagingDocuments, stagingCap: Int = 500, metrics: IdentityStagingPublisher.TakeMetrics = IdentityStagingPublisher.TakeMetrics.Unrecorded,
                        filmTitle: Int => Option[String] = _ => None) =
    new IdentityStagingPublisher("pl", new IdentityStaging(docs, clock), () => taken, () => feed.drain(),
      d => waits.getOrElse(d.members.toSet, WaitingOn.model(d)), qs => outcomes.view.filterKeys(qs).toMap, clock, window = 6.hours,
      stagingCap = stagingCap, takeMetrics = metrics, filmTitle = filmTitle)

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
    projected(decidedOn(dune, Some(438631)))
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
    projected(moved(belle, Some(1), Some(2)), decidedOn(dune, Some(438631)))
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
    projected(decidedOn(dune, Some(438631), joined = true), moved(dune2, None, Some(438631)))
    live.publish()
    // the cluster it was in, regrouped; Kino Muza's listing joining it; Rialto's, given a film: moved
    shown(docs).map(c => (c.resolution, c.listings.map(_.venue))) should contain theSameElementsAs Seq(
      (Some(StagedCluster.Regrouped), Seq("Rialto")), (Some(StagedCluster.Joined), Seq("Kino Muza")), (Some(StagedCluster.Gained), Seq("Rialto")))

    docs.docs.filter(_.kind == "resolved").map(d => d.expireAt.map(_.toEpochMilli)) shouldBe
      Seq.fill(3)(Some(clock.millis() + 6.hours.toMillis))

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
    projected(moved(belle, Some(1), Some(2)))
    live.publish()
    clock.advanceSeconds(20)
    taken = Some(resolution(decision(Seq(belle), None, ResolverDecision.Basis.Vetoed)))
    projected(moved(belle, Some(2), None).copy(basisAfter = Some(ResolverDecision.Basis.Vetoed)))
    live.publish()
    shown(docs).map(c => (c.resolution, c.was, c.take.basis)) should contain theSameElementsAs Seq(
      (Some(StagedCluster.Changed), Some(1), "OwnMatch"), (Some(StagedCluster.Lost), Some(2), "Vetoed"))
  }

  it should "list a new listing re-keying a cluster as joined, and none of its listings decided or moved" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs)
    val early = key("Kino Amok", "A Diuna")   // sorts first: the cluster's id, its key, moves to it
    waits = Map.empty
    taken = Some(resolution(decision(Seq(dune), Some(438631), ResolverDecision.Basis.OwnMatch), decision(Seq(belle))))
    live.publish()
    clock.advanceSeconds(20)
    taken = Some(resolution(decision(Seq(early, dune), Some(438631), ResolverDecision.Basis.OwnMatch), decision(Seq(early, belle))))
    projected(decidedOn(early, Some(438631), joined = true))
    live.publish()
    shown(docs).map(c => (c.resolution, c.listings.map(_.rawTitle), c.listingCount)) shouldBe Seq((Some(StagedCluster.Joined), Seq("A Diuna"), 1))
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

  // DE ZOO PALAST Berlin's "Gigi", 2026-10-24: staging, then Agreed on tmdb 17281 — shown as `settled`, which never
  // compares films, and as "tmdb 17281": the film only the agreement took is not in the model's resolution.
  it should "list a cluster that stopped waiting and whose film moved as that move, after its wait, titled from the TMDB store" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs, filmTitle = Map(17281 -> "Gigi").get)
    val gigi = key("ZOO PALAST Berlin", "Gigi")
    waits = Map(Set(gigi) -> WaitingOn(0, Set(search), onTmdb = false))
    taken = Some(resolution(decision(Seq(gigi), None, ResolverDecision.Basis.Vetoed)))
    live.publish()
    val began = shown(docs).head.since
    clock.advanceSeconds(180)
    waits = Map.empty
    outcomes = Map(search -> QuestionMemory.Outcome.Answered)
    taken = Some(resolution(decision(Seq(gigi), Some(17281), ResolverDecision.Basis.Agreed)))
    projected(moved(gigi, None, Some(17281)).copy(basisAfter = Some(ResolverDecision.Basis.Agreed)))
    live.publish()
    val Seq(resolved) = shown(docs)
    (resolved.resolution, resolved.was, resolved.since, resolved.take.title) shouldBe ((Some(StagedCluster.Gained), None, began, Some("Gigi")))
    resolved.resolvedAt.get - resolved.since shouldBe 180000L   // after waiting 3 minutes
    resolved.questions.map(q => q.text -> q.outcome) shouldBe Seq("imdb|title|Diuna" -> StagedQuestion.Answered)
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
    projected(moved(belle, Some(1), Some(2)))
    an[IllegalStateException] should be thrownBy live.publish()
    broken = false
    live.publish()
    shown(docs).map(c => (c.resolution, c.was)) shouldBe Seq((Some(StagedCluster.Changed), Some(1)))
  }

  it should "report its takes by basis" in {
    val heldBy  = scala.collection.mutable.Buffer.empty[(Map[String, Int], Map[String, Int])]
    val metrics = new IdentityStagingPublisher.TakeMetrics {
      def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit = heldBy += clusters -> listings
      def dropped(events: Long): Unit                                        = ()
    }
    val live = new IdentityStagingPublisher("pl", new IdentityStaging(new InMemoryStagingDocuments, clock), () => taken, () => feed.drain(),
      d => WaitingOn.model(d), _ => Map.empty, clock, takeMetrics = metrics)
    taken = Some(resolution(decision(Seq(belle), None, ResolverDecision.Basis.Vetoed), decision(Seq(dune, dune2), Some(438631), ResolverDecision.Basis.Agreed)))
    live.publish()
    heldBy.last shouldBe ((Map("Vetoed" -> 1, "Agreed" -> 1), Map("Vetoed" -> 1, "Agreed" -> 2)))
  }


  it should "list the longest waiting up to its cap" in {
    val docs = new InMemoryStagingDocuments
    val live = publisher(docs, stagingCap = 1)
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

  it should "count what its feed let go in its beat and its metric, never dropping one silently" in {
    val docs    = new InMemoryStagingDocuments
    val small   = new ChangeFeed[StagingEvent](2)
    var counted = 0L
    val metrics = new IdentityStagingPublisher.TakeMetrics {
      def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit = ()
      def dropped(events: Long): Unit                                        = counted += events
    }
    val live = new IdentityStagingPublisher("pl", new IdentityStaging(docs, clock), () => taken, () => small.drain(),
      d => WaitingOn.model(d), _ => Map.empty, clock, takeMetrics = metrics)
    waits = Map.empty
    taken = Some(resolution(decision(Seq(dune, dune2, belle), Some(438631), ResolverDecision.Basis.OwnMatch)))
    small.offer(Seq(dune, dune2, belle).map(l => StagingEvent.Changed(decidedOn(l, Some(438631)), 1, clock.millis())))
    live.publish()
    counted shouldBe 1
    new IdentityStaging(docs, clock).read(Map.empty).head.beat.flatMap(_.dropped) shouldBe Some(1L)
    live.publish()
    new IdentityStaging(docs, clock).read(Map.empty).head.beat.flatMap(_.dropped) shouldBe None   // counted once
  }

  // Exactly once, proved over random runs: every listing's take transition a projection tells the feed is in exactly one
  // resolution written — through clusters forming, merging, splitting and re-keying, listings arriving and leaving,
  // films changing and unmatched clusters matched, several projections between two publishes, publishes that fail and
  // are written by the next, and the publisher restarting.
  it should "write every change it is told in exactly one resolution, whatever happens between and to its publishes" in {
    val listings = (1 to 8).map(i => key(s"Kino $i", s"Film ${i % 3}"))
    (1 to 300).foreach { seed =>
      val rng     = new scala.util.Random(seed)
      val docs    = new InMemoryStagingDocuments
      var broken  = false
      val flaky   = new StagingDocuments {
        def index()                  = docs.index()
        def bodies(ids: Seq[String]) = docs.bodies(ids)
        def put(ds: Seq[StagingDoc]) = if (broken) throw new IllegalStateException("mongo down") else docs.put(ds)
        def remove(ids: Seq[String]) = docs.remove(ids)
      }
      val told    = scala.collection.mutable.ArrayBuffer.empty[String]
      var live    = publisher(flaky)
      var stored  = Map.empty[ListingKey, Option[Int]]   // each listing's film on the site, as the projection wrote it
      waits = Map.empty
      (1 to 30).foreach { _ =>
        // a projection: some listings published, grouped and matched at random — merges, splits, re-keys, films moving
        val published = listings.filter(_ => rng.nextInt(4) != 0)
        val groups    = published.groupBy(_ => rng.nextInt(3)).values.toSeq.map(_.sorted)
        val clusters  = groups.map(ls => ls -> Option.when(rng.nextInt(3) != 0)(1 + rng.nextInt(3)))
        taken = Some(resolution(clusters.map { case (ls, film) => decision(ls, film, ResolverDecision.Basis.OwnMatch) }*))
        // some clusters wait, and settle later — changing their film or not
        waits = clusters.collect { case (ls, _) if rng.nextInt(4) == 0 => ls.toSet[ListingKey] -> WaitingOn(0, Set(search), onTmdb = false) }.toMap
        val changes = clusters.flatMap { case (ls, film) =>
          ls.flatMap { l =>
            stored.get(l) match {
              case Some(was) if was == film => None
              case Some(was)                => Some(moved(l, was, film))
              case None                     => Some(decidedOn(l, film, joined = ls.exists(stored.contains)))
            }
          }
        }
        stored = stored -- listings.filterNot(published.contains) ++ clusters.flatMap { case (ls, film) => ls.map(_ -> film) }
        projected(changes*)
        told ++= changes.map(c => s"${c.listing.venue} ${c.kind.label} ${c.before}")
        rng.nextInt(5) match {
          case 0 => broken = true; an[IllegalStateException] should be thrownBy live.publish(); broken = false
          case 1 => live.stop(); live = publisher(flaky)                                  // a restart: it publishes on stopping
          case 2 => ()                                                                    // no publish: the next writes both
          case _ => clock.advanceSeconds(20); live.publish()
        }
      }
      live.publish()
      val written = new IdentityStaging(docs, clock).read(Map.empty).filter(_.country == "pl").flatMap(_.changed)
        .filter(_.resolution.exists(r => TakeChange.Kind.values.exists(_.label == r)))
      written.foreach(c => c.listings.size shouldBe c.listingCount)
      withClue(s"seed $seed: ")(written.flatMap(c => c.listings.map(l => s"${l.venue} ${c.resolution.get} ${c.was}")).sorted shouldBe told.sorted)
    }
  }
}
