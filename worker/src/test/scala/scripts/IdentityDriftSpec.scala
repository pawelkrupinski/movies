package scripts

import org.bson.{BsonArray, BsonDocument, BsonInt64}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.{FamilyAnswerStore, InMemoryTmdbDocuments, TmdbKind, TmdbStore}
import services.identity.agreement.VoterFamily
import tools.MutableClock

import java.time.{Duration, Instant}

/** The drift replay's queue asks what production's asker asks: a family question whose answer was filed and is fresh is
 *  not asked of the recording's sites again — read again, the recording's answer replaced the one filed, which the whole
 *  resolve reads (PL "LALKA / DOLLY", seed 2, run 3154). */
class IdentityDriftSpec extends AnyFlatSpec with Matchers {
  private val question = VoterFamily.RottenTomatoes -> "title|Lalka Dolly"
  private val id       = FamilyAnswerStore.questionId(question._1, question._2)

  /** prod's filed answer to `question`, held back by a gate that holds every question (seed 2's held it). */
  private def filed(clock: MutableClock) = {
    val inner = new InMemoryTmdbDocuments
    inner.put(TmdbKind.Family, Seq(id -> new BsonDocument("hits", new BsonArray()).append(TmdbStore.FetchedAt, BsonInt64(clock.millis()))))
    val docs = new IdentityDrift.GatedDocuments(inner, new IdentityDrift.Share(2, 100))
    docs -> new FamilyAnswerStore(docs, clock)
  }

  "asks" should "release a held answer rather than ask for it" in {
    val clock         = new MutableClock(Instant.parse("2026-10-20T13:26:32Z"))
    val (docs, store) = filed(clock)
    IdentityDrift.asks(store, Seq(question), id => docs.held(id) && { docs.release(id); true }) shouldBe empty
    store.wanted(id) shouldBe false
  }

  it should "not ask again a gap a wait still lists once its answer was filed, the gate opened first (seed 2's order)" in {
    val clock         = new MutableClock(Instant.parse("2026-10-20T13:26:32Z"))
    val (docs, store) = filed(clock)
    // the cluster met the question while its answer was held: a gap its wait lists
    store.wanted(id) shouldBe true
    // every answer filed, then the wait's gaps handed to the queue again
    docs.openAll(store.noteFiled)
    IdentityDrift.asks(store, Seq(question), _ => false) shouldBe empty
  }

  it should "ask a question not filed, and one filed longer ago than its kind keeps it" in {
    val clock         = new MutableClock(Instant.parse("2026-10-20T13:26:32Z"))
    val (docs, store) = filed(clock)
    docs.openAll(store.noteFiled)
    val unfiled = VoterFamily.RottenTomatoes -> "title|Halloween '26: Lalka ( Dolly)"
    IdentityDrift.asks(store, Seq(question, unfiled), _ => false) shouldBe Seq(unfiled)
    clock.advance(Duration.ofDays(91))
    IdentityDrift.asks(store, Seq(question, unfiled), _ => false) shouldBe Seq(question, unfiled)
  }

  "a listing's inputs" should "move with its model cluster's members, and at a step that moved listings with the model's take of it" in {
    val lalka = IdentityDrift.ModelCluster(members = 17, film = "tmdb:1321666")
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka.copy(members = 18)), listingsMoved = false) shouldBe true
    // PL Kino Eva's "LALKA", run 3509: new venues' listings came, its cluster kept its members, the model took it as none
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka.copy(film = "none")), listingsMoved = true) shouldBe true
    // a deploy under the same rules moved nothing: the model's take moving there is a flip of the model's own
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka.copy(film = "none")), listingsMoved = false) shouldBe false
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka), listingsMoved = true) shouldBe false
    IdentityDrift.inputsMoved(None, Some(lalka), listingsMoved = false) shouldBe true
    // a listing no model cluster held at either step: nothing to tell its inputs apart by, read as moved as the members'
    // hash alone read it — the model take's clause only ever removes a flag
    IdentityDrift.inputsMoved(None, None, listingsMoved = false) shouldBe true
  }

  "the reviewed flips" should "stay empty: a flip the leg finds is fixed, never listed (the owner, 2026-10-27)" in {
    val path = "test/resources/fixtures/identity-flips-allow.tsv"
    deploy.RepoFile.exists(path) shouldBe true
    IdentityDrift.flipAllowList(java.nio.file.Path.of(path)) shouldBe empty
  }
}
