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
}
