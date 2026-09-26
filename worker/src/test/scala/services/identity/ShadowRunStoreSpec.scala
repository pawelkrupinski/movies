package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import tools.MutableClock

import java.time.Instant
import scala.concurrent.duration._

/** The shadow runs' rules over the in-memory backend (`MongoShadowRunStoreIntegrationSpec` runs the
 *  same behaviour over Mongo), and the document shape both collections store. */
class ShadowRunStoreSpec extends AnyFlatSpec with Matchers with ShadowRunStoreBehaviour {

  "the in-memory shadow run store" should behave like shadowRunStore(clock => ShadowRunStore.inMemory(clock))

  "a shadow cluster and family" should "survive their stored document shape whole" in {
    val (cluster, family) = ShadowRunStoreBehaviour.sample
    MongoShadowRunBackend.decodeCluster(MongoShadowRunBackend.encodeCluster(cluster)) shouldBe cluster
    MongoShadowRunBackend.decodeFamily(MongoShadowRunBackend.encodeFamily(family)) shouldBe family
  }
}

/** What every [[ShadowRunBackend]] must do under [[ShadowRunStore]]'s rules. */
trait ShadowRunStoreBehaviour { this: AnyFlatSpec & Matchers =>
  import ShadowRunStoreBehaviour._

  def shadowRunStore(make: java.time.Clock => ShadowRunStore): Unit = {

    it should "read nothing before a run is recorded" in {
      val store = make(new MutableClock(T0))
      store.latestRun() shouldBe None
      store.latest() shouldBe Nil
      store.verdicts() shouldBe Nil
    }

    it should "serve the latest run's decisions and verdicts, a later run replacing the earlier whole" in {
      val clock = new MutableClock(T0)
      val store = make(clock)
      val (cluster, family) = sample
      store.record(ShadowRun(T0, Seq(cluster, cluster.copy(relation = Some(ShadowRelation.Split))), Seq(family)), Retention)
      store.latest() shouldBe Seq(cluster.decision, cluster.decision)
      store.verdicts() shouldBe Seq(ConfidenceCalibration.Sample(cluster.decision.confidence, correct = true))
      store.latestRun().map(_.families) shouldBe Some(Seq(family))

      clock.advance(java.time.Duration.ofHours(1))
      val moved = cluster.copy(relation = Some(ShadowRelation.Moved))
      store.record(ShadowRun(clock.instant(), Seq(moved), Nil), Retention)
      store.latestRun() shouldBe Some(ShadowRun(clock.instant(), Seq(moved), Nil))
      store.verdicts() shouldBe Seq(ConfidenceCalibration.Sample(cluster.decision.confidence, correct = false))
    }

    it should "stop serving a run once its retention has passed since it ran" in {
      val clock = new MutableClock(T0)
      val store = make(clock)
      store.record(ShadowRun(T0, Seq(sample._1), Nil), Retention)
      clock.advance(java.time.Duration.ofMillis((Retention.value - 1.second).toMillis))
      store.latest() should have size 1
      clock.advance(java.time.Duration.ofSeconds(1))
      store.latestRun() shouldBe None
      store.latest() shouldBe Nil
    }
  }
}

object ShadowRunStoreBehaviour {
  val T0: Instant = Instant.parse("2026-09-26T10:00:00Z")
  val Retention: ShadowRetention = ShadowRetention(8.days)

  val sample: (ShadowCluster, ShadowFamily) = {
    val native    = ListingKey.Native("Kino Muza", "https://kinomuza.pl/film/lalka", "Lalka (2026)")
    val published = ListingKey.Published("Kino Amok", "Lalka", Some(2026), Seq("Maciej Kawalski"))
    val bare      = ListingKey.Published("Kino Pod Baranami", "Lalka", None, Nil)
    val decision  = ResolverDecision(Seq(native, published), Some(1321666), 0.87, ResolverDecision.Basis.PooledMatch,
      Seq("'Lalka' [2026] {Maciej Kawalski} ×1 → tmdb 1321666"), Seq("cannot-link held 'Lalka' ×1 apart"))
    val film = PipelineFilmRef("f-lalka", Some(1321666))
    (ShadowCluster(decision, 7, Some(ShadowRelation.Identical), Seq(film)),
      ShadowFamily(7, Seq(Some(1321666) -> Seq(native, published), None -> Seq(bare)),
        Seq(film -> Seq(native, published), PipelineFilmRef("f-lalka-1968", None) -> Seq(bare)), Seq(bare),
        Map(ShadowRelation.Identical -> 1, ShadowRelation.Split -> 1)))
  }
}
