package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{ShadowLookupRound, ShadowTick}
import services.observations.ObservationStore
import tools._

import java.time.{Clock, ZoneOffset}
import scala.collection.mutable
import scala.util.Try

/**
 * The shadow run's paced live lookup fill, as PRODUCTION wires it (`WorkerWiring.shadowLookupFill`,
 * docs/design/identity-resolver.md §17), on the recorded corpora — the replay standing in for the
 * live services. The pipeline boots with the capture on, so the store starts as production's would:
 * holding only the pipeline's own lookups. Then shadow ticks and fill rounds alternate, as the
 * settle tick runs them, at the default cap, until a round finds nothing left to ask.
 *
 * It requires every round to stay within its allowance, no request during a shadow tick, and the
 * gaps to fall to what the services themselves cannot answer; and it reports how many rounds that
 * took and what it asked — the rollout's request-volume numbers.
 *
 * The hard clusters always run (itAll); the full corpora with `KINOWO_IDENTITY_FULL`, as for
 * `IdentityShadowIntegrationSpec`.
 */
class ShadowLookupFillIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  import IdentityShadow._

  private val fixtureRoot = configuration.fixtureRoot
  private val storages    = mutable.ListBuffer.empty[ConvergenceStorage]
  private val corpora: Seq[Corpus] =
    hardClusters(configuration.hardClusterCountries.map(_.value.map(_.code))) ++
      configuration.identityCorpusDirectory.toSeq.flatMap(d => IdentityShadow.full(configuration.identityFullCorpora.value, d.value, fixtureRoot))
  private val MaxRounds = 48

  override def afterAll(): Unit = { storages.synchronized(storages.foreach(s => Try(s.close()))); super.afterAll() }

  corpora.foreach { c =>
    "the shadow run's live lookup fill" should s"close the shadow's gaps within its cap, from the pipeline's capture alone, on ${c.label}" in {
      val observations = ObservationStore.inMemory(Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC))
      val storage      = ConvergenceStorage.mongo(mongoTarget, s"shadow-fill-${c.label}", c.normalizer)
      storages.synchronized(storages += storage)
      val w = FetchReplayWiring(c.country, storage, c.rows, c.fetch, fixtureRoot, retrySleep = (_: Long) => (),
        // The suite's own environment (as `IdentityShadow.wiring` passes it), with both switches on.
        environment = new Env(key => Map("KINOWO_IDENTITY_SHADOW" -> "true", "KINOWO_IDENTITY_SHADOW_LOOKUPS" -> "true").get(key)
          .orElse(configuration.env.get(key))),
        observations = Some(observations))
      bootPipeline(w)
      val reaper = w.shadowIdentityReaper.getOrElse(fail("the shadow run is not wired"))
      val fill   = w.shadowLookupFill.getOrElse(fail("the lookup fill is not wired"))
      val allowance = fill.effectiveRate.allowanceOver(w.identityShadowInterval.value)

      def tick(): ShadowTick = {
        val before = c.fetch.requests.get()
        val t = reaper.tick()
        withClue(s"[${c.label}] a shadow tick made a request: ") { c.fetch.requests.get() shouldBe before }
        t
      }
      val ticks  = mutable.ArrayBuffer(tick())
      val rounds = mutable.ArrayBuffer.empty[ShadowLookupRound]
      while (rounds.size < MaxRounds && rounds.lastOption.forall(_.asked > 0)) {
        val before = c.fetch.requests.get()
        val r = fill.round()
        rounds += r
        withClue(s"[${c.label}] round ${rounds.size}: ") {
          r.asked should be <= allowance
          (c.fetch.requests.get() - before) should be >= r.asked.toLong
        }
        ticks += tick()
      }

      def matched(t: ShadowTick) = t.run.fold(0)(_.clusters.count(_.decision.film.isDefined))
      val asked = rounds.map(_.asked).sum
      val converged = rounds.indexWhere(_.deferred == 0) + 1
      info(s"[${c.label}] ${ticks.head.listings} listings; rate ${fill.effectiveRate.perMinute}/min, allowance $allowance a round. " +
        s"Gaps per tick: ${ticks.map(_.gaps).mkString(" → ")}; matched clusters ${matched(ticks.head)} → ${matched(ticks.last)} of " +
        s"${ticks.last.run.fold(0)(_.clusters.size)}. Rounds: ${rounds.size} (nothing deferred from round " +
        s"${if (converged == 0) "—" else converged.toString}); asked ${rounds.map(_.asked).mkString("+")} = $asked " +
        s"(${rounds.map(_.failed).sum} failed); ${"%.2f".format(asked.toDouble / ticks.head.listings.max(1))} asks per listing")

      withClue(s"[${c.label}] ") {
        rounds.size should be < MaxRounds
        rounds.last.asked shouldBe 0
        ticks.last.gaps should be < ticks.head.gaps
      }
    }
  }
}
