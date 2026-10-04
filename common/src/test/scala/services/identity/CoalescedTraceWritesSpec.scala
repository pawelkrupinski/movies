package services.identity

import tools.SpecTimeouts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.collection.mutable
import scala.util.Random

/** A trace store's writer, slow while the model keeps updating: what waits is one hand-over per family, never one per
 *  update — and what lands is what writing every hand-over in turn would have left. */
class CoalescedTraceWritesSpec extends AnyFlatSpec with Matchers {

  private def trace(listing: Int, family: String, rule: String) =
    ListingTrace(ListingKey.Published("Kino Muza", s"Film $listing", None, Nil), family, None, "OwnMatch", Seq(rule), None)

  /** Writes into `held` as `identity_traces` does: drop the removed families' documents, then upsert by listing. */
  private final class Collection {
    val held = mutable.LinkedHashMap.empty[ListingKey, ListingTrace]
    def write(removed: Set[String], added: Iterator[ListingTrace]): Unit = synchronized {
      held.filterInPlace((_, t) => !removed(t.family)); added.foreach(t => held(t.listing) = t)
    }
  }

  "a slow store's pending writes" should "hold one hand-over per family, however many updates re-resolve it" in {
    val started = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val collection = new Collection
    val writes = new CoalescedTraceWrites((removed, added) => { started.countDown(); release.await(); collection.write(removed, added) },
      (_, e) => throw e)
    writes.replace(Set.empty, FamilyTraces.of(Seq(trace(0, "f0", "first"))))
    started.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
    (1 to 1000).foreach { update =>
      val family = s"f${update % 10}"
      writes.replace(Set(family), FamilyTraces.of(Seq(trace(update % 10, family, s"update $update"))))
    }
    withClue("hand-overs waiting behind the blocked write: ")(writes.waiting shouldBe 10)
    release.countDown()
    writes.flush()
    collection.held.values.map(t => t.family -> t.rules).toMap shouldBe (991 to 1000).map(u => s"f${u % 10}" -> Seq(s"update $u")).toMap
  }

  they should "land what writing every hand-over in turn would, whenever the writer catches up" in {
    (1 to 40).foreach { seed =>
      val random     = new Random(seed)
      val coalesced  = new Collection
      val inTurn     = new Collection
      val writes     = new CoalescedTraceWrites((removed, added) => { Thread.sleep(seed % 2L); coalesced.write(removed, added) },
        (_, e) => throw e)
      (1 to 60).foreach { update =>
        val added   = Seq.fill(random.nextInt(3))(random.nextInt(8)).distinct.map { family =>
          val traces = (0 to random.nextInt(3)).map(i => trace(family * 10 + i, s"f$family", s"u$update"))
          FamilyTraces(s"f$family", () => traces)
        }
        // as the model hands them over: a family handed over again was re-resolved, so its earlier traces are removed
        val removed = Seq.fill(random.nextInt(3))(s"f${random.nextInt(8)}").toSet ++ added.map(_.family)
        writes.replace(removed, added)
        inTurn.write(removed, added.iterator.flatMap(_.build()))
        if (random.nextInt(10) == 0) writes.flush()
      }
      writes.flush()
      withClue(s"seed $seed: ")(coalesced.held.toMap shouldBe inTurn.held.toMap)
    }
  }

  they should "be dropped on close, and nothing handed over after it be built or written" in {
    val release    = new CountDownLatch(1)
    val collection = new Collection
    val writes     = new CoalescedTraceWrites((removed, added) => { release.await(); collection.write(removed, added) }, (_, _) => ())
    writes.replace(Set.empty, FamilyTraces.of(Seq(trace(0, "f0", "first"))))
    writes.replace(Set.empty, FamilyTraces.of(Seq(trace(1, "f1", "queued"))))
    writes.close()
    var built = false
    writes.replace(Set.empty, Seq(FamilyTraces("f2", () => { built = true; Nil })))
    release.countDown()
    writes.flush()
    writes.waiting shouldBe 0
    built shouldBe false
    collection.held.values.map(_.family).toSet should not contain "f1"
  }
}
