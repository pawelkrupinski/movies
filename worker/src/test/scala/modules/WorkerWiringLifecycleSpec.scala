package modules

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{ObjectGraph, TestWiring, ThreadLeaks, UnmanagedMembers}

/** `stop()` shuts everything with a lifecycle the worker wiring built — every reaper, census, schedule,
 *  service and pool, each registered with its [[tools.ManagedResources]] where it is created — and leaves
 *  no thread of its own running. */
class WorkerWiringLifecycleSpec extends AnyFlatSpec with Matchers {

  /** Members with a lifecycle the wiring does not own, and why. */
  private val OwnedElsewhere: Map[String, String] = Map.empty

  "Every member with a lifecycle the worker wiring builds" should "be registered, so stop() shuts it" in {
    val wiring = new OfflineWorkerWiring(Country.Poland)
    try {
      wiring.forceBoot() shouldBe empty
      val unmanaged = UnmanagedMembers.of(wiring, wiring.managedResources).filterNot(m => OwnedElsewhere.keySet.exists(k => m.startsWith(s"$k ")))
      withClue("wrap its creation in managedResources.stopping / stoppingEach / executor / register:\n")(unmanaged shouldBe empty)
      // Positive control: the walk does see the members it checks.
      wiring.managedResources.holds(wiring.scrapeReaper) shouldBe true
      wiring.managedResources.open should be > 20
    } finally wiring.stop()
  }

  private class PooledWiring extends TestWiring {
    /** Build the pools a running worker builds as it goes, so the stop has them to shut. */
    def buildPools(): Unit = { identityPrefetchPool; identityModelScheduler; shadowLookupExecutor; () }
  }

  "A started and stopped worker wiring" should "shut every pool it registered and leave no thread of its own" in {
    val before = ThreadLeaks.live()
    val wiring = new PooledWiring
    wiring.start()
    wiring.buildPools()
    wiring.managedResources.open should be >= 3
    wiring.stop()
    wiring.managedResources.open shouldBe 0
    wiring.managedResources.unterminated shouldBe empty
    ThreadLeaks.survivors(before) shouldBe empty
  }

  "A pool built after the stop" should "be shut at once" in {
    val wiring = new PooledWiring
    wiring.stop()
    wiring.buildPools()
    wiring.managedResources.open shouldBe 0
    wiring.managedResources.unterminated shouldBe empty
  }

  "The member walk" should "name an unregistered member, and not a registered one" in {
    val managed = new tools.ManagedResources
    final class Root { val loose = new services.Stoppable { def stop(): Unit = () }; val held = managed.stopping(new services.Stoppable { def stop(): Unit = () }) }
    UnmanagedMembers.of(new Root, managed).map(_.takeWhile(_ != ' ')) shouldBe Seq("loose")
    ObjectGraph.collect(new Root) { case s: services.Stoppable => s }.size shouldBe 2
  }
}
