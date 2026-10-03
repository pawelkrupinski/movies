package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{ObjectGraph, ThreadLeaks, UnmanagedMembers}

/** `stop()` shuts every pool and closeable the serving wiring built — through its [[tools.ManagedResources]] —
 *  and leaves no thread of its own running. */
class WebWiringLifecycleSpec extends AnyFlatSpec with Matchers {

  "Every member with a lifecycle the web wiring builds" should "be registered, so stop() shuts it" in {
    val wiring = new TestWebWiring()
    try {
      wiring.boot()
      ObjectGraph.forceLazyMembers(wiring) shouldBe empty
      withClue("wrap its creation in managedResources.stopping / stoppingEach / executor / register:\n")(
        UnmanagedMembers.of(wiring, wiring.managedResources) shouldBe empty)
      wiring.managedResources.holds(wiring.webReadModel) shouldBe true
    } finally wiring.shutdown()
  }

  "A booted and stopped web wiring" should "shut every pool it registered and leave no thread of its own" in {
    val before = ThreadLeaks.live()
    val wiring = new TestWebWiring()
    wiring.boot()
    Seq(wiring.movieController, wiring.pageRefreshExecutor)
    wiring.managedResources.open should be >= 1
    wiring.shutdown()
    wiring.managedResources.open shouldBe 0
    wiring.managedResources.unterminated shouldBe empty
    ThreadLeaks.survivors(before) shouldBe empty
  }
}
