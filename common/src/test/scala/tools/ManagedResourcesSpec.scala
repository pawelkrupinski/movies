package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class ManagedResourcesSpec extends AnyFlatSpec with Matchers {

  "closeAll" should "close newest first, and close the rest when one fails" in {
    val closed  = ListBuffer.empty[String]
    val managed = new ManagedResources
    managed.register("a", "a")(closed += _)
    managed.register("b", "b")(_ => throw new IllegalStateException("b refuses"))
    managed.register("c", "c")(closed += _)
    managed.closeAll()
    closed.toList shouldBe List("c", "a")
    managed.open shouldBe 0
  }

  it should "shut a registered executor, so nothing it ran is left running" in {
    val managed = new ManagedResources
    val pool    = managed.executor("pool")(DaemonExecutors.scheduler("managed-spec"))
    pool.execute(() => ())
    managed.closeAll()
    pool.isTerminated shouldBe true
    managed.unterminated shouldBe empty
  }

  "A resource registered once the stop has begun" should "be closed at once" in {
    val closed  = ListBuffer.empty[String]
    val managed = new ManagedResources
    managed.closeAll()
    managed.register("late", "late")(closed += _)
    closed.toList shouldBe List("late")
  }
}
