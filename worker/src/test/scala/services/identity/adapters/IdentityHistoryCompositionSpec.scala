package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/** A harness writes no identity history, by its wiring's choice ([[IdentityHistory.Override]]), never a switch read at
 *  run time: the convergence legs' wiring names `Unwritten` and gets no writer; production names none and keeps its. */
class IdentityHistoryCompositionSpec extends AnyFlatSpec with Matchers {
  "the history" should "be the writer under no override, and none for a harness Unwritten" in {
    val writer = new InMemoryIdentityHistory
    IdentityHistory.composed(writer, Set.empty) should be theSameInstanceAs writer
    IdentityHistory.composed(writer, Set(IdentityHistory.Override.Unwritten)) should be theSameInstanceAs IdentityHistory.Discard
    IdentityHistory.Override.values.foreach(_.reason should not be empty)
  }

  "the convergence legs' wiring" should "name Unwritten, and production's wiring no override" in {
    Files.readString(Paths.get("worker/src/fixtures/scala/tools/ArchiveReplayWiring.scala")) should include ("IdentityHistory.Override.Unwritten")
    Files.readString(Paths.get("worker/src/main/scala/modules/WorkerWiring.scala")) should include (
      "protected def identityHistoryOverrides: Set[services.identity.adapters.IdentityHistory.Override] = Set.empty")
  }
}
