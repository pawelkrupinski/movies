package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import _root_.tools.CommandLine

/** The live venue measures read the forms their clients' specs name, and refuse a flag or venue they do not know. */
class VenueMeasureSpec extends AnyFlatSpec with Matchers {

  private def run(line: CommandLine, runOf: CommandLine.Arguments => VenueMeasure.Run, args: String*) =
    runOf(line.parse(args).fold(stop => fail(s"$stop"), identity))

  "WriteSpektrix" should "record the venue named, and measure every one when none is" in {
    run(WriteSpektrix.commandLine, WriteSpektrix.runOf, "--record", "dundeeca") shouldBe VenueMeasure.Run(record = true, Set("dundeeca"))
    run(WriteSpektrix.commandLine, WriteSpektrix.runOf, "--record", "brewhousetaunton").wants("ica") shouldBe false
    run(WriteSpektrix.commandLine, WriteSpektrix.runOf).wants("ica") shouldBe true
  }

  "WriteSavoy" should "name its venues by cinema" in {
    run(WriteSavoy.commandLine, WriteSavoy.runOf, "--record", "Theatr Gwaun Fishguard") shouldBe
      VenueMeasure.Run(record = true, Set("Theatr Gwaun Fishguard"))
  }

  "WriteTicketsolve" should "re-record every feed with a bare --record" in {
    run(WriteTicketsolve.commandLine, WriteTicketsolve.runOf, "--record") shouldBe VenueMeasure.Run(record = true, Set.empty)
  }

  "Every venue measure" should "refuse a mistyped flag or venue rather than measure nothing" in {
    // args.filterNot(_ == "--record") read both as a venue no entry matched: the run measured nothing, silently
    Seq[(CommandLine, CommandLine.Arguments => VenueMeasure.Run)]((WriteTicketsolve.commandLine, WriteTicketsolve.runOf),
      (WriteSpektrix.commandLine, WriteSpektrix.runOf), (WriteSavoy.commandLine, WriteSavoy.runOf)).foreach { case (line, runOf) =>
      line.parse(Seq("--recrod")).isLeft shouldBe true
      an[CommandLine.InvalidArguments] should be thrownBy run(line, runOf, "--record", "nosuchvenue")
    }
  }
}
