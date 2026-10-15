package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The draft bench's arguments: what it reads, and the inputs it used to run past. */
class IdentityDraftBenchSpec extends AnyFlatSpec with Matchers {

  import IdentityDraftBench.{Options, parse}

  "The draft bench" should "read its ticks, re-reads, histogram directory, words and named counts in any order" in {
    val dir = Files.createTempDirectory("draft-bench")
    try {
      parse(Seq("8", "300", dir.toString, "adopt", "churn=4")) shouldBe Right(Options(ticks = 8, histograms = Some(dir), adopt = true, churn = Some(4)))
      parse(Seq("whole", "2", "300", "-", "shifts=10", "decoded")) shouldBe Right(Options(ticks = 2, whole = true, decoded = true, shifts = 10))
      parse(Nil) shouldBe Right(Options())
    } finally Files.delete(dir)
  }

  it should "refuse what it cannot read rather than run on a default" in {
    // the old reading took a mistyped `adpot` for the histogram directory, and `lots` re-reads as 300
    parse(Seq("8", "300", "-", "adpot")).isLeft shouldBe true
    parse(Seq("8", "300", "adpot")).isLeft shouldBe true
    parse(Seq("8", "lots")).isLeft shouldBe true
    parse(Seq("eight")).isLeft shouldBe true
    parse(Seq("shift=10")).isLeft shouldBe true
    parse(Seq("churn=many")).isLeft shouldBe true
  }
}
