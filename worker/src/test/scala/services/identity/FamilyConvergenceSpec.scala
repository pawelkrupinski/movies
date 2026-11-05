package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** The per-family convergence check ([[FamilyConvergence]]) over committed slices of known flips: it finds them by the
 *  lattice -- Lichtblick "Buddies", a final-take flip the drift seeds needed luck for, and Quad Derby's "Oxide Ghosts" --
 *  and a second run over its cache walks nothing again, unless a read of a family's closure moved. */
class FamilyConvergenceSpec extends AnyFlatSpec with Matchers {
  private val slices  = Path.of("test", "resources", "fixtures", "drift-slices")
  private val buddies = slices.resolve("de/programmkino-lichtblick-e-v-buddies-69181215")
  private val oxide   = slices.resolve("uk/quad-derby-oxide-ghosts-the-brass-eye-tapes-q-a-18--7c64f181")
  private val options = FamilyConvergence.Options(threads = 2)

  private lazy val first = FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), Map.empty, options)

  "The per-family check" should "find Lichtblick's Buddies flip in the final take, which the model alone does not take" in {
    first.walked shouldBe first.verdicts.size
    val flips = first.flips.map(_._2)
    flips.filter(f => f.stage == "final" && f.listing.startsWith("Programmkino Lichtblick")) should not be empty
  }

  it should "find Quad Derby's Oxide Ghosts flip" in {
    val run = FamilyConvergence.check(FamilyConvergence.sliceCorpus(oxide, "rules-1"), Map.empty, options)
    run.flips.map(_._2.listing).exists(_.startsWith("Quad Derby")) shouldBe true
  }

  it should "reuse every cached verdict whose key and closure stand, and walk again one whose closure moved or whose rules did" in {
    val cache = first.verdicts.map(v => v.key -> v).toMap
    val again = FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), cache, options)
    again.walked shouldBe 0
    again.verdicts.map(_.flips) shouldBe first.verdicts.map(_.flips)
    // a closure read recorded otherwise than it reads now: that family is walked again
    val walkedOnce = first.verdicts.head
    val moved = cache.updated(walkedOnce.key, walkedOnce.copy(closure = FamilyConvergence.Read("context", "spread|buddies", 0L) +: walkedOnce.closure))
    FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), moved, options).walked shouldBe 1
    FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-2"), cache, options).walked shouldBe first.verdicts.size
  }

  "A cache" should "read back what it wrote" in {
    val file = java.nio.file.Files.createTempFile("family-convergence", ".json")
    try {
      FamilyConvergence.save(file, first.verdicts)
      FamilyConvergence.load(file).values.toSeq.sortBy(_.family) shouldBe first.verdicts.sortBy(_.family)
    } finally java.nio.file.Files.deleteIfExists(file)
  }
}
