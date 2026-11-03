package services.identity

import org.scalatest.Suite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

/**
 * THE REAL-FAMILY SPECS' PARTS ARE THE WHOLE SPEC ([[SliceParts]]): for each of [[TitleRelationSpec]],
 * [[ReadSetClosureSpec]] and [[PopulationMonotonicitySpec]], its [[SliceParts.Count]] part classes together register
 * every case the whole spec does, each in exactly one part -- so splitting them across the identity shards runs every
 * case exactly once (shard_tests.py's own test proves each class runs in exactly one shard). Building a spec registers
 * its cases and resolves nothing: this spec takes a second.
 */
class RealFamilyPartsSpec extends AnyFlatSpec with Matchers {

  private val families: Seq[(String, () => Suite, Seq[() => Suite])] = Seq(
    ("TitleRelation", () => new TitleRelationSpec(SliceParts.All) {},
      Seq(() => new TitleRelationPart1Spec, () => new TitleRelationPart2Spec, () => new TitleRelationPart3Spec, () => new TitleRelationPart4Spec)),
    ("ReadSetClosure", () => new ReadSetClosureSpec(SliceParts.All) {},
      Seq(() => new ReadSetClosurePart1Spec, () => new ReadSetClosurePart2Spec, () => new ReadSetClosurePart3Spec, () => new ReadSetClosurePart4Spec)),
    ("PopulationMonotonicity", () => new PopulationMonotonicitySpec(SliceParts.All) {},
      Seq(() => new PopulationMonotonicityPart1Spec, () => new PopulationMonotonicityPart2Spec, () => new PopulationMonotonicityPart3Spec,
        () => new PopulationMonotonicityPart4Spec)))

  // the part-local check of PopulationMonotonicity's: one per part holding a flipping slice, its slices compared below
  private def partLocal(name: String) = name.startsWith("The population walk on several threads should find what one thread finds")

  "The slices" should "fall into every part, and nowhere else" in {
    val dirs = PopulationMonotonicitySpec.sliceDirs
    dirs.map(SliceParts.partOf).toSet shouldBe (1 to SliceParts.Count).toSet
    (1 to SliceParts.Count).flatMap(SliceParts.of(_, dirs)).sortBy(_.toString) shouldBe dirs.sortBy(_.toString)
  }

  families.foreach { case (name, whole, parts) =>
    s"$name's parts" should s"be ${SliceParts.Count} classes, Part1 to Part${SliceParts.Count}" in {
      parts.map(_().getClass.getName) shouldBe (1 to SliceParts.Count).map(k => s"services.identity.${name}Part${k}Spec")
      Try(Class.forName(s"services.identity.${name}Part${SliceParts.Count + 1}Spec")).isFailure shouldBe true
    }

    it should "register every case of the whole spec, each in exactly one part" in {
      val ofParts = parts.flatMap(_().testNames.toSeq).filterNot(partLocal)
      val all     = whole().testNames.toSeq.filterNot(partLocal)
      ofParts.diff(ofParts.distinct) shouldBe empty
      ofParts.sorted shouldBe all.sorted
      all.size should be > PopulationMonotonicitySpec.sliceDirs.size // a case per slice at least: not vacuous
    }
  }

  "PopulationMonotonicity's walk on several threads" should "compare each flipping slice in exactly one part" in {
    val whole = new PopulationMonotonicitySpec(SliceParts.All) {}
    val parts = Seq(new PopulationMonotonicityPart1Spec, new PopulationMonotonicityPart2Spec, new PopulationMonotonicityPart3Spec,
      new PopulationMonotonicityPart4Spec)
    whole.flippingHere should not be empty
    parts.flatMap(_.flippingHere).sortBy(_.toString) shouldBe whole.flippingHere.sortBy(_.toString)
    parts.foreach(p => p.testNames.exists(partLocal) shouldBe p.flippingHere.nonEmpty)
  }
}
