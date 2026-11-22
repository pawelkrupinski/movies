package tools

import kinowo.build.{ClassReferences, SameFileEdges}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The test-impact selection's same-file edges (project/SameFileEdges.scala) over the fixture's class files
 *  (tools.impactfixture, compiled into this test configuration): Zinc's record, hand-written as it records the
 *  fixture, misses the concrete parts of an abstract check declared in its file; the class files add them. */
class SameFileEdgesSpec extends AnyFlatSpec with Matchers {

  private val pkg = "tools.impactfixture"
  private def n(simple: String) = s"$pkg.$simple"
  private lazy val compiled: Seq[ClassReferences.Compiled] = {
    val dir = java.nio.file.Paths.get(getClass.getResource("/" + pkg.replace('.', '/')).toURI)
    Files.list(dir).iterator.asScala.toSeq.filter(_.toString.endsWith(".class")).map(p => ClassReferences.read(Files.readAllBytes(p)))
  }
  // Walk.scala's Zinc class names and their binary names (a class and its object share one class name)
  private val names = Set("Walk", "WalkPart1", "WalkPart2", "Unrelated", "Walk$.Row").map(n)
  private val binaries = Map(n("Walk") -> Set(n("Walk"), n("Walk$")), n("Walk$.Row") -> Set(n("Walk$Row")),
    n("WalkPart1") -> Set(n("WalkPart1")), n("WalkPart2") -> Set(n("WalkPart2")), n("Unrelated") -> Set(n("Unrelated")))
  // what Zinc records of the fixture, dependency -> dependent: Walk's (its object's) use of Relation, and nothing between
  // the classes of Walk.scala
  private val zinc = Set(n("Relation") -> n("Walk"))
  private lazy val sameFile = SameFileEdges.of(names, binaries)(compiled)
  private def affected(edges: Set[(String, String)]): Set[String] =
    SameFileEdges.closure(Set(n("Relation")), c => edges.collect { case (`c`, dependent) => dependent })

  "a change to a class an abstract check's file uses" should "miss the check's concrete parts on Zinc's record alone" in {
    affected(zinc) shouldBe Set(n("Relation"), n("Walk"))
  }

  it should "reach every concrete part of its file through the class files, and leave out a class using none of them" in {
    affected(zinc ++ sameFile) shouldBe Set("Relation", "Walk", "WalkPart1", "WalkPart2", "Walk$.Row").map(n)
  }

  "the same-file edges" should "make each subclass depend on its base, and a nested class and its enclosing one on each other" in {
    sameFile should contain allOf (n("Walk") -> n("WalkPart1"), n("Walk") -> n("WalkPart2"))
    sameFile should contain allOf (n("Walk") -> n("Walk$.Row"), n("Walk$.Row") -> n("Walk"))
    sameFile.filter { case (from, to) => from == n("Unrelated") || to == n("Unrelated") } shouldBe empty
    sameFile.filter { case (from, to) => from == to } shouldBe empty
  }

  "a file of one top-level class" should "read no class file" in {
    SameFileEdges.of(Set(n("Relation")), Map(n("Relation") -> Set(n("Relation$"))))(fail("read a class file")) shouldBe empty
  }
}
