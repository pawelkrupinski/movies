package tools

import kinowo.build.{ClassCycles, Coupling}
import kinowo.build.Coupling.{Graph, Type}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The class cycles inside one package (project/ClassCycles.scala, `scripts/class-cycles`): its strongly connected
 *  components and the edges pointing up a proposed order of its classes, over a hand-built class graph. */
class ClassCyclesSpec extends AnyFlatSpec with Matchers {

  private def t(name: String) = Type(name, "identity", isAbstract = false)
  // p.A <-> p.B; p.C <-> p.sub.D; p.A -> p.sub.D closes A, B, C, D into one; p.E alone; q.F outside the package
  private def graph(edges: (String, String)*) =
    Graph(Seq("p.A", "p.B", "p.C", "p.sub.D", "p.E", "q.F").map(t), edges.toSet)
  private val apart = graph("p.A" -> "p.B", "p.B" -> "p.A", "p.C" -> "p.sub.D", "p.sub.D" -> "p.C", "p.C" -> "p.A", "p.E" -> "q.F", "q.F" -> "p.E")
  private val knotted = graph((apart.edges.toSeq :+ ("p.A" -> "p.sub.D"))*)

  "a package's class cycles" should "be the components of the classes inside it and its sub-packages, the edges out of it left out" in {
    ClassCycles.within(apart, "p")._1 shouldBe Seq("p.A", "p.B", "p.C", "p.E", "p.sub.D")
    Coupling.components(ClassCycles.within(apart, "p")._1, ClassCycles.within(apart, "p")._2) shouldBe Seq(Seq("p.A", "p.B"), Seq("p.C", "p.sub.D"))
    Coupling.components(ClassCycles.within(knotted, "p")._1, ClassCycles.within(knotted, "p")._2) shouldBe Seq(Seq("p.A", "p.B", "p.C", "p.sub.D"))
  }

  "an order of groups" should "match a class by its full or its relative name, `*` any run of characters" in {
    val order = ClassCycles.Order.inline("A,B<C,sub.*")
    order.groups.map(_._1) shouldBe Seq("A,B", "C,sub.*")
    order.groupOf("p", "p.B") shouldBe Some("A,B")
    order.groupOf("p", "p.sub.D") shouldBe Some("C,sub.*")
    order.groupOf("p", "p.E") shouldBe None
  }

  it should "read a class -> group table, its groups in their number's order when every one is a number, else in their first row's" in {
    ClassCycles.Order.table(Seq("# tiers", "p.C\t1", "p.A\t0", "B\t0")).groups shouldBe Seq("0" -> Seq("p.A", "B"), "1" -> Seq("p.C"))
    ClassCycles.Order.table(Seq("C\trules", "A\tvocabulary")).groups.map(_._1) shouldBe Seq("rules", "vocabulary")
    an[IllegalArgumentException] should be thrownBy ClassCycles.Order.table(Seq("p.A"))
  }

  "the upward edges" should "be every class edge from a lower group to a higher one, with its class references" in {
    val order = ClassCycles.Order.inline("A,B<C,sub.*")
    ClassCycles.upward(apart, "p", order) shouldBe empty
    ClassCycles.upward(knotted, "p", order) shouldBe Seq(("A,B", "C,sub.*") -> Seq("p.A" -> "p.sub.D"))
  }

  "the report" should "list the components, the groups and the upward edges" in {
    val report = ClassCycles.report(knotted, "p", Some(ClassCycles.Order.inline("A,B<C,sub.*")))
    report should include("## Strongly connected components of 2+ classes (1; largest 4)")
    report should include("- 4 classes: A B C sub.D")
    report should include("- group `A,B` (2): A B")
    report should include("- outside the order (1): E")
    report should include("### `A,B` -> `C,sub.*`: 1 references\n- A -> sub.D")
  }
}
