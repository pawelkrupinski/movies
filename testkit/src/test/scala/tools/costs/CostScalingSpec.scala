package tools.costs

import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CostScalingSpec extends AnyFlatSpec with Matchers {

  "assertLinear" should "pass a linear cost and fail a quadratic one, even one inside the budget at the small size" in {
    CostScaling.assertLinear("linear", n = 10, perUnit = 2.0)(n => 2L * n)
    val quadratic = intercept[TestFailedException](CostScaling.assertLinear("quadratic", n = 10, perUnit = 100.0)(n => n.toLong * n))
    quadratic.getMessage should include("grew 16.0x")
  }

  it should "fail a linear cost that is over its budget per unit" in {
    intercept[TestFailedException](CostScaling.assertLinear("steep", n = 10, perUnit = 1.0)(n => 3L * n))
  }

  "assertIndependent" should "fail any growth with size" in {
    CostScaling.assertIndependent("flat", n = 10)(_ => 7L)
    intercept[TestFailedException](CostScaling.assertIndependent("creeping", n = 10)(n => 7L + n / 10))
  }

  "Work.counting" should "count the rows reads return, per write the rows it names, and an index read as one" in {
    val work  = new Work
    val store = Work.counting(classOf[CostScalingSpec.Store], new CostScalingSpec.Store {
      def all()                        = Seq("a", "b", "c")
      def one(id: String)              = Some(id)
      def checked()                    = (Map("a" -> 1, "b" -> 2), true)
      def upsertAll(rows: Seq[String]) = ()
      def delete(id: String)           = ()
      def venues()                     = Set("x", "y", "z")
    }, work, indexOnly = Set("venues"))
    store.all(); store.one("a"); store.checked(); store.venues()
    store.upsertAll(Seq("a", "b")); store.delete("a")
    (work.reads, work.writes) shouldBe (3L + 1L + 2L + 1L, 2L + 1L)
    work.breakdown("all") shouldBe 3L
  }
}

object CostScalingSpec {
  trait Store {
    def all(): Seq[String]
    def one(id: String): Option[String]
    def checked(): (Map[String, Int], Boolean)
    def upsertAll(rows: Seq[String]): Unit
    def delete(id: String): Unit
    def venues(): Set[String]
  }
}
