package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringPoolSpec extends AnyFlatSpec with Matchers {

  "canonical" should "return the SAME instance for byte-identical strings (interning)" in {
    val a = new String("A long editorial blurb that repeats across every venue in town.")
    val b = new String("A long editorial blurb that repeats across every venue in town.")
    (a eq b) shouldBe false                       // distinct instances, equal content
    val ca = StringPool.canonical(a)
    val cb = StringPool.canonical(b)
    (ca eq cb) shouldBe true                       // interned to one shared object
    ca shouldBe a                                  // content preserved
  }

  it should "keep distinct strings distinct" in {
    val one = StringPool.canonical("the first film's synopsis")
    val two = StringPool.canonical("a wholly different film's synopsis")
    one should not be theSameInstanceAs(two)
    one shouldBe "the first film's synopsis"
  }

  "canonicalAll" should "intern every element so equal list members share one instance" in {
    // Two films' cast lists that share a country/actor token — interned to one object each.
    val listA = StringPool.canonicalAll(Seq(new String("Poland"), new String("Cate Blanchett")))
    val listB = StringPool.canonicalAll(Seq(new String("Poland"), new String("Cate Blanchett")))
    (listA(0) eq listB(0)) shouldBe true
    (listA(1) eq listB(1)) shouldBe true
    listA shouldBe Seq("Poland", "Cate Blanchett")   // order + content preserved
    StringPool.canonicalAll(Seq.empty) shouldBe empty
  }
}
