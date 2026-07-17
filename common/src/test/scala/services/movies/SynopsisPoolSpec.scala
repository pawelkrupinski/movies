package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SynopsisPoolSpec extends AnyFlatSpec with Matchers {

  "canonical" should "return the SAME instance for byte-identical synopses (interning)" in {
    val a = new String("A long editorial blurb that repeats across every venue in town.")
    val b = new String("A long editorial blurb that repeats across every venue in town.")
    (a eq b) shouldBe false                       // distinct instances, equal content
    val ca = SynopsisPool.canonical(a)
    val cb = SynopsisPool.canonical(b)
    (ca eq cb) shouldBe true                       // interned to one shared object
    ca shouldBe a                                  // content preserved
  }

  it should "keep distinct synopses distinct" in {
    val one = SynopsisPool.canonical("the first film's synopsis")
    val two = SynopsisPool.canonical("a wholly different film's synopsis")
    one should not be theSameInstanceAs(two)
    one shouldBe "the first film's synopsis"
  }
}
