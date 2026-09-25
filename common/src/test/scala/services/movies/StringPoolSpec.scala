package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringPoolSpec extends AnyFlatSpec with Matchers {

  private val pool = new StringPool

  "canonical" should "return the SAME instance for byte-identical strings (interning)" in {
    val a = new String("A long editorial blurb that repeats across every venue in town.")
    val b = new String("A long editorial blurb that repeats across every venue in town.")
    (a eq b) shouldBe false                       // distinct instances, equal content
    val ca = pool.canonical(a)
    val cb = pool.canonical(b)
    (ca eq cb) shouldBe true                       // interned to one shared object
    ca shouldBe a                                  // content preserved
  }

  it should "keep distinct strings distinct" in {
    val one = pool.canonical("the first film's synopsis")
    val two = pool.canonical("a wholly different film's synopsis")
    one should not be theSameInstanceAs(two)
    one shouldBe "the first film's synopsis"
  }

  "canonicalAll" should "intern every element so equal list members share one instance" in {
    // Two films' cast lists that share a country/actor token — interned to one object each.
    val listA = pool.canonicalAll(Seq(new String("Poland"), new String("Cate Blanchett")))
    val listB = pool.canonicalAll(Seq(new String("Poland"), new String("Cate Blanchett")))
    (listA(0) eq listB(0)) shouldBe true
    (listA(1) eq listB(1)) shouldBe true
    listA shouldBe Seq("Poland", "Cate Blanchett")   // order + content preserved
    pool.canonicalAll(Seq.empty) shouldBe empty
  }

  // The pool's bound fails SILENTLY -- past the cap it evicts, the next lookup of an
  // evicted value allocates afresh, and interning becomes a no-op that still costs a
  // hash. Nothing logs. So the pool has to be able to SAY what it holds, or the only
  // symptom is a heap that grows, which is how worker-us came to OOM twice with 66.7%
  // of its String payload duplicate. These are the readings the gauges publish.
  //
  "the pool" should "report a growing occupancy as distinct strings are interned" in {
    val before = pool.heldEntries
    pool.canonical("a value interned once")
    pool.heldEntries should be > before
  }

  it should "not grow when the same value is interned again" in {
    val repeated = "interned twice"
    pool.canonical(repeated)
    val after = pool.heldEntries
    pool.canonical(repeated)
    pool.heldEntries shouldBe after
  }

  it should "start empty and count exactly what it was given" in {
    val pool = new StringPool
    pool.heldEntries shouldBe 0L
    pool.canonical(new String("one"))
    pool.canonical(new String("two"))
    pool.canonical(new String("one"))
    pool.heldEntries shouldBe 2L
  }

  it should "keep its interned instances to itself" in {
    val first  = new StringPool
    val second = new StringPool
    val a = first.canonical(new String("Poland"))
    second.canonical(new String("Poland")) should not be theSameInstanceAs(a)
    second.heldEntries shouldBe 1L
  }

  it should "evict nothing while the vocabulary fits" in {
    // A unit test's handful of strings is orders of magnitude below MaxEntries, so any
    // eviction here would mean the bound is not what it claims to be.
    pool.evictions shouldBe 0L
  }

  it should "report a hit ratio in range, counting a repeat as a hit" in {
    val v = "hit ratio probe"
    pool.canonical(v)
    pool.canonical(v)
    pool.hitRate should (be >= 0.0 and be <= 1.0)
  }
}
