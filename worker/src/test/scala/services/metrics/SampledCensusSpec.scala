package services.metrics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration.*

class SampledCensusSpec extends AnyFlatSpec with Matchers {

  /** A census whose reading blocks until released, recording the thread it ran on. */
  private final class Blocking(delay: FiniteDuration) extends SampledCensus {
    val release = new CountDownLatch(1)
    val sampled = new CountDownLatch(1)
    @volatile var sampledOn: String = ""
    protected def censusName: String = "blocking-census"
    protected def sampleInterval: FiniteDuration = 1.hour
    override protected def firstSampleDelay: FiniteDuration = delay
    def sample(): Unit = { sampledOn = Thread.currentThread().getName; release.await(); sampled.countDown() }
  }

  "a census" should "start without taking a reading on the caller's thread" in {
    // Taken at start on the boot thread, the censuses held a US worker's boot for ~200 s.
    val census = new Blocking(0.seconds)
    try {
      census.start()                       // returns though the reading blocks
      census.release.countDown()
      census.sampled.await(5, TimeUnit.SECONDS) shouldBe true
      census.sampledOn should not be Thread.currentThread().getName
    } finally census.stop()
  }

  it should "take its first reading after the boot delay, not a whole interval later" in {
    val census = new Blocking(100.millis)
    try {
      census.release.countDown()
      census.start()
      census.sampled.getCount shouldBe 1L
      census.sampled.await(5, TimeUnit.SECONDS) shouldBe true
    } finally census.stop()
  }

  "the first-sample delay" should "never exceed the census's own interval" in {
    (new SampledCensusSpec.Quick).delay shouldBe 30.seconds
    SampledCensus.FirstSampleDelay shouldBe 2.minutes
  }
}

object SampledCensusSpec {
  final class Quick extends SampledCensus {
    protected def censusName: String = "quick-census"
    protected def sampleInterval: FiniteDuration = 30.seconds
    def sample(): Unit = ()
    def delay: FiniteDuration = firstSampleDelay
  }
}
