package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class MemoizedHttpFetchSpec extends AnyFlatSpec with Matchers {

  /** Records every URL the decorator actually forwards, and answers per URL:
   *  a `404` prefix throws, anything else comes back as the body. */
  private class RecordingFetch extends GetOnlyHttpFetch {
    val requested: mutable.ListBuffer[String] = mutable.ListBuffer.empty
    def get(url: String): String = {
      requested += url
      if (url.startsWith("404")) throw new RuntimeException(s"HTTP 404 for $url")
      s"body of $url"
    }
    override def getBytes(url: String): Array[Byte] = { requested += s"bytes:$url"; Array.emptyByteArray }
  }

  "MemoizedHttpFetch" should "fetch a repeated URL once and replay the body" in {
    val underlying = new RecordingFetch
    val fetch = new MemoizedHttpFetch(underlying)

    fetch.get("a") shouldBe "body of a"
    fetch.get("a") shouldBe "body of a"
    fetch.get("a") shouldBe "body of a"

    underlying.requested.toList shouldBe List("a")
  }

  it should "still fetch distinct URLs separately, in call order" in {
    val underlying = new RecordingFetch
    val fetch = new MemoizedHttpFetch(underlying)

    fetch.get("a"); fetch.get("b"); fetch.get("a"); fetch.get("c")

    underlying.requested.toList shouldBe List("a", "b", "c")
  }

  // A 404 IS the answer to a slug probe, not a transient error: re-probing the
  // same slug inside one ladder cannot come back different, so the failure is
  // memoised alongside the successes. Without this the dedup would miss exactly
  // the case that costs the most — a ladder where every probe misses.
  it should "replay a thrown failure without re-fetching" in {
    val underlying = new RecordingFetch
    val fetch = new MemoizedHttpFetch(underlying)

    val first  = intercept[RuntimeException](fetch.get("404/movie/sting"))
    val second = intercept[RuntimeException](fetch.get("404/movie/sting"))

    first.getMessage shouldBe second.getMessage
    underlying.requested.toList shouldBe List("404/movie/sting")
  }

  it should "forward getBytes to the delegate rather than re-encoding a memoised String" in {
    // Per the HttpFetch contract for delegating wrappers: inheriting the default
    // would round-trip wire bytes through a UTF-8 decode and mojibake a legacy
    // single-byte page.
    val underlying = new RecordingFetch
    new MemoizedHttpFetch(underlying).getBytes("a")
    underlying.requested.toList shouldBe List("bytes:a")
  }

  it should "collapse concurrent gets of the same URL to one fetch" in {
    // Rows are refreshed in parallel; a single memo instance is per-attempt, but
    // nothing stops an attempt from fanning out, so the map must be safe.
    val underlying = new RecordingFetch
    val fetch = new MemoizedHttpFetch(underlying)

    val threads = (1 to 8).map(_ => new Thread(() => { fetch.get("a"); () }))
    threads.foreach(_.start())
    threads.foreach(_.join())

    underlying.requested.toList shouldBe List("a")
  }
}
