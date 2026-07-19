package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class CountingHttpFetchSpec extends AnyFlatSpec with Matchers {

  /** Delegate that returns a body or throws a scripted error for the next call. */
  private class StubFetch(outcome: () => String) extends HttpFetch {
    def get(url: String): String = outcome()
    def post(url: String, body: String, contentType: String): String = outcome()
    override def getBytes(url: String): Array[Byte] = outcome().getBytes("UTF-8")
  }

  private def recordingFetch(outcome: () => String) = {
    val seen = mutable.ListBuffer.empty[String]
    val fetch = new CountingHttpFetch(new StubFetch(outcome), (o: String) => { seen += o; () })
    (fetch, seen)
  }

  private def ok        = () => "body"
  private def status(c: Int) = () => throw new HttpStatusException(c, "GET", "u", None)

  "CountingHttpFetch" should "record success for a normal return and pass the body through" in {
    val (fetch, seen) = recordingFetch(ok)
    fetch.get("u") shouldBe "body"
    seen.toList shouldBe List(HttpOutcome.Success)
  }

  // The named codes each get their own series because they mean opposite things:
  // a 404 is the RT/Metacritic slug probe working as designed, a 403 is a host
  // refusing us. Aggregated as one `http_4xx` line, a rising 403 hides behind the
  // 404 noise floor.
  it should "give each probable 4xx code its own outcome" in {
    val expected = Seq(
      400 -> HttpOutcome.Http400,
      401 -> HttpOutcome.Http401,
      403 -> HttpOutcome.Http403,
      404 -> HttpOutcome.Http404,
      410 -> HttpOutcome.Http410,
      429 -> HttpOutcome.Http429)
    expected.foreach { case (code, outcome) =>
      val (fetch, seen) = recordingFetch(status(code))
      a [HttpStatusException] should be thrownBy fetch.get("u")
      withClue(s"status $code: ") { seen.toList shouldBe List(outcome) }
    }
  }

  it should "fold an improbable 4xx into the residual bucket" in {
    Seq(409, 422, 451).foreach { code =>
      val (fetch, seen) = recordingFetch(status(code))
      a [HttpStatusException] should be thrownBy fetch.get("u")
      withClue(s"status $code: ") { seen.toList shouldBe List(HttpOutcome.Http4xx) }
    }
  }

  // Seeding the Prometheus grid walks `all`; a label the classifier can emit but
  // `all` omits would draw a Grafana line that pops in mid-chart.
  it should "list every outcome it can classify in `all`" in {
    val classified = Seq(400, 401, 403, 404, 410, 429, 409, 503)
      .map(c => HttpOutcome.classify(new HttpStatusException(c, "GET", "u", None)))
    classified.distinct.foreach(o => withClue(s"$o missing from all: ") { HttpOutcome.all should contain (o) })
  }

  it should "classify 5xx" in {
    val (fetch, seen) = recordingFetch(status(503))
    a [HttpStatusException] should be thrownBy fetch.get("u")
    seen.toList shouldBe List(HttpOutcome.Http5xx)
  }

  it should "classify connect and read timeouts as timeout" in {
    val (connect, seenC) = recordingFetch(() => throw new java.net.http.HttpConnectTimeoutException("connect"))
    a [java.net.http.HttpTimeoutException] should be thrownBy connect.get("u")
    seenC.toList shouldBe List(HttpOutcome.Timeout)

    val (read, seenR) = recordingFetch(() => throw new java.net.http.HttpTimeoutException("read"))
    a [java.net.http.HttpTimeoutException] should be thrownBy read.get("u")
    seenR.toList shouldBe List(HttpOutcome.Timeout)
  }

  it should "classify connection-layer errors as connection_error" in {
    val cases: Seq[Throwable] = Seq(
      new java.net.ConnectException("refused"),
      new java.net.UnknownHostException("nope"),
      new java.net.SocketException("reset"),
      new javax.net.ssl.SSLHandshakeException("tls"),
      new java.io.IOException("generic io"))
    cases.foreach { ex =>
      val (fetch, seen) = recordingFetch(() => throw ex)
      a [Throwable] should be thrownBy fetch.get("u")
      withClue(s"$ex: ") { seen.toList shouldBe List(HttpOutcome.ConnectionError) }
    }
  }

  it should "classify an unrelated error as other, still re-throwing it" in {
    val (fetch, seen) = recordingFetch(() => throw new IllegalStateException("weird"))
    a [IllegalStateException] should be thrownBy fetch.get("u")
    seen.toList shouldBe List(HttpOutcome.Other)
  }

  it should "count getBytes and post as well, forwarding getBytes to the delegate's bytes" in {
    val (fetch, seen) = recordingFetch(ok)
    new String(fetch.getBytes("u"), "UTF-8") shouldBe "body"
    fetch.post("u", "b", "application/json") shouldBe "body"
    seen.toList shouldBe List(HttpOutcome.Success, HttpOutcome.Success)
  }

  it should "count each retry separately — three 429s then a success is 3+1 entries" in {
    // The decorator is call-per-attempt; the retry loop lives above it, so a
    // caller that re-invokes on failure produces one entry per attempt.
    var n = 0
    val (fetch, seen) = recordingFetch(() => { n += 1; if (n <= 3) throw new HttpStatusException(429, "GET", "u", None) else "body" })
    (1 to 4).foreach(_ => try fetch.get("u") catch { case _: HttpStatusException => () })
    seen.toList shouldBe List(HttpOutcome.Http429, HttpOutcome.Http429, HttpOutcome.Http429, HttpOutcome.Success)
  }
}
