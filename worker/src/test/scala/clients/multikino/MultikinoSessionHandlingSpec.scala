package clients.multikino

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tools.HttpFetch
import services.cinemas.pl.MultikinoClient

import scala.collection.mutable

/** Exercises `MultikinoClient`'s built-in session handling — optimistic API
 *  call, homepage warm-up on failure, then retry — by driving `fetch()`
 *  against an in-memory `HttpFetch`. The same code path runs in
 *  production (`RealHttpFetch`) and in fixture tests (`FakeHttpFetch`),
 *  because the rules live inside the client rather than in a wrapper
 *  callers compose around the fetch. */
class MultikinoSessionHandlingSpec extends AnyFlatSpec with Matchers {

  private val HomeUrl  = "https://www.multikino.pl/"
  private val EmptyApi = """{"result":[]}"""

  // Minimal in-memory HttpFetch: records every URL it sees, and lets the test
  // pre-program a queue of responses per URL (a body to return, or a
  // throwable to raise).
  private class ProgrammedHttpFetch(
    responses: mutable.Map[String, mutable.Queue[Either[Throwable, String]]]
  ) extends HttpFetch {
    val calls: mutable.Buffer[String] = mutable.Buffer.empty
    override def get(url: String): String = {
      calls += url
      responses.get(url).flatMap(q => Option.when(q.nonEmpty)(q.dequeue())) match {
        case Some(Right(body)) => body
        case Some(Left(err))   => throw err
        case None              => throw new RuntimeException(s"unexpected url=$url")
      }
    }
    override def post(url: String, body: String, contentType: String): String =
      throw new UnsupportedOperationException
  }

  private def programmed(pairs: (String, Seq[Either[Throwable, String]])*): ProgrammedHttpFetch =
    new ProgrammedHttpFetch(
      mutable.Map.from(pairs.map { case (k, v) => k -> mutable.Queue.from(v) })
    )

  "MultikinoClient.fetch" should "hit only the API URL when the first call succeeds" in {
    val http = programmed(MultikinoClient.ApiUrl -> Seq(Right(EmptyApi)))

    new MultikinoClient(http).fetch() shouldBe empty
    http.calls.toList shouldBe List(MultikinoClient.ApiUrl)
  }

  it should "warm up the homepage then retry the API when the first API call fails" in {
    val http = programmed(
      MultikinoClient.ApiUrl -> Seq(Left(new RuntimeException("HTTP 401")), Right(EmptyApi)),
      HomeUrl                -> Seq(Right("<html/>")),
    )

    new MultikinoClient(http).fetch() shouldBe empty
    http.calls.toList shouldBe List(MultikinoClient.ApiUrl, HomeUrl, MultikinoClient.ApiUrl)
  }

  it should "propagate the failure when the retry also fails" in {
    val http = programmed(
      MultikinoClient.ApiUrl -> Seq(
        Left(new RuntimeException("HTTP 401 #1")),
        Left(new RuntimeException("HTTP 401 #2")),
      ),
      HomeUrl                -> Seq(Right("<html/>")),
    )

    val thrown = the[RuntimeException] thrownBy new MultikinoClient(http).fetch()
    thrown.getMessage should include ("HTTP 401 #2")
    http.calls.toList shouldBe List(MultikinoClient.ApiUrl, HomeUrl, MultikinoClient.ApiUrl)
  }
}
