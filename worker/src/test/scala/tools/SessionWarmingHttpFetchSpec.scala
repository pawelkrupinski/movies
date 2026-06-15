package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class SessionWarmingHttpFetchSpec extends AnyFlatSpec with Matchers {

  // Records calls; per-URL queue of Either[Throwable, body] so each test arranges
  // the cold-fail / warm / retry shape it wants.
  private class FakeFetch(responses: Map[String, mutable.Queue[Either[Throwable, String]]]) extends HttpFetch {
    val calls: mutable.Buffer[String] = mutable.Buffer.empty
    override def get(url: String): String = {
      calls += url
      responses.get(url).flatMap(qq => Option.when(qq.nonEmpty)(qq.dequeue())) match {
        case Some(Right(body)) => body
        case Some(Left(err))   => throw err
        case None              => throw new RuntimeException(s"unexpected $url")
      }
    }
    override def post(url: String, body: String, contentType: String): String =
      throw new UnsupportedOperationException
  }
  private def q(xs: Either[Throwable, String]*) = mutable.Queue(xs*)

  private val Home = "https://www.multikino.pl/"
  private val Api  = "https://www.multikino.pl/api/x"

  "SessionWarmingHttpFetch" should "pass through when the first GET succeeds — no warm-up" in {
    val f = new FakeFetch(Map(Api -> q(Right("ok"))))
    new SessionWarmingHttpFetch(f, Home).get(Api) shouldBe "ok"
    f.calls.toList shouldBe List(Api)
  }

  it should "warm the homepage then retry when the first GET fails (the Multikino 401 case)" in {
    val f = new FakeFetch(Map(
      Api  -> q(Left(new RuntimeException("HTTP 401")), Right("warmed-ok")),
      Home -> q(Right("<html/>"))))
    new SessionWarmingHttpFetch(f, Home).get(Api) shouldBe "warmed-ok"
    f.calls.toList shouldBe List(Api, Home, Api)
  }

  it should "still retry the target even if the warm-up GET itself fails" in {
    val f = new FakeFetch(Map(
      Api  -> q(Left(new RuntimeException("HTTP 401")), Right("ok-anyway")),
      Home -> q(Left(new RuntimeException("home boom")))))
    new SessionWarmingHttpFetch(f, Home).get(Api) shouldBe "ok-anyway"
    f.calls.toList shouldBe List(Api, Home, Api)
  }

  it should "propagate the failure when the retry also fails (→ FallbackHttpFetch rolls to Zyte)" in {
    val f = new FakeFetch(Map(
      Api  -> q(Left(new RuntimeException("401 #1")), Left(new RuntimeException("401 #2"))),
      Home -> q(Right("<html/>"))))
    val e = the[RuntimeException] thrownBy new SessionWarmingHttpFetch(f, Home).get(Api)
    e.getMessage shouldBe "401 #2"
    f.calls.toList shouldBe List(Api, Home, Api)
  }
}
