package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The phase-1 gate's arithmetic: a lookup is answerable only when EVERY request it made was
 *  served, a request shared by two lookups is one request, and the gate is met only at zero gaps. */
class IdentityQueryCoverageSpec extends AnyFlatSpec with Matchers {
  import IdentityQueryCoverage._

  private object Answers extends HttpFetch {
    override def get(url: String): String =
      if (url.contains("blocked")) throw new HttpStatusException(403, "GET", url, None)
      else if (url.contains("gone")) throw new HttpStatusException(404, "GET", url, None)
      else s"ok $url"
    override def post(url: String, body: String, contentType: String): String = "ok"
  }

  "the request log" should "file every request under the lookup running when it was made" in {
    val log = new RequestLog(Answers)
    log.get("https://api.themoviedb.org/3/search/movie?query=Belle&api_key=K")
    log.get("https://api.themoviedb.org/3/movie/1/credits?api_key=K")
    log.cut("resolve Belle")
    log.cut("resolve nothing-asked")
    log.post("https://api.graphql.imdb.com/", "{q}", "application/json")
    log.cut("resolve IMDb")
    log.byLookup.map { case (l, rs) => l -> rs.map(_.host) } shouldBe Seq(
      "resolve Belle"         -> Seq("api.themoviedb.org", "api.themoviedb.org"),
      "resolve nothing-asked" -> Nil,
      "resolve IMDb"          -> Seq("api.graphql.imdb.com"))
    log.byLookup.head._2.map(_.query).foreach(_ should not include "api_key=K")
  }

  it should "mark a request answered only by a failed read, and rethrow what the recording answered" in {
    val log = new RequestLog(Answers)
    intercept[HttpStatusException](log.get("https://www.cineworld.co.uk/api/blocked")).code shouldBe 403
    intercept[HttpStatusException](log.get("https://api.themoviedb.org/3/movie/gone")).code shouldBe 404
    log.cut("detail")
    log.byLookup.head._2.map(_.failedRead) shouldBe Seq(true, false)
    val c = IdentityQueryCoverage.of("uk", log.byLookup, missed = _ => false)
    c.failedReads.map(_.host) shouldBe Seq("www.cineworld.co.uk")
    c.line should include ("1 of them by a remembered failed read: www.cineworld.co.uk 1")
  }

  private def req(q: String) = Request(q, q, q.split(' ')(1))

  "coverage" should "count a lookup answerable only when every request it made was served" in {
    val c = IdentityQueryCoverage.of("pl", Seq(
      "resolve A" -> Seq(req("GET a.org/1"), req("GET a.org/2")),
      "resolve B" -> Seq(req("GET a.org/2"), req("GET b.org/3")),
      "resolve C" -> Nil), missed = _.query == "GET b.org/3")
    (c.lookups, c.answerableLookups, c.requests, c.answeredRequests) shouldBe ((3, 2, 3, 2))
    c.met shouldBe false
    c.gapsByHost shouldBe Seq("b.org/3" -> 1)
    c.line should include ("1 gap(s)")
  }

  it should "be met only when nothing is missed" in {
    val c = IdentityQueryCoverage.of("es", Seq("resolve A" -> Seq(req("GET a.org/1"))), missed = _ => false)
    c.met shouldBe true
    c.lookupShare shouldBe 1.0
    c.line should include ("GATE MET")
  }
}
