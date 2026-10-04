package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsObject}

/**
 * A 2xx is not an answer until its body is the content the endpoint serves. These bodies
 * are the TRANSPORT content of the check itself — minimal, real-world-shaped interstitials
 * (no challenge page has been recorded yet) — not inputs to any client's parser.
 */
class HttpReadSpec extends AnyFlatSpec with Matchers {
  import ReadOutcome._
  import HttpRead.PageMarker

  private val url = "https://upstream.example/api?apikey=secret"

  private def answering(body: String): HttpFetch = new GetOnlyHttpFetch {
    def get(u: String): String = body
  }
  private def throwing(failure: Throwable): HttpFetch = new GetOnlyHttpFetch {
    def get(u: String): String = throw failure
  }

  // The shape Cloudflare's managed challenge serves (trimmed): a 200/403 HTML document
  // whose only content is the interstitial script.
  private val CloudflareChallenge =
    """<!DOCTYPE html><html lang="en-US"><head><title>Just a moment...</title>
      |<meta http-equiv="refresh" content="390"></head><body><div class="main-wrapper">
      |<script>(function(){window._cf_chl_opt={cvId: '3',cType: 'managed'};}());</script>
      |</div></body></html>""".stripMargin
  // DataDome's interstitial: an almost-empty page loading its captcha frame.
  private val DataDomeChallenge =
    """<html><head><title>example.com</title></head><body>
      |<script>var dd={'rt':'c','cid':'AHrlqAAAAAMA','hsh':'2211F5','t':'fe','host':'geo.captcha-delivery.com'}</script>
      |<script src="https://ct.captcha-delivery.com/c.js"></script></body></html>""".stripMargin

  private def failedBody(outcome: ReadOutcome[?]): String = outcome match {
    case Failed(ReadFailure.UnexpectedBody(e)) => e.getMessage
    case other => fail(s"expected an unexpected-body failure, got $other")
  }

  "jsonArray" should "hand a JSON array to the parser" in {
    HttpRead.jsonArray(answering("""[1,2]"""), url)(a => Answered(a.value.size)) shouldBe Answered(2)
  }

  it should "leave an empty array to the parser — which alone may call it 'none'" in {
    HttpRead.jsonArray(answering("[]"), url)(a =>
      if (a.value.isEmpty) ReadOutcome.none("no results") else Answered(a)) shouldBe ReadOutcome.none("no results")
  }

  it should "fail, not answer empty, when the 2xx body is an HTML page" in {
    failedBody(HttpRead.jsonArray(answering("<html><body>Service unavailable</body></html>"), url)(_ => Answered(())))
      .should(include("not JSON"))
  }

  it should "fail on a challenge page served where JSON was expected" in {
    failedBody(HttpRead.jsonArray(answering(CloudflareChallenge), url)(_ => Answered(()))) should include("Cloudflare")
  }

  it should "fail when the root is an error object rather than the array" in {
    failedBody(HttpRead.jsonArray(answering("""{"Error":"Request limit reached!"}"""), url)((_: JsArray) => Answered(())))
      .should(include("not an array"))
  }

  "jsonObject" should "fail when the root is an array" in {
    failedBody(HttpRead.jsonObject(answering("[]"), url)((_: JsObject) => Answered(()))) should include("not an object")
  }

  it should "turn a parser that throws into an unexpected body, not an empty answer" in {
    failedBody(HttpRead.jsonObject(answering("""{"a":1}"""), url)(o => Answered((o \ "missing").as[String])))
      .should(include("parser threw"))
  }

  "html" should "parse a page carrying its marker" in {
    HttpRead.html(answering("""<div id="repertoire">x</div>"""), url, PageMarker("id=\"repertoire\""))(b => Answered(b.length))
      .shouldBe(a[Answered[?]])
  }

  it should "fail on a page without the marker" in {
    failedBody(HttpRead.html(answering("<html>maintenance</html>"), url, PageMarker("id=\"repertoire\""))(_ => Answered(())))
      .should(include("repertoire"))
  }

  it should "fail on a DataDome challenge" in {
    failedBody(HttpRead.html(answering(DataDomeChallenge), url, PageMarker("<body"))(_ => Answered(()))) should include("DataDome")
  }

  "text" should "not mistake Cloudflare's beacon on a normal page for a challenge" in {
    val page = """<html><body>films<script src="/cdn-cgi/challenge-platform/scripts/jsd/main.js"></script></body></html>"""
    HttpRead.text(answering(page), url)(b => Answered(b)) shouldBe Answered(page)
  }

  // Imperva Incapsula injects this script into EVERY page it protects, so a real page carries it.
  private val IncapsulaInjected = """<script type="text/javascript" src="/_Incapsula_Resource?SWJIYLWA=719d34d31c8e3a6e6fffd425f7e032f3&ns=1&cb=1" async></script>"""
  // Its block page: an iframe on the CWUDNSAI resource and the incident line.
  private val IncapsulaBlock =
    """<html style="height:100%"><head><META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW"></head><body style="margin:0px;height:100%">
      |<iframe id="main-iframe" src="/_Incapsula_Resource?CWUDNSAI=9&xinfo=1-2-0&incident_id=1-2&edet=12" frameborder=0>
      |Request unsuccessful. Incapsula incident ID: 1-2</iframe></body></html>""".stripMargin
  // Its JS challenge: a bare head running the injected script and nothing else.
  private val IncapsulaJsChallenge =
    s"""<html><head><META NAME="robots" CONTENT="noindex,nofollow">$IncapsulaInjected</head><body></body></html>"""

  "ChallengePage" should "not mistake Incapsula's script injected into a real page for a challenge" in {
    val page = s"<html><head>$IncapsulaInjected</head><body>${"<div class=\"film\">Diuna</div>" * 300}</body></html>"
    ChallengePage.detect(page) shouldBe None
  }

  it should "recognise Incapsula's block page and its bare JS challenge" in {
    ChallengePage.detect(IncapsulaBlock) shouldBe Some("Incapsula")
    ChallengePage.detect(IncapsulaJsChallenge) shouldBe Some("Incapsula")
  }

  "every helper" should "read a typed 404 as absent and a 503 as failed" in {
    HttpRead.text(throwing(new HttpStatusException(404, "GET", url, None)), url)(Answered(_)) shouldBe a[Absent]
    HttpRead.jsonArray(throwing(new HttpStatusException(503, "GET", url, None)), url)(Answered(_)) shouldBe a[Failed]
  }

  it should "mask credentials in the explained failure" in {
    HttpRead.jsonArray(answering("<html/>"), url)(Answered(_)).explain should not include "secret"
  }

  "postJsonObject" should "check a POSTed answer like a GET one" in {
    val graphQl = new HttpFetch {
      def get(u: String): String = fail("no GET")
      def post(u: String, body: String, contentType: String): String = if (body.contains("ok")) """{"data":{}}""" else "<html/>"
    }
    HttpRead.postJsonObject(graphQl, url, "ok")(o => Answered(o.keys)) shouldBe Answered(Set("data"))
    HttpRead.postJsonObject(graphQl, url, "x")(o => Answered(o)) shouldBe a[Failed]
  }

  "page" should "return a page, rethrow a 404 as the original status, and refuse a challenge" in {
    HttpRead.page(answering("<html>films</html>"), url) shouldBe "<html>films</html>"
    val gone = new HttpStatusException(404, "GET", url, None)
    (the[HttpStatusException] thrownBy HttpRead.page(throwing(gone), url)) shouldBe gone
    an[UnexpectedBodyException] should be thrownBy HttpRead.page(answering(CloudflareChallenge), url)
  }

  "postPage and pageBytes" should "refuse a challenge page like page does" in {
    val graph = new HttpFetch {
      def get(u: String): String = CloudflareChallenge
      def post(u: String, body: String, contentType: String): String = if (body == "ok") "{}" else CloudflareChallenge
    }
    HttpRead.postPage(graph, url, "ok") shouldBe "{}"
    an[UnexpectedBodyException] should be thrownBy HttpRead.postPage(graph, url, "x")
    an[UnexpectedBodyException] should be thrownBy HttpRead.pageBytes(graph, url)
    new String(HttpRead.pageBytes(answering("caf\u00e9"), url), "UTF-8") shouldBe "caf\u00e9"
  }
}
