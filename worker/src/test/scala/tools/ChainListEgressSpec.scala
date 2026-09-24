package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.roster.ChainDirectory.Multikino
import services.cinemas.roster.RosterSourceReader
import services.cinemas.roster.RosterFinding.DirectoryNotRead

import scala.collection.mutable

/** The roster audit's route to the chains' own venue lists: GitHub's runner
 *  IPs are Cloudflare-403'd by Multikino (run 35910520576, 2026-09-23), so with
 *  the Decodo credentials set the lists go through the residential proxy, and a
 *  list still unread after that fails the run instead of passing with a note. */
class ChainListEgressSpec extends AnyFlatSpec with Matchers {

  private class Recording(name: String, answer: String => String) extends GetOnlyHttpFetch {
    val calls = mutable.Buffer.empty[String]
    override def get(url: String): String = { calls += url; answer(url) }
  }
  private def blocked(url: String): String = throw new HttpStatusException(403, "GET", url, None)
  private val listUrl = "https://www.multikino.pl/api/microservice/showings/cinemas"
  private val unread  = DirectoryNotRead("Multikino", 38, "HttpStatusException: HTTP 403")

  "without proxy credentials" should "fetch the list directly and only note an unread one" in {
    val direct = new Recording("direct", _ => "{}")
    val egress = new ChainListEgress(direct, proxyShards = None)
    egress.fetchFor(Multikino)(listUrl).body shouldBe "{}"
    direct.calls shouldBe Seq(listUrl)
    egress.judged(unread).failing shouldBe false
  }

  "with proxy credentials" should "read the list through the residential proxy, not the blocked direct address" in {
    val direct = new Recording("direct", blocked)
    val proxy  = new Recording("proxy", _ => """{"result":[]}""")
    val page   = new ChainListEgress(direct, Some(IndexedSeq(proxy))).fetchFor(Multikino)(listUrl)
    page shouldBe FetchedPage(listUrl, """{"result":[]}""")
    proxy.calls shouldBe Seq(listUrl)
    direct.calls shouldBe empty
  }

  it should "fall back to the direct fetch when the proxy fails" in {
    val direct = new Recording("direct", _ => "direct")
    val proxy  = new Recording("proxy", url => throw new java.io.IOException("Tunnel failed, got: 503"))
    new ChainListEgress(direct, Some(IndexedSeq(proxy))).fetchFor(Multikino)(listUrl).body shouldBe "direct"
  }

  // Multikino's list wants the home page's session cookie. The audit warmed it up
  // front AND the proxy leg warmed again on the list's cold 401, so every run paid
  // for two home-page fetches through the proxy where one suffices.
  it should "warm Multikino's session once, when the list asks for it" in {
    var listCalls = 0
    val proxy = new Recording("proxy", url =>
      if (Multikino.warmUpUrl.contains(url)) ""
      else { listCalls += 1; if (listCalls == 1) throw new HttpStatusException(401, "GET", url, None) else "{}" })
    RosterSourceReader.readDirectory(new ChainListEgress(new Recording("direct", blocked), Some(IndexedSeq(proxy))).fetchFor(Multikino),
      java.time.LocalDate.of(2026, 9, 24))(Multikino, Nil)
    proxy.calls.count(Multikino.warmUpUrl.contains) shouldBe 1
    listCalls shouldBe 2
  }

  it should "fail the audit on a list that stays unread" in {
    val egress = new ChainListEgress(new Recording("direct", blocked), Some(IndexedSeq(new Recording("proxy", blocked))))
    val judged = egress.judged(unread)
    judged.failing shouldBe true
    judged.describe should include("residential proxy")
  }
}
