package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.roster.ChainDirectory.Multikino
import services.cinemas.roster.RosterSourceReader
import services.cinemas.roster.RosterFinding.DirectoryNotRead

import clients.tools.{RequestLogHttpFetch, ScriptedByUrlHttpFetch}

/** The roster audit's route to the chains' own venue lists: GitHub's runner
 *  IPs are Cloudflare-403'd by Multikino (run 35910520576, 2026-09-23), so with
 *  the Decodo credentials set the lists go through the residential proxy, and a
 *  list still unread after that fails the run instead of passing with a note. */
class ChainListEgressSpec extends AnyFlatSpec with Matchers {

  private def recording(answer: String => String) = new RequestLogHttpFetch(new ScriptedByUrlHttpFetch(answer))
  private def blocked(url: String): String = throw new HttpStatusException(403, "GET", url, None)
  private val listUrl = "https://www.multikino.pl/api/microservice/showings/cinemas"
  private val unread  = DirectoryNotRead("Multikino", 38, "HttpStatusException: HTTP 403")

  "without proxy credentials" should "fetch the list directly and only note an unread one" in {
    val direct = recording(_ => "{}")
    val egress = new ChainListEgress(direct, proxyShards = None, SpecClock.Pinned, _root_.tools.SpecStopwatch.Frozen)
    egress.fetchFor(Multikino)(listUrl).body shouldBe "{}"
    direct.gets shouldBe Seq(listUrl)
    egress.judged(unread).failing shouldBe false
  }

  "with proxy credentials" should "read the list through the residential proxy, not the blocked direct address" in {
    val direct = recording(blocked)
    val proxy  = recording(_ => """{"result":[]}""")
    val page   = new ChainListEgress(direct, Some(IndexedSeq(proxy)), SpecClock.Pinned, _root_.tools.SpecStopwatch.Frozen).fetchFor(Multikino)(listUrl)
    page shouldBe FetchedPage(listUrl, """{"result":[]}""")
    proxy.gets shouldBe Seq(listUrl)
    direct.gets shouldBe empty
  }

  it should "fall back to the direct fetch when the proxy fails" in {
    val direct = recording(_ => "direct")
    val proxy  = recording(_ => throw new java.io.IOException("Tunnel failed, got: 503"))
    new ChainListEgress(direct, Some(IndexedSeq(proxy)), SpecClock.Pinned, _root_.tools.SpecStopwatch.Frozen).fetchFor(Multikino)(listUrl).body shouldBe "direct"
  }

  // Multikino's list wants the home page's session cookie. The audit warmed it up
  // front AND the proxy leg warmed again on the list's cold 401, so every run paid
  // for two home-page fetches through the proxy where one suffices.
  it should "warm Multikino's session once, when the list asks for it" in {
    var listCalls = 0
    val proxy = recording(url =>
      if (Multikino.warmUpUrl.contains(url)) ""
      else { listCalls += 1; if (listCalls == 1) throw new HttpStatusException(401, "GET", url, None) else "{}" })
    RosterSourceReader.readDirectory(new ChainListEgress(recording(blocked), Some(IndexedSeq(proxy)), SpecClock.Pinned, _root_.tools.SpecStopwatch.Frozen).fetchFor(Multikino),
      java.time.LocalDate.of(2026, 9, 24))(Multikino, Nil)
    proxy.gets.count(Multikino.warmUpUrl.contains) shouldBe 1
    listCalls shouldBe 2
  }

  it should "fail the audit on a list that stays unread" in {
    val egress = new ChainListEgress(recording(blocked), Some(IndexedSeq(recording(blocked))), SpecClock.Pinned, _root_.tools.SpecStopwatch.Frozen)
    val judged = egress.judged(unread)
    judged.failing shouldBe true
    judged.describe should include("residential proxy")
  }
}
