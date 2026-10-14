package services.facebook

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.Instant

/** The web's record of the film pages Facebook's crawler fetches: what decides which pages the
 *  worker asks Facebook to re-scrape. */
class FacebookFetchRecorderSpec extends AnyFlatSpec with Matchers {

  private val T0      = Instant.parse("2026-10-14T18:00:00Z")
  private val Crawler = Some("facebookexternalhit/1.1 (+http://www.facebook.com/externalhit_uatext.php)")
  private val Browser = Some("Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 Version/18.0 Mobile/15E148 Safari/604.1")
  private val Page    = "https://kinowo.net/poznan/movie/testowy-film"

  private final class Rig(store: FacebookFetches = new InMemoryFacebookFetches) {
    val clock    = new MutableClock(T0)
    val recorder = new FacebookFetchRecorder(store, clock)
  }

  "A film page Facebook's crawler fetched" should "be recorded with its latest fetch, once a batch is written" in {
    val store = new InMemoryFacebookFetches
    val rig   = new Rig(store)
    rig.recorder.served(Crawler, Page)
    store.recorded shouldBe empty
    rig.clock.advanceSeconds(10)
    rig.recorder.served(Some("Facebot"), Page)
    rig.recorder.flush()
    store.recorded shouldBe Map(Page -> T0.plusSeconds(10))
  }

  "A film page anybody else fetched" should "not be recorded" in {
    val store = new InMemoryFacebookFetches
    val rig   = new Rig(store)
    rig.recorder.served(Browser, Page)
    rig.recorder.served(None, Page)
    rig.recorder.served(Some("Twitterbot/1.0"), Page)
    rig.recorder.flush()
    store.recorded shouldBe empty
  }

  // A crawl of every film page (Facebook has fetched ~2,500 a fortnight in one country) must not
  // grow the web's heap between writes: past the cap a batch takes no new page.
  "A batch" should "hold at most its cap of pages between writes" in {
    val store = new InMemoryFacebookFetches
    val rig   = new Rig(store)
    (1 to FacebookFetchRecorder.MaxPending + 50).foreach(i => rig.recorder.served(Crawler, s"$Page-$i"))
    rig.recorder.served(Crawler, s"$Page-1")
    rig.recorder.flush()
    store.recorded.size shouldBe FacebookFetchRecorder.MaxPending
    rig.recorder.served(Crawler, Page)
    rig.recorder.flush()
    store.recorded.keySet should contain(Page)
  }

  "A batch the store refuses" should "not stop the next one being written" in {
    var down  = true
    val inner = new InMemoryFacebookFetches
    val store = new FacebookFetches {
      def record(fetched: Map[String, Instant]): Unit = if (down) throw new IllegalStateException("fleet database unreachable") else inner.record(fetched)
      def fetchedSince(urls: Seq[String], since: Instant): Set[String] = inner.fetchedSince(urls, since)
    }
    val rig = new Rig(store)
    rig.recorder.served(Crawler, Page)
    rig.recorder.flush()
    down = false
    rig.recorder.served(Crawler, s"$Page-2")
    rig.recorder.flush()
    inner.recorded.keySet shouldBe Set(s"$Page-2")
  }

  "Stopping" should "write the batch still held" in {
    val store = new InMemoryFacebookFetches
    val rig   = new Rig(store)
    rig.recorder.served(Crawler, Page)
    rig.recorder.stop()
    store.recorded.keySet shouldBe Set(Page)
  }
}
