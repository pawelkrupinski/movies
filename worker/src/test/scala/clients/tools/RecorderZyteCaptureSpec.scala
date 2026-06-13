package clients.tools

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FallbackHttpFetch, GetOnlyHttpFetch, HttpFetch}

import java.io.File
import java.nio.file.Files

/**
 * Guards `RecordAllDataToFixture`'s capture of Zyte-routed cinemas (Multikino,
 * Kino Kameralne / biletyna). Those sit behind a WAF that blocks our datacenter
 * IP, so they're fetched through a Zyte-primary → `direct` chain whose Zyte leg
 * tunnels through its OWN HttpClient. A `RecordingHttpFetch` wired as the
 * chain's inner `direct` fallback therefore never sees a Zyte-served response —
 * the scrape succeeds but the corpus silently lacks every `www.multikino.pl`
 * fixture. The recorder fixes this by wrapping the WHOLE chain in recording.
 *
 * Both halves are pinned here with a hermetic stand-in for the Zyte leg (a
 * fetch that returns a body without delegating to `direct`, exactly like
 * `ZyteFetch` when it serves) — no network, no `ZYTE_API_KEY` needed.
 */
class RecorderZyteCaptureSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val tmpRoot = new File("test/resources/fixtures/recorder-zyte-capture-spec")
  private val MultikinoFilmsUrl =
    "https://www.multikino.pl/api/microservice/showings/cinemas/0011/films"

  /** Stand-in for the Zyte leg: serves `body` for any URL without consulting a
   *  fallback — the same shape as a real `ZyteFetch` that succeeds. */
  private def zyteServing(body: String): HttpFetch = new GetOnlyHttpFetch {
    override def get(url: String): String = body
  }

  private def filmsFixture(dir: String): File =
    new File(s"test/resources/fixtures/recorder-zyte-capture-spec/$dir/" +
      "www.multikino.pl/api/microservice/showings/cinemas/0011/films")

  "Recording wired as the chain's inner `direct` fallback (the old wiring)" should
    "miss a Zyte-served response — the bug this fix closes" in {
    val recorderAsDirect = new RecordingHttpFetch(
      "recorder-zyte-capture-spec/inner-direct", zyteServing("unused"))
    // Zyte serves first, so the `direct` recorder is never consulted.
    val chain = new FallbackHttpFetch(Seq(
      "zyte"   -> zyteServing("films-json-from-zyte"),
      "direct" -> recorderAsDirect))

    chain.get(MultikinoFilmsUrl) shouldBe "films-json-from-zyte"
    filmsFixture("inner-direct").exists shouldBe false
  }

  "Recording wrapped around the whole chain (the new wiring)" should
    "capture the Zyte-served response keyed by the target URL" in {
    val chain: HttpFetch = new FallbackHttpFetch(Seq(
      "zyte"   -> zyteServing("films-json-from-zyte"),
      "direct" -> zyteServing("unused")))
    // Compiles only because RecordingHttpFetch's delegate was widened from
    // RealHttpFetch to HttpFetch — the change that lets the recorder wrap the
    // Zyte chain at all.
    val recorder = new RecordingHttpFetch("recorder-zyte-capture-spec/outer", chain)

    recorder.get(MultikinoFilmsUrl) shouldBe "films-json-from-zyte"
    val f = filmsFixture("outer")
    f.exists shouldBe true
    new String(Files.readAllBytes(f.toPath), "UTF-8") shouldBe "films-json-from-zyte"
  }

  "The recorder's wiring" should
    "wrap the Multikino and biletyna chains in recording from the OUTSIDE" in {
    // The actual fix: these must be RecordingHttpFetch (recording the whole
    // Zyte chain), not a bare FallbackHttpFetch with recording buried inside as
    // `direct`. Forcing these lazy vals builds the chains only — no Mongo, no
    // network, no `main()`.
    RecordAllDataToFixture.multikinoFetch shouldBe a[RecordingHttpFetch]
    RecordAllDataToFixture.biletynaFetch  shouldBe a[RecordingHttpFetch]
  }

  override def afterAll(): Unit = {
    deleteRecursively(tmpRoot)
    super.afterAll()
  }

  private def deleteRecursively(f: File): Unit = {
    if (f.isDirectory) Option(f.listFiles).foreach(_.foreach(deleteRecursively))
    f.delete()
    ()
  }
}
