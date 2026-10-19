package clients.tools

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpFetch

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Files

/**
 * Guards that `RecordingHttpFetch` captures the RAW wire bytes of a legacy
 * single-byte page, not a UTF-8 round-trip of them.
 *
 * Kino Charlie and Kino Pod Baranami ship raw ISO-8859-2 with `Content-Type:
 * text/html` and NO charset declaration. Their scrapers read the undecoded
 * bytes (`HttpFetch.getBytes`) and decode ISO-8859-2 themselves. The recorder
 * used to capture via `delegate.get(url)` — a UTF-8 decode that mangles every
 * Polish letter to `U+FFFD` before the bytes ever reach disk — so the recorded
 * fixture could never round-trip back to the real title (`Człowiek` →
 * `ďż˝`-laden garbage on replay). The fix records `delegate.getBytes(url)`
 * verbatim, so the bytes the scraper decodes on replay are the bytes the site
 * actually served.
 */
class RecorderCharsetCaptureSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val Latin2: Charset = Charset.forName("ISO-8859-2")
  private val temporaryRoot   = new File("test/resources/fixtures/recorder-charset-capture-spec")
  private val pageUrl         = "https://www.charlie.pl/"

  /** The Polish word "Człowiek" — `ł` is byte 0xB3 in ISO-8859-2, which is an
   *  invalid lone UTF-8 byte, so a UTF-8 decode of these bytes loses it. */
  private val realPageBytes: Array[Byte] = "Człowiek z marmuru".getBytes(Latin2)

  /** Stand-in for `RealHttpFetch`: `getBytes` yields the true wire bytes;
   *  `get` decodes them as UTF-8 (lossy for ISO-8859-2), exactly as the real
   *  client does for a charset-less single-byte page. */
  private def serveLatin2Page: HttpFetch = new HttpFetch {
    override def getBytes(url: String): Array[Byte] = realPageBytes
    override def get(url: String): String           = new String(realPageBytes, StandardCharsets.UTF_8)
    override def post(url: String, body: String, contentType: String): String =
      throw new UnsupportedOperationException
  }

  private def fixtureFile(directory: String): File =
    new File(s"test/resources/fixtures/recorder-charset-capture-spec/$directory/www.charlie.pl")

  "RecordingHttpFetch.getBytes" should
    "write the raw ISO-8859-2 wire bytes, not a lossy UTF-8 round-trip" in {
    val recorder = new RecordingHttpFetch("recorder-charset-capture-spec/getbytes", serveLatin2Page)

    val returned = recorder.getBytes(pageUrl)
    returned shouldBe realPageBytes

    val onDisk = Files.readAllBytes(fixtureFile("getbytes").toPath)
    onDisk shouldBe realPageBytes
    // The decisive proof: the recorded bytes still decode to the real title…
    new String(onDisk, Latin2) shouldBe "Człowiek z marmuru"
    // …and carry no replacement char (the old UTF-8 round-trip baked one in).
    onDisk should not contain 0xEF.toByte // first byte of the `U+FFFD` (ef bf bd)
  }

  "RecordingHttpFetch.get" should
    "also record the raw wire bytes, so a get-driven capture isn't lossy either" in {
    val recorder = new RecordingHttpFetch("recorder-charset-capture-spec/get", serveLatin2Page)

    recorder.get(pageUrl)

    val onDisk = Files.readAllBytes(fixtureFile("get").toPath)
    onDisk shouldBe realPageBytes
    new String(onDisk, Latin2) shouldBe "Człowiek z marmuru"
  }

  override def afterAll(): Unit = {
    deleteRecursively(temporaryRoot)
    super.afterAll()
  }

  private def deleteRecursively(f: File): Unit = {
    if (f.isDirectory) Option(f.listFiles).foreach(_.foreach(deleteRecursively))
    f.delete()
    ()
  }
}
