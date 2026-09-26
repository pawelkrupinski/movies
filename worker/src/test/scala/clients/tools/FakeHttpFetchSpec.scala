package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** The fixture replay's lookup is case-exact (a mis-cased request fails on a Mac as it does on
 *  CI), and that must not turn a fixture root reached through a SYMLINK into "no fixture": a root
 *  handed in as a link (a scratch directory pointing at a downloaded tree) replayed every request
 *  as a miss, silently, because the check compared the link's path with the file's real one. */
class FakeHttpFetchSpec extends AnyFlatSpec with Matchers {

  private def tree(): (Path, Path) = {
    val base = Files.createTempDirectory("fake-http-fetch")
    val real = Files.createDirectories(base.resolve("real/enrichment-xx/api.example.com/3/search"))
    val key  = RecordingHttpFetch.fixtureKey("https://api.example.com/3/search/person?query=Ridley+Scott", foldYear = false)
    Files.writeString(base.resolve("real/enrichment-xx").resolve(key), """{"results":[]}""", StandardCharsets.UTF_8)
    val link = Files.createSymbolicLink(base.resolve("link"), base.resolve("real"))
    (base.resolve("real"), link)
  }

  "a fixture root reached through a symlink" should "replay what the tree holds" in {
    val (_, link) = tree()
    val fetch = new FakeHttpFetch("enrichment-xx", strict = true, foldYear = false, root = settings.FixtureRoot(link))
    fetch.get("https://api.example.com/3/search/person?query=Ridley+Scott&api_key=k") shouldBe """{"results":[]}"""
  }

  it should "still refuse a mis-cased request" in {
    val (_, link) = tree()
    val fetch = new FakeHttpFetch("enrichment-xx", strict = true, foldYear = false, root = settings.FixtureRoot(link))
    a[java.io.FileNotFoundException] should be thrownBy fetch.get("https://api.example.com/3/SEARCH/person?query=Ridley+Scott")
  }
}
