package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The fetching half of "every convergence build publishes the missing data it found": what a hermetic
 * leg's tree could not answer, fetched by the next leg's rows before their suites, within a budget, into a tree the
 * row's suite and the legs after it replay.
 */
class MissingFixtureFillSpec extends AnyFlatSpec with Matchers {

  private val Tree = "enrichment-us"
  private def gap(url: String) = MissingFixtures.Refetch("GET", url)

  private class Live(answer: String => String = url => s"page $url") extends HttpFetch {
    val asked   = new ConcurrentLinkedQueue[String]()
    val headers = new ConcurrentLinkedQueue[Map[String, String]]()
    override def get(url: String): String = { asked.add(url); answer(url) }
    override def get(url: String, sent: Map[String, String]): String = { headers.add(sent); get(url) }
    override def getBytes(url: String): Array[Byte] = get(url).getBytes("UTF-8")
    override def post(url: String, body: String, contentType: String): String = sys.error("never posted")
  }

  private def withRoots(test: (settings.FixtureRoot, settings.FixtureRoot) => Unit): Unit = {
    val held = Files.createTempDirectory("fill-held")
    val out  = Files.createTempDirectory("fill-out")
    try test(settings.FixtureRoot(held), settings.FixtureRoot(out))
    finally Seq(held, out).foreach(root => Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p)))
  }

  "a fill" should "write each page it fetches where a hermetic replay of the tree looks for it" in withRoots { (held, out) =>
    val live = new Live
    val page = "https://www.flicks.us/movie/toy-story-5/"
    val outcome = new MissingFixtureFill(MissingFixtureFill.heldIn(held, Tree), MissingFixtureFill.recordingInto(out, Tree, live), threads = 2)
      .fill(Seq(gap(page)), 1.minute)

    outcome.fetched shouldBe 1
    new clients.tools.FakeHttpFetch(Tree, strict = true, foldYear = false, root = out).get(page) shouldBe s"page $page"
    MissingFixtureFill.heldIn(out, Tree)(gap(page)) shouldBe true
  }

  // A page gone from the origin is a verdict the hermetic replay should give, as the recording would have:
  // remembered. A 503 says nothing about the page: the next leg's fill asks again.
  it should "remember a durable verdict, and only a durable one" in withRoots { (held, out) =>
    val gone  = "https://drafthouse.com/s/mother/v2/schedule/presentation/gone"
    val flaky = "https://drafthouse.com/s/mother/v2/schedule/presentation/flaky"
    val live  = new Live(url => throw new HttpStatusException(if (url == gone) 404 else 503, "GET", url, None))
    val outcome = new MissingFixtureFill(MissingFixtureFill.heldIn(held, Tree), MissingFixtureFill.recordingInto(out, Tree, live), threads = 1)
      .fill(Seq(gap(gone), gap(flaky)), 1.minute)

    outcome.failed shouldBe 2
    MissingFixtureFill.heldIn(out, Tree)(gap(gone)) shouldBe true
    MissingFixtureFill.heldIn(out, Tree)(gap(flaky)) shouldBe false
  }

  it should "never ask again what an earlier fill already holds" in withRoots { (held, out) =>
    val page = "https://www.flicks.us/movie/akira-1988/"
    MissingFixtureFill.recordingInto(held, Tree, new Live).get(page)
    val live = new Live
    val outcome = new MissingFixtureFill(MissingFixtureFill.heldIn(held, Tree), MissingFixtureFill.recordingInto(out, Tree, live), threads = 1)
      .fill(Seq(gap(page)), 1.minute)

    outcome.alreadyHeld shouldBe 1
    live.asked.asScala shouldBe empty
  }

  // The budget is what keeps a row's pre-suite fill inside its ~90 s: a request not started by then is the next leg's.
  it should "start nothing past its budget, and say how many it left" in {
    val now  = new AtomicLong(0L)
    val live = new Live(url => { now.addAndGet(40.seconds.toNanos); s"page $url" })
    val outcome = new MissingFixtureFill(_ => false, live, threads = 1, nanoTime = () => now.get)
      .fill((1 to 5).map(i => gap(s"https://www.flicks.us/movie/film-$i/")), 1.minute)

    outcome.fetched shouldBe 2
    outcome.unreached shouldBe 3
    outcome.describe should include("3 left for the next leg")
  }

  // Flicks is paced at 200 ms a request; listed first, it would hold every other host's pages behind it.
  "a fill's order" should "take the hosts in turn" in {
    val order = MissingFixtureFill.interleavedByHost(Seq(
      gap("https://www.flicks.us/movie/a/"), gap("https://www.flicks.us/movie/b/"), gap("https://www.flicks.us/movie/c/"),
      gap("https://drafthouse.com/x"), gap("https://drafthouse.com/y")))
    order.map(g => Path.of(java.net.URI.create(g.url).getPath).getFileName.toString) shouldBe Seq("x", "a", "y", "b", "c")
  }

  // A TMDB gap is listed with its key masked: the fill signs it again with the key it holds, as the TMDB client does —
  // query parameter and bearer header — and records the answer where a replay looks for it, which no key reaches. A
  // gap whose credential it does not hold is left for the recorder, never asked with the mask in it.
  it should "sign a masked request with the credential it holds, and leave one it holds none for" in withRoots { (held, out) =>
    val live   = new Live(url => s"""{"id":1018,"asked":"${url.length}"}""")
    val tmdb   = "https://api.themoviedb.org/3/movie/1018?language=pl-PL&api_key=***"
    val omdb   = "https://www.omdbapi.com/?t=A&apikey=***"
    val signer = FillCredentials(Some(settings.TmdbApiKey("k3y")))
    val outcome = new MissingFixtureFill(MissingFixtureFill.heldIn(held, Tree), MissingFixtureFill.recordingInto(out, Tree, live),
        threads = 1, sign = signer.sign).fill(Seq(gap(tmdb), gap(omdb)), 1.minute)

    live.asked.asScala.toSeq shouldBe Seq("https://api.themoviedb.org/3/movie/1018?language=pl-PL&api_key=k3y")
    live.headers.asScala.toSeq shouldBe Seq(clients.TmdbClient.authorization(settings.TmdbApiKey("k3y")))
    (outcome.fetched, outcome.unsigned) shouldBe (1, 1)
    MissingFixtureFill.heldIn(out, Tree)(gap(tmdb)) shouldBe true
    Files.walk(out.value).iterator().asScala.filter(Files.isRegularFile(_)).map(Files.readString).mkString should not include "k3y"
  }
}
