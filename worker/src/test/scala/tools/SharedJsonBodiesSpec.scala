package tools

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import services.identity.{InMemoryTmdbDocuments, NormalizingHttpFetch, TmdbKind, TmdbNormalizer, TmdbStore}

import java.util.concurrent.atomic.AtomicInteger

/** An order-independence replay's passes read the same TMDB bodies side by side: shared, each body is
 *  parsed once between them, and every pass reads exactly what it would have parsed for itself. */
class SharedJsonBodiesSpec extends AnyFlatSpec with Matchers {

  private final class Counted(budget: Long = SharedJsonBodies.Budget) {
    val parses = new AtomicInteger
    val bodies = new SharedJsonBodies(budget, body => { parses.incrementAndGet(); Json.parse(body) })
  }

  private val body = """{"id":1018,"title":"Mulholland Drive","credits":{"crew":[{"job":"Director","name":"David Lynch"}]}}"""
  /** The body as another pass reads it: its own string, read from its own copy of the fixture. */
  private def anotherPassOf(s: String) = new String(s.toCharArray)

  "shared JSON bodies" should "parse a body once for every pass that reads it, each from its own string" in {
    val c      = new Counted
    val passes = (1 to 3).map(_ => anotherPassOf(body))
    val trees  = passes.par(c.bodies.parse)
    c.parses.get shouldBe 1
    trees.foreach(_ should be theSameInstanceAs trees.head)
    trees.head shouldBe new JsonBodies().parse(body)
  }

  it should "give every body the tree the unshared parse gives it" in {
    val c = new Counted
    val samples = Seq(body, """{"results":[]}""", """{"crew":[]}""", """[1,2.5,"x",null,true]""", """{"popularity":8.8983,"a":{"b":[{}]}}""")
    samples.map(c.bodies.parse) shouldBe samples.map(new JsonBodies().parse)
    samples.map(s => c.bodies.parse(anotherPassOf(s))) shouldBe samples.map(Json.parse)
    c.parses.get shouldBe samples.size
  }

  // What one pass derives from a shared tree is its own: a parsed tree is immutable, so nothing a pass
  // does with it reaches the next pass to read the body.
  it should "let no pass change the tree another pass reads" in {
    val c     = new Counted
    val first = c.bodies.parse(body)
    val edited = first.as[JsObject] + ("title" -> JsString("Changed")) - "credits"
    edited should not be first
    val next = c.bodies.parse(anotherPassOf(body))
    next should be theSameInstanceAs first
    next shouldBe Json.parse(body)
    (next \ "title").as[String] shouldBe "Mulholland Drive"
  }

  // Bounded, so only what the passes are reading now is held — and a body evicted and read again is
  // parsed again into an equal tree, so whether a parse was shared never shows in what a pass reads.
  it should "hold bodies only up to its budget, and parse an evicted one again into an equal tree" in {
    val many = (1000 to 1039).map(id => s"""{"id":$id,"title":"Film $id"}""")
    val c    = new Counted(budget = 3L * many.head.length)
    many.foreach(c.bodies.parse); many.foreach(c.bodies.parse)
    c.bodies.held should be <= 3L * many.head.length
    many.map(s => c.bodies.parse(anotherPassOf(s))) shouldBe many.map(Json.parse)
    c.parses.get should be > many.size
  }

  // The seam the replay shares it through: the identity store's normalizer and the TMDB client of each
  // pass, over that pass's own store. Two passes file the very documents, and read the very record,
  // that a pass parsing for itself does — and each body is parsed once between them.
  it should "file and answer, through the normalizer and the client, exactly what an unshared pass does" in {
    val film = 1018
    def pass(bodies: JsonBodies) = {
      val docs     = new InMemoryTmdbDocuments
      val store    = new TmdbStore(docs, new MutableClock(TestWiring.FixedInstant))
      val observed = new NormalizingHttpFetch(new FakeHttpFetch("08-06-2026", strict = true), new TmdbNormalizer(store, bodies))
      val client   = new TmdbClient(observed, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => (), bodies = bodies)
      (client.identityRecord(film), docs.get(TmdbKind.Film, Seq(film.toString)))
    }
    val unshared = pass(new JsonBodies)
    unshared._1 shouldBe defined
    val c = new Counted
    Seq(pass(c.bodies), pass(c.bodies)) shouldBe Seq(unshared, unshared)
    c.parses.get shouldBe 2   // the record's two responses, once each for both passes
  }

  extension (passes: Seq[String])
    private def par(parse: String => JsValue): Seq[JsValue] = {
      val trees   = new Array[JsValue](passes.size)
      val threads = passes.zipWithIndex.map { case (s, i) => new Thread(() => trees(i) = parse(s)) }
      threads.foreach(_.start()); threads.foreach(_.join())
      trees.toSeq
    }
}
