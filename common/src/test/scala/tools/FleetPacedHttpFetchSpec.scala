package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

/** A host every country's worker shares is held to one pace across the fleet: a slot within the horizon is waited for,
 *  one further off turns the request back — the breaker's fast-fail, naming when to come back — and the worker's other
 *  hosts are never paced by it. */
class FleetPacedHttpFetchSpec extends AnyFlatSpec with Matchers {
  private final class World {
    var at     = Instant.parse("2026-10-04T21:00:00Z")
    val slept  = scala.collection.mutable.ArrayBuffer.empty[Long]
    val pace   = new InMemoryFleetHostPace
    val leaf   = new HttpFetch {
      override def get(url: String): String = "ok"
      override def post(url: String, body: String, contentType: String): String = "ok"
    }
    /** One country's worker's fetch: its own decorator, the fleet's one pace. */
    def worker(): FleetPacedHttpFetch = new FleetPacedHttpFetch(leaf, pace,
      url => Option.when(url.contains("wikidata"))(500.millis), horizon = 2.seconds, now = () => at, sleep = ms => { slept += ms; () })
  }
  private val wikidata = "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=Q1"

  "a fleet-paced host" should "be paced across every worker sharing the fleet's pace, a slot within the horizon waited for" in {
    val w = new World
    val (pl, uk) = (w.worker(), w.worker())
    pl.get(wikidata); uk.get(wikidata); pl.get(wikidata)
    w.slept.toSeq shouldBe Seq(500L, 1000L)   // the first at once, then a slot every 500 ms — whichever worker asks
  }

  it should "turn a request back, naming when to come, once the next slot is past the horizon" in {
    val w = new World
    val pl = w.worker()
    (1 to 5).foreach(_ => pl.get(wikidata))   // slots to +2.0 s taken
    val back = intercept[CircuitOpenException](pl.get(wikidata))
    (back.host, back.openForMs) shouldBe (("www.wikidata.org", 2500L))
    w.at = w.at.plusMillis(2500)
    pl.get(wikidata) shouldBe "ok"            // its time come, it is served
  }

  it should "pace no other host" in {
    val w = new World
    (1 to 20).foreach(_ => w.worker().get("https://www.metacritic.com/movie/dune/"))
    w.slept shouldBe empty
  }
}
