package controllers

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Sink
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers
import services.UptimeMonitor

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The `/uptime/stream` SSE feed must NOT degrade as the cinema roster grows. A
 * worker poll can flip one bucket per active service in a single cycle; the old
 * feed pushed one SSE frame per changed bucket, so the page took N parses + N
 * DOM passes per poll for N scrapers. `eventSource` now coalesces a burst into
 * one batched `data: [...]` frame. These lock that the frame count tracks the
 * batch *window*, not the number of services.
 */
class UptimeStreamSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private implicit val sys: ActorSystem  = ActorSystem("uptime-stream-spec")
  private implicit val mat: Materializer = Materializer(sys)

  override def afterAll(): Unit = Await.result(sys.terminate(), 10.seconds)

  private def controller(monitor: UptimeMonitor) =
    new UptimeController(Helpers.stubControllerComponents(), monitor)

  private def servicesIn(frame: String): Seq[String] =
    Json.parse(frame.stripPrefix("data: ").trim).as[List[JsObject]].map(o => (o \ "service").as[String])

  "the uptime SSE stream" should "coalesce a burst of per-service updates into a handful of frames, not one per service" in {
    val monitor = new UptimeMonitor()
    val collecting = controller(monitor).eventSource().takeWithin(1500.millis).runWith(Sink.seq)

    Thread.sleep(100) // let the stream materialize before we record
    val n = 100
    (1 to n).foreach(i => monitor.recordSuccess(s"svc-$i"))

    val frames = Await.result(collecting, 5.seconds)

    // The whole point: ~100 changes arrive as a couple of frames, never ~100.
    frames.size should be < 10
    frames.foreach(_ should startWith ("data: ["))

    // …yet every service still surfaces exactly once across the batched frames.
    val services = frames.flatMap(servicesIn)
    services.size shouldBe n
    services.toSet shouldBe (1 to n).map(i => s"svc-$i").toSet
  }

  it should "emit nothing while idle — no empty-batch spam" in {
    val monitor = new UptimeMonitor()
    val frames = Await.result(
      controller(monitor).eventSource().takeWithin(700.millis).runWith(Sink.seq), 3.seconds)
    frames shouldBe empty
  }

  it should "deliver a lone update as a one-element batch frame" in {
    val monitor = new UptimeMonitor()
    val collecting = controller(monitor).eventSource().takeWithin(1.second).runWith(Sink.seq)

    Thread.sleep(100)
    monitor.recordFailure("TMDB", "boom")

    val frames = Await.result(collecting, 3.seconds)
    frames should have size 1
    val updates = Json.parse(frames.head.stripPrefix("data: ").trim).as[List[JsObject]]
    updates should have size 1
    (updates.head \ "service").as[String] shouldBe "TMDB"
    (updates.head \ "status").as[String] shouldBe "red"
  }
}
