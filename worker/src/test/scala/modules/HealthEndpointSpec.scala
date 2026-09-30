package modules

import com.sun.net.httpserver.HttpServer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{HttpURLConnection, InetSocketAddress, URI}

/**
 * `/health` is up before the wiring is, so it must answer "alive" while the process
 * boots and then report the fleet's real liveness. The switch is a [[BootLiveness]]
 * `main` owns — one per boot, so a spec's (or a second server's) swap reaches only its own.
 */
class HealthEndpointSpec extends AnyFlatSpec with Matchers {

  private def status(liveness: BootLiveness): Int = {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    WorkerMain.addHealthEndpoint(server, liveness)
    server.start()
    try {
      val c = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/health").toURL
        .openConnection().asInstanceOf[HttpURLConnection]
      c.getResponseCode
    } finally server.stop(0)
  }

  "/health" should "answer alive while the process is still booting" in {
    status(new BootLiveness) shouldBe 200
  }

  it should "report the real probe once it has been handed one" in {
    val liveness = new BootLiveness
    liveness.becomes(() => false)
    status(liveness) shouldBe 503
  }

  "A boot's liveness" should "not be swapped by another boot's" in {
    val wedged = new BootLiveness
    wedged.becomes(() => false)
    new BootLiveness().isAlive shouldBe true
  }

  private def readyStatus(readiness: BootReadiness): Int = {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    WorkerMain.addReadyEndpoint(server, readiness)
    server.start()
    try {
      val c = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/ready").toURL
        .openConnection().asInstanceOf[HttpURLConnection]
      c.getResponseCode
    } finally server.stop(0)
  }

  // The next workers roll out only once these are ready: five booting at once saturated the node.
  "/ready" should "answer booting until the boot work has settled, then ready" in {
    val readiness = new BootReadiness(scala.concurrent.duration.Duration(8, "minutes"))
    var settled   = false
    readyStatus(readiness) shouldBe 503
    readiness.becomes(() => settled)
    readyStatus(readiness) shouldBe 503
    settled = true
    readyStatus(readiness) shouldBe 200
  }

  it should "answer ready once its cap has passed, settled or not, so a stuck step cannot hold every rollout" in {
    var now       = 0L
    val readiness = new BootReadiness(scala.concurrent.duration.Duration(8, "minutes"), new tools.Stopwatch(() => now))
    readiness.becomes(() => false)
    readyStatus(readiness) shouldBe 503
    now = scala.concurrent.duration.Duration(8, "minutes").toNanos
    readyStatus(readiness) shouldBe 200
  }
}
