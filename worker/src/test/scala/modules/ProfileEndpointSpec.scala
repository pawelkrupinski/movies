package modules

import com.sun.net.httpserver.HttpServer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.HeapDumpDirectory

import java.net.{HttpURLConnection, InetSocketAddress, URI}
import java.nio.file.Files
import scala.concurrent.duration._

/** `/profile` starts a flight recording of the running worker — the one way to see a production
 *  worker's CPU and allocation, since its JRE-only image lets nothing attach from outside. */
class ProfileEndpointSpec extends AnyFlatSpec with Matchers {

  private final class Recorded(answer: Either[String, String] = Right("/data/heapdumps/profile.jfr")) extends FlightRecorder {
    var asked = Seq.empty[FiniteDuration]
    def record(duration: FiniteDuration): Either[String, String] = { asked :+= duration; answer }
  }

  private def call(recorder: FlightRecorder, method: String, query: String = ""): (Int, String) = {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    WorkerMain.addProfileEndpoint(server, recorder)
    server.start()
    try {
      val c = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/profile$query").toURL.openConnection().asInstanceOf[HttpURLConnection]
      c.setRequestMethod(method)
      val status = c.getResponseCode
      val stream = Option(if (status < 400) c.getInputStream else c.getErrorStream)
      (status, stream.map(s => new String(s.readAllBytes(), "UTF-8")).getOrElse(""))
    } finally server.stop(0)
  }

  "/profile" should "start a ten-minute recording on a bare POST, and name the file it will write" in {
    val recorder = new Recorded
    call(recorder, "POST") shouldBe ((202, "recording 600 s to /data/heapdumps/profile.jfr"))
    recorder.asked shouldBe Seq(10.minutes)
  }

  it should "record for the seconds asked, within one minute and half an hour" in {
    val recorder = new Recorded
    call(recorder, "POST", "?seconds=120")._1 shouldBe 202
    call(recorder, "POST", "?seconds=30")._1 shouldBe 400
    call(recorder, "POST", "?seconds=3600")._1 shouldBe 400
    recorder.asked shouldBe Seq(2.minutes)
  }

  it should "refuse a GET, and say when a recording is already running" in {
    call(new Recorded, "GET")._1 shouldBe 405
    call(new Recorded(Left("a recording is already running")), "POST") shouldBe ((409, "a recording is already running"))
  }

  "JfrFlightRecorder" should "write its recording when it ends, and refuse a second while it runs" in {
    val dir      = HeapDumpDirectory(Files.createTempDirectory("profile"))
    val recorder = new JfrFlightRecorder(dir)
    val file     = recorder.record(1.second).toOption.get
    recorder.record(1.second) shouldBe Left("a recording is already running")
    val deadline = System.nanoTime() + 20.seconds.toNanos
    while (!(Files.exists(java.nio.file.Path.of(file)) && Files.size(java.nio.file.Path.of(file)) > 0) && System.nanoTime() < deadline) Thread.sleep(200)
    Files.size(java.nio.file.Path.of(file)) should be > 0L
  }
}
