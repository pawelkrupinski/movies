package modules

import com.sun.net.httpserver.HttpServer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{HttpURLConnection, InetSocketAddress, URI}

/**
 * `/heapdump` exists because the only heap dumps this worker ever produced were the
 * ones `-XX:+HeapDumpOnOutOfMemoryError` wrote as it died — so a worker running merely
 * hot could never be inspected, and the box is JRE-only, with no jcmd/jmap to attach.
 *
 * The dump itself is [[tools.HeapDumper]]'s job and is injected here: a spec must not
 * stop the world and write a few hundred MB of the test JVM's own heap to disk.
 */
class HeapDumpEndpointSpec extends AnyFlatSpec with Matchers {

  private def withEndpoint(dump: String => Option[String])(body: String => Unit): Unit = {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    WorkerMain.addHeapDumpEndpoint(server, "/tmp/does-not-matter", dump)
    server.start()
    try body(s"http://127.0.0.1:${server.getAddress.getPort}/heapdump")
    finally server.stop(0)
  }

  private def call(url: String, method: String): (Int, String) = {
    val c = URI.create(url).toURL.openConnection().asInstanceOf[HttpURLConnection]
    c.setRequestMethod(method)
    if (method == "POST") { c.setDoOutput(true); c.getOutputStream.close() }
    val status = c.getResponseCode
    val stream = if (status < 400) c.getInputStream else c.getErrorStream
    val text   = new String(stream.readAllBytes(), "UTF-8")
    (status, text)
  }

  "POST /heapdump" should "take a dump and report where it landed" in {
    var askedFor: Option[String] = None
    withEndpoint({ dir => askedFor = Some(dir); Some(s"$dir/wedge-1.hprof") }) { url =>
      val (status, body) = call(url, "POST")
      status shouldBe 200
      body   should include ("wedge-1.hprof")
    }
    askedFor shouldBe Some("/tmp/does-not-matter")
  }

  // A dump stops the world for a full GC and writes hundreds of MB. A health-checker,
  // a crawler or a link-prefetch must not be able to trigger that by accident.
  it should "refuse a GET rather than dumping" in {
    var dumped = false
    withEndpoint({ _ => dumped = true; Some("/x.hprof") }) { url =>
      val (status, _) = call(url, "GET")
      status shouldBe 405
    }
    dumped shouldBe false
  }

  it should "report a failed dump as a server error, not a success" in {
    withEndpoint(_ => None) { url =>
      val (status, body) = call(url, "POST")
      status shouldBe 500
      body   should include ("failed")
    }
  }
}
