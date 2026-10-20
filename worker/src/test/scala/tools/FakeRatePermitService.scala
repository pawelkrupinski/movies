package tools

import com.sun.net.httpserver.HttpServer

import java.net.InetSocketAddress
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/**
 * The CI runner's rate-permit service (bitcashier infra, nix/files/forgejo-rate-permits) on a loopback port: it
 * grants every `GET /permit?service=<name>` at once (200), or answers the statuses `refuseFirst` lists first, in
 * order, and remembers the service each request asked for.
 */
final class FakeRatePermitService(refuseFirst: Int*) extends AutoCloseable {

  private val scripted = new ConcurrentLinkedQueue[Integer](refuseFirst.map(Int.box).asJava)
  private val asked    = new ConcurrentLinkedQueue[String]()
  private val server   = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)

  server.createContext(
    "/permit",
    exchange => {
      val service = Option(exchange.getRequestURI.getRawQuery).toSeq
        .flatMap(_.split('&'))
        .collectFirst { case s"service=$name" => java.net.URLDecoder.decode(name, java.nio.charset.StandardCharsets.UTF_8) }
        .getOrElse("")
      asked.add(service)
      val status = Option(scripted.poll()).map(_.intValue).getOrElse(200)
      exchange.sendResponseHeaders(status, -1)
      exchange.close()
    }
  )
  server.start()

  /** The base URL a job finds in `FORGEJO_RATE_PERMITS`. */
  val url: String = s"http://127.0.0.1:${server.getAddress.getPort}"

  /** Every service a permit was asked for, in order. */
  def services: Seq[String] = asked.asScala.toSeq

  override def close(): Unit = server.stop(0)
}
