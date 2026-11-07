package clients.tools

import tools.HttpFetch

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** Pass-through `HttpFetch` that logs every request it forwards to `delegate`, so a
 *  test can assert WHICH URLs a client asked for, in what order, and how often — over
 *  recorded fixtures (`FakeHttpFetch`), a constant body, or any other fake.
 *
 *  The log is a concurrent queue: clients fan detail fetches across a pool, and a
 *  `ListBuffer` append from several threads loses entries (see `RoutingHttpFetch`). */
class RequestLogHttpFetch(delegate: HttpFetch) extends HttpFetch {
  private val log = new ConcurrentLinkedQueue[(String, String)]()

  /** Every `(method, url)` forwarded, in call order. A snapshot — read it after the
   *  work under test has finished. */
  def calls: Seq[(String, String)] = log.asScala.toSeq

  /** The URLs of every GET (string or bytes), in call order. */
  def gets: Seq[String] = calls.collect { case ("GET", url) => url }

  /** The URLs of every POST, in call order. */
  def posts: Seq[String] = calls.collect { case ("POST", url) => url }

  override def get(url: String): String = { log.add("GET" -> url); delegate.get(url) }

  override def get(url: String, headers: Map[String, String]): String = {
    log.add("GET" -> url); delegate.get(url, headers)
  }

  override def getBytes(url: String): Array[Byte] = { log.add("GET" -> url); delegate.getBytes(url) }

  override def post(url: String, body: String, contentType: String): String = {
    log.add("POST" -> url); delegate.post(url, body, contentType)
  }
}
