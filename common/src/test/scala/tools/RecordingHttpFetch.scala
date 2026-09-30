package tools

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** The delegate a decorator spec wraps: answers each GET through `respond` — swappable, so a
 *  response can flip mid-test — and records every request that reached it, so the spec can
 *  assert what the decorator forwarded and what it answered on its own. Safe across threads.
 *
 *  A byte read is recorded as `bytes:<url>`, apart from a GET of the same URL, so a spec can
 *  tell a forwarded `getBytes` from a decorator that decoded a GET instead. */
final class RecordingHttpFetch(@volatile var respond: String => String = url => s"body of $url") extends GetOnlyHttpFetch {
  private val log = new ConcurrentLinkedQueue[String]()

  override def get(url: String): String = { log.add(url); respond(url) }

  override def getBytes(url: String): Array[Byte] = { log.add(s"bytes:$url"); respond(url).getBytes(UTF_8) }

  /** Every request that reached this delegate, in arrival order. */
  def requested: List[String] = log.asScala.toList

  /** How many requests reached this delegate. */
  def calls: Int = log.size
}
