package tools

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/** The hosts in a spec's memory, each written through the Mongo store's own codec and read back
 *  ([[tools.BsonRoundTrip]]), so it holds what `http_unrecovered_hosts` would. A test double: production keeps
 *  them in Mongo ([[MongoUnrecoveredHosts]]) or, with no database, nowhere ([[UnrecoveredHosts.none]]). */
final class InMemoryUnrecoveredHosts extends UnrecoveredHosts {
  private val hosts = new ConcurrentHashMap[String, UnrecoveredHosts.Host]()
  def load(): Seq[UnrecoveredHosts.Host] = hosts.values.asScala.toSeq.sortBy(_.host)
  def failed(host: UnrecoveredHosts.Host): Unit = {
    MongoUnrecoveredHosts.decode(tools.BsonRoundTrip(MongoUnrecoveredHosts.encode("memory", host)))
      .foreach(kept => hosts.put(kept.host, kept))
  }
  def recovered(host: String): Unit = { hosts.remove(host); () }
}
