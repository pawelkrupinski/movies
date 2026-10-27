package services.identity

/** Pins held in memory, each written as [[MongoPinStore]] stores it to BSON and read back — for tests. */
final class InMemoryPinStore extends PinStore {
  private val pins = new java.util.concurrent.ConcurrentHashMap[String, Pin]()
  def all(): Seq[Pin]              = { import scala.jdk.CollectionConverters._; pins.values.asScala.toSeq }
  def insert(pin: Pin): Unit = {
    val stored = MongoPinStore.decode(services.BsonRoundTrip(MongoPinStore.encode(pin)))
      .getOrElse(throw new IllegalStateException(s"pin ${pin.id} does not read back as stored"))
    pins.put(stored.id, stored); ()
  }
  def delete(id: String): Boolean  = pins.remove(id) != null
}
