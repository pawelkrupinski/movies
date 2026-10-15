package services.identity

/** Pins held in memory — for tests: every rule lives in [[Pins]], above the store. */
final class InMemoryPinStore extends PinStore {
  private val pins = new java.util.concurrent.ConcurrentHashMap[String, Pin]()
  def all(): Seq[Pin]              = { import scala.jdk.CollectionConverters._; pins.values.asScala.toSeq }
  def insert(pin: Pin): Unit       = { pins.put(pin.id, pin); () }
  def delete(id: String): Boolean  = pins.remove(id) != null
}
