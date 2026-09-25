package services.events

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/** An `EventBus` that dispatches nowhere and remembers what was published, so a spec
 *  can assert that a component announced an event (or stayed quiet) without wiring the
 *  subscribers that would react to it. */
final class RecordingEventBus extends EventBus {
  private val log = new ConcurrentLinkedQueue[DomainEvent]()

  /** Every published event, in publish order. */
  def published: Seq[DomainEvent] = log.asScala.toSeq

  def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = ()
  def publish(event: DomainEvent): Unit = { log.add(event); () }
}
