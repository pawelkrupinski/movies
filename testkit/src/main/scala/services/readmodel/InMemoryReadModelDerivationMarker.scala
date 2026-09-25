package services.readmodel

import scala.util.{Failure, Success, Try}

/** In-memory [[ReadModelDerivationMarker]]: starts at `initial`, and `unreadable` makes the next
 *  reads fail, as a Mongo read that timed out does. */
class InMemoryReadModelDerivationMarker(initial: Option[String] = None) extends ReadModelDerivationMarker {
  @volatile private var version: Option[String] = initial
  @volatile var unreadable: Boolean              = false

  def recorded(): Try[Option[String]] =
    if (unreadable) Failure(new RuntimeException("derivation marker unreadable")) else Success(version)

  def record(version: String): Unit = this.version = Some(version)

  def current: Option[String] = version
}
