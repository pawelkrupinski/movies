package services.readmodel

import scala.util.{Failure, Success, Try}

/** In-memory [[ReadModelDerivationMarker]]: starts at `initial`, and `unreadable` makes the next
 *  reads fail, as a Mongo read that timed out does. What is recorded is kept as
 *  [[MongoReadModelDerivationMarker]] stores it, written to BSON and read back. */
class InMemoryReadModelDerivationMarker(initial: Option[String] = None,
                                        initialProgress: Option[DerivationProgress] = None) extends ReadModelDerivationMarker {
  @volatile private var version: Option[DerivationVersion]   = initial.map(DerivationVersion(_))
  @volatile private var pass: Option[DerivationProgress]     = initialProgress
  @volatile var unreadable: Boolean                          = false

  def recorded(): Try[Option[DerivationVersion]] = readable(version)
  def record(version: DerivationVersion): Unit   =
    this.version = MongoReadModelDerivationMarker.versionOf(services.BsonRoundTrip(MongoReadModelDerivationMarker.encode(version)))

  def progress(): Try[Option[DerivationProgress]]        = readable(pass)
  def recordProgress(progress: DerivationProgress): Unit =
    pass = Some(MongoReadModelDerivationMarker.progressOf(services.BsonRoundTrip(MongoReadModelDerivationMarker.encode(progress))))

  def current: Option[String]                  = version.map(_.value)
  def currentProgress: Option[DerivationProgress] = pass

  private def readable[A](value: A): Try[A] =
    if (unreadable) Failure(new RuntimeException("derivation marker unreadable")) else Success(value)
}
