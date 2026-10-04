package services

/**
 * What every repository wired at boot holds of a Mongo connection: the database as it stood THEN.
 *
 * Repositories take `connection.database` — an `Option[MongoDatabase]` — once, at construction. A
 * required connection that found Mongo unreachable at boot starts degraded (rather than crash-loop,
 * see [[MongoConnection]]) and later publishes the database from a background reconnect, but nothing
 * wired before that ever reads it again: each repository keeps its `None` and answers as a no-op
 * store — empty reads, dropped writes — for the life of the process. The web's read model read that
 * as a complete, empty corpus and reported the pod ready serving no films; the reconnect's
 * "RECOVERED" changed nothing until something happened to restart the process.
 *
 * So the process says so: not ready while it is bound degraded, and not ALIVE once the database is
 * back — a liveness probe restarts it onto the live database. While Mongo stays down it is alive,
 * so an outage does not become a crash loop.
 */
trait DatabaseBinding {
  /** Boot found Mongo unreachable: what was wired then holds no database. */
  def boundDegraded: Boolean
  /** The database answered since, but only a restart rebinds what was wired at boot. */
  def restartRequired: Boolean
}

object DatabaseBinding {
  /** Whether none of `bindings` left the process holding a no-op store. */
  def allBound(bindings: Seq[DatabaseBinding]): Boolean = !bindings.exists(_.boundDegraded)
  /** Whether any of `bindings` recovered under a process that cannot use it. */
  def anyRestartRequired(bindings: Seq[DatabaseBinding]): Boolean = bindings.exists(_.restartRequired)

  /** A binding for a test or a wiring with no Mongo: never degraded. */
  val Bound: DatabaseBinding = new DatabaseBinding {
    def boundDegraded: Boolean   = false
    def restartRequired: Boolean = false
  }
}
