package services

import org.mongodb.scala.MongoCollection

import scala.concurrent.duration.Duration

/** The way out of the client's operation timeout ([[MongoConnection.DefaultOperationTimeout]]) for
 *  the operations that legitimately outlive it — change streams and index builds. */
object OperationTimeoutOptOut {

  extension [T](collection: MongoCollection[T])
    /** This collection view without an operation timeout (`timeoutMS=0`): the operation runs as long
     *  as it needs to, as every operation did before the client had one. */
    def withoutOperationTimeout: MongoCollection[T] = collection.withTimeout(Duration.Zero)
}
