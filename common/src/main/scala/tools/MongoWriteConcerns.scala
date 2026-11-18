package tools

import org.mongodb.scala.WriteConcern

/** The write concerns the stores share, kept beside the other Mongo helpers so a store needs no other store's
 *  package for one. */
object MongoWriteConcerns {

  /** `{w:1, j:false}` — ack on primary apply, no journal-fsync/majority wait. For recoverable bookkeeping (the task
   *  queue, freshness, cadence, resolution and attempt ledgers): the durability trade buys back the per-op cost that
   *  dominated the shared-cpu Mongo's load. See [[services.tasks.MongoTaskQueue]]'s class document. */
  val Bookkeeping: WriteConcern = WriteConcern.W1.withJournal(false)
}
