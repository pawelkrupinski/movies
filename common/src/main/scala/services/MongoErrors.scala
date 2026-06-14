package services

import com.mongodb.MongoWriteException

/** Shared classification of Mongo write errors. */
object MongoErrors {

  /** True when `exception` is a duplicate-key (11000) write error — an insert/upsert
   *  that lost the race for a unique `_id` to a concurrent writer. The basis of
   *  the distributed-claim idiom used by `MongoTaskQueue` and
   *  `services.schedule.MongoScheduledRunStore`. */
  def isDuplicateKey(exception: MongoWriteException): Boolean =
    Option(exception.getError).exists(_.getCode == 11000)
}
