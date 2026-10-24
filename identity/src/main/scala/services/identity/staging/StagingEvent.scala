package services.identity.staging

import services.identity.{Question, TakeChange}

/**
 * What the identity system tells the staging screen AS IT HAPPENS (docs/design/identity-questions.md, "The staging
 * screen"), into the [[ChangeFeed]] its publisher drains: the agreement stage's waits starting and ending, and each
 * projection's change list. A cluster is named by its id (`StoredFamily.idOf`, its smallest listing), as the stage keeps
 * it; times are epoch millis.
 */
enum StagingEvent {
  /** The agreement stage began waiting on `cluster` at `at`, for `questions`: a new cluster's families asked inline, a
   *  cluster put to wait on its families' questions or its fill's TMDB answer, or a model take's correction waiting on
   *  its evidence. */
  case Waiting(cluster: String, at: Long, questions: Set[Question])
  /** The stage stopped waiting on `cluster` at `at`: every question it waited on answered or attempted, its take
   *  decided. */
  case Settled(cluster: String, at: Long)
  /** A written projection changed a listing's take (its change list, [[TakeChange]]). */
  case Changed(change: TakeChange)
}
