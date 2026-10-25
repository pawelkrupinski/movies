package services.identity.staging

import services.identity.{Question, TakeChange}

/**
 * What the identity system tells the staging screen AS IT HAPPENS (docs/design/identity-questions.md, "The staging
 * screen"), into the [[ChangeFeed]] its publisher drains: the agreement stage's waits starting and ending, and each
 * projection's change list. A cluster is named by its key (`AgreementStage.keyOf`), as the stage keeps it; times are
 * epoch millis.
 */
enum StagingEvent {
  /** The agreement stage began waiting on `cluster` at `at`, for `questions`: a new cluster's families asked inline, a
   *  cluster put to wait on its families' questions or its fill's TMDB answer, or a model take's correction waiting on
   *  its evidence. */
  case Waiting(cluster: String, at: Long, questions: Set[Question])
  /** The stage stopped waiting on `cluster` at `at`: every question it waited on answered or attempted, its take
   *  decided. */
  case Settled(cluster: String, at: Long)
  /** The stage's cluster `absorbed` merged into `cluster` at `at`, which kept the older key and took on the absorbed
   *  one's memories: a wait the absorbed cluster began is the merged cluster's, since it began. */
  case Merged(cluster: String, absorbed: String, at: Long)
  /** Projection number `projection` (counted from the worker's start), written at `at`, changed a listing's take (an
   *  entry of its change list, [[TakeChange]]). */
  case Changed(change: TakeChange, projection: Long, at: Long)
}
