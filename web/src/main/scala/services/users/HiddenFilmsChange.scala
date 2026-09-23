package services.users

/** One atomic change to ONE country's hidden-films bucket — what
 *  `UserStateRepository.changeHiddenFilms` applies to the stored row in a single
 *  step, so two overlapping requests for the same user (another tab, the app)
 *  can't erase each other's title the way a read-modify-write of the whole row
 *  could.
 *
 *  The rules — which title, what bound, which country — are the caller's
 *  (`UserStateController`); this only says what the store does with them.
 *  [[applyTo]] is that meaning in Scala: the in-memory store runs it directly,
 *  and `MongoUserStateRepository` states the same thing as an update pipeline
 *  (`UserStateWritesContract` holds both to it). */
sealed trait HiddenFilmsChange {

  /** The bucket after this change, or `None` when the change declines to
   *  apply — nothing is written then, `updatedAt` included. */
  def applyTo(bucket: Set[String]): Option[Set[String]]
}

object HiddenFilmsChange {

  /** Add `title`, unless it is new and the bucket already holds `bucketLimit`
   *  titles — then decline. Re-adding a title already there always applies. */
  final case class Hide(title: String, bucketLimit: Int) extends HiddenFilmsChange {
    // A zero bound would decline even on a user with no row yet, where the
    // Mongo upsert inserts regardless — keep the two stores' outcomes identical.
    require(bucketLimit > 0, s"bucketLimit must be positive, got $bucketLimit")
    def applyTo(bucket: Set[String]): Option[Set[String]] =
      Option.when(bucket.contains(title) || bucket.size < bucketLimit)(bucket + title)
  }

  final case class Unhide(title: String) extends HiddenFilmsChange {
    def applyTo(bucket: Set[String]): Option[Set[String]] = Some(bucket - title)
  }

  case object Clear extends HiddenFilmsChange {
    def applyTo(bucket: Set[String]): Option[Set[String]] = Some(Set.empty)
  }
}
