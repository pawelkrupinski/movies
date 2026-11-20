package services.identity.agreement

import services.identity.vocabulary.Question

/** A cluster (or a take's correction) waiting on answers: its listings' digest and the questions it waits on — none where
 *  only its fill's TMDB answers were missing (`blindAt`), resolved again once something is filed, and until then read as
 *  the verdict that resolve gave (`read`). */
private[agreement] final case class ClusterWait(listings: Long, gaps: Set[Question], blindAt: Option[Long] = None,
                                               read: Option[StoredVerdict] = None)
