package services.facebook

import java.time.Instant

/** [[FacebookFetches]] in a map — what a spec's web recorder and worker drain share, the way
 *  the fleet shares the Mongo collection. Same contract, pinned by `FacebookFetchesContractSpec`. */
class InMemoryFacebookFetches extends FacebookFetches {
  private val fetches = collection.mutable.Map.empty[String, Instant]

  def record(fetched: Map[String, Instant]): Unit = synchronized {
    fetched.foreach { case (url, at) => fetches(url) = fetches.get(url).filter(_.isAfter(at)).getOrElse(at) }
  }

  def fetchedSince(urls: Seq[String], since: Instant): Set[String] = synchronized {
    urls.filter(url => fetches.get(url).exists(!_.isBefore(since))).toSet
  }

  /** Every page recorded, with its latest fetch. */
  def recorded: Map[String, Instant] = synchronized(fetches.toMap)
}
