package services.identity

import scala.concurrent.duration.FiniteDuration

/**
 * Which NEW listings the identity model waits for before taking them in: one whose venue detail page
 * (director, runtime, original title) is not read yet, so its first resolve is not a title-only guess
 * the page would later move it off. The page is asked for once, and the listing is taken in when the
 * page is read or gone — or after `limit`, so a page that never answers cannot hide a film.
 */
trait PageWait {
  /** Is `listing`'s venue page still unread? */
  def awaiting(listing: Listing): Boolean
  /** Ask for `listing`'s venue page to be read. */
  def request(listing: Listing): Unit
  /** How long a listing waits at most. */
  def limit: FiniteDuration
}

object PageWait {
  /** Nothing waits: the shadow run, and any country whose listings are not the model's to serve. */
  val Never: PageWait = new PageWait {
    def awaiting(listing: Listing): Boolean = false
    def request(listing: Listing): Unit     = ()
    val limit: FiniteDuration               = FiniteDuration(0, "seconds")
  }
}
