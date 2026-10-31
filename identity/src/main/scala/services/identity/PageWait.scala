package services.identity

/**
 * Which NEW listings' venue detail pages (director, runtime, original title) are not read yet: each is asked
 * for once as its listing is taken in. The listing is decided at once on what is known, its page read as no
 * detail, and its take is Pending on the page (`ResolverDecision.awaits`) until it is read or gone (step 6,
 * the owner 2026-10-31: decide from the inputs as they stand; it was held out of the model before).
 */
trait PageWait {
  /** Is `listing`'s venue page still unread? */
  def awaiting(listing: Listing): Boolean
  /** Ask for `listing`'s venue page to be read. */
  def request(listing: Listing): Unit
}

object PageWait {
  /** Nothing waits: a model run without venue pages to read (specs, offline harnesses). */
  val Never: PageWait = new PageWait {
    def awaiting(listing: Listing): Boolean = false
    def request(listing: Listing): Unit     = ()
  }
}
