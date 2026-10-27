package services.identity

import services.movies.ListingKey

import java.time.Instant

/** What a pin asserts about its listings. */
enum PinClaim {
  /** The listings ARE this TMDB film: overrides their lookups, and must-links them to each other
   *  and to every other listing pinned to the same film. */
  case IsFilm(tmdbId: Int)
  /** The listings are ONE film, whichever it resolves to. */
  case SameFilm
  /** The listings are NEVER this TMDB film: drops that answer from their lookups, and
   *  cannot-links them from every listing that is that film. */
  case NeverFilm(tmdbId: Int)
}

/**
 * An admin's assertion about film identity — the emergency escape hatch for a case the
 * evidence cannot decide (docs/design/identity-resolver.md, "Phase 3: curation"). Not a
 * workflow: the resolver is expected to be right without pins, and a pin is data the resolver
 * reads as a hard constraint (`PinConstraints`), never a patch to stored state.
 *
 * Its [[id]] is its CONTENT (the claim over the sorted listing set), so re-asserting a pin is a
 * no-op, not a second pin, and the id is the same on every replica.
 */
final case class Pin(listings: Seq[ListingKey], claim: PinClaim, author: String, reason: String, createdAt: Instant) {
  lazy val id: String = Pin.idOf(listings, claim)
}

object Pin {
  def idOf(listings: Seq[ListingKey], claim: PinClaim): String = {
    // each key by its stored form, never its toString: a decoded key's directors come back in whatever collection the
    // decoder built, and "List()" stored against "Vector()" read gave the same pin two ids
    val canonical = (claim.toString +: listings.distinct.sorted.map(ListingKey.serialised)).mkString("\u0000")
    java.security.MessageDigest.getInstance("SHA-256").digest(canonical.getBytes("UTF-8"))
      .take(8).map(b => f"${b & 0xff}%02x").mkString
  }
}

/** Where pins are kept. */
trait PinStore {
  def all(): Seq[Pin]
  def insert(pin: Pin): Unit
  /** Whether a pin with this id was there to delete. */
  def delete(id: String): Boolean
}
