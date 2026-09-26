package services.identity

import services.movies.ListingKey

import java.time.{Clock, Instant}

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
 * reads as a hard constraint (`ListingConstraints.pinned`), never a patch to stored state.
 *
 * Its [[id]] is its CONTENT (the claim over the sorted listing set), so re-asserting a pin is a
 * no-op, not a second pin, and the id is the same on every replica.
 */
final case class Pin(listings: Seq[ListingKey], claim: PinClaim, author: String, reason: String, createdAt: Instant) {
  lazy val id: String = Pin.idOf(listings, claim)
}

object Pin {
  def idOf(listings: Seq[ListingKey], claim: PinClaim): String = {
    val canonical = (claim.toString +: listings.distinct.sorted.map(_.toString)).mkString("\u0000")
    java.security.MessageDigest.getInstance("SHA-256").digest(canonical.getBytes("UTF-8"))
      .take(8).map(b => f"${b & 0xff}%02x").mkString
  }
}

/** Where pins are kept — storage only; every rule lives in [[Pins]]. */
trait PinStore {
  def all(): Seq[Pin]
  def insert(pin: Pin): Unit
  /** Whether a pin with this id was there to delete. */
  def delete(id: String): Boolean
}

final class InMemoryPinStore extends PinStore {
  private val pins = new java.util.concurrent.ConcurrentHashMap[String, Pin]()
  def all(): Seq[Pin]              = { import scala.jdk.CollectionConverters._; pins.values.asScala.toSeq }
  def insert(pin: Pin): Unit       = { pins.put(pin.id, pin); () }
  def delete(id: String): Boolean  = pins.remove(id) != null
}

/** The pin rules above the store: what a pin must carry, and that the pin set never
 *  contradicts itself (a listing pinned to two films, or to a film it is pinned never to be). */
final class Pins(store: PinStore, clock: Clock) {

  /** Every pin, oldest first. */
  def all(): Seq[Pin] = store.all().sortBy(p => (p.createdAt, p.id))

  def constraints(): PinConstraints = PinConstraints(all())

  def add(listings: Seq[ListingKey], claim: PinClaim, author: String, reason: String): Either[String, Pin] = {
    val keys = listings.distinct.sorted
    val pin  = Pin(keys, claim, author.trim, reason.trim, clock.instant())
    val held = all()
    val malformed = Seq(
      Option.when(keys.isEmpty)("a pin needs at least one listing"),
      Option.when(claim == PinClaim.SameFilm && keys.sizeIs < 2)("a same-film pin needs two or more listings"),
      claim match {
        case PinClaim.IsFilm(id) if id <= 0    => Some(s"not a TMDB id: $id")
        case PinClaim.NeverFilm(id) if id <= 0 => Some(s"not a TMDB id: $id")
        case _                                 => None
      },
      Option.when(pin.author.isEmpty)("a pin needs its author"),
      Option.when(pin.reason.isEmpty)("a pin needs its reason"),
      Option.when(held.exists(_.id == pin.id))(s"already pinned as ${pin.id}")).flatten
    malformed.headOption.orElse(PinConstraints(held :+ pin).conflicts.headOption) match {
      case Some(refusal) => Left(refusal)
      case None          => store.insert(pin); Right(pin)
    }
  }

  def remove(id: String): Boolean = store.delete(id)
}
