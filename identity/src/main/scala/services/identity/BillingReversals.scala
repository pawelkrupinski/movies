package services.identity

/**
 * The BILLINGS whose takes a projection reversed ([[TakeReversals]]): a title many venues bill flips at every one of them
 * at once, so a reversal counted per listing says one bug a hundred times, and two rules undoing each other on one
 * billing hides among a country's listings. UK "RBO Cinema Season 2026-27: Macbeth": 57 listings lost the Met's record at
 * every worker boot and took it back four minutes later; "The Ties That Bind Us" flipped once a pod start. Each [[note]]
 * names every billing a projection reversed, once, by its title folded (case, accents and punctuation aside), cut to
 * [[BillingReversals.LabelLength]] characters.
 *
 * Its names are metric labels, so they are BOUNDED: the [[capacity]] billings that reversed most recently are kept; one
 * past it forgets the least recent, named in [[BillingReversals.Noted.forgotten]] for its series to be dropped.
 */
final class BillingReversals(capacity: Int = BillingReversals.Capacity) {
  private val held = new java.util.LinkedHashMap[String, Unit](16, 0.75f, true)

  def note(reversals: Seq[TakeChange]): BillingReversals.Noted = synchronized {
    val reversed  = reversals.map(change => BillingReversals.billing(change.listing.rawTitle)).filter(_.nonEmpty).distinct
    reversed.foreach(held.put(_, ()))
    val forgotten = Seq.newBuilder[String]
    while (held.size > capacity) {
      val eldest = held.keySet.iterator.next()
      held.remove(eldest)
      forgotten += eldest
    }
    BillingReversals.Noted(reversed, forgotten.result())
  }
}

object BillingReversals {
  /** How many billings' reversals a country keeps labelled: far above the few that flip at once (three billings explained
   *  ~60 % of the 971 reversals of 2026-10-11..19), small enough for a metric's labels. */
  val Capacity = 25
  /** The longest billing label. */
  val LabelLength = 80

  /** The billings a projection reversed, and those forgotten to keep [[Capacity]]. */
  final case class Noted(reversed: Seq[String], forgotten: Seq[String])

  def billing(rawTitle: String): String = services.movies.TitleContainment.tokens(rawTitle).mkString(" ").take(LabelLength)
}
