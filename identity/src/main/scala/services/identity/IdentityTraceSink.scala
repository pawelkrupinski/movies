package services.identity

import services.movies.ListingKey

/** Where the resolver hands each family it settled, for the rules behind its decisions to be kept
 *  ([[IdentityTraceStore]] builds and writes them). The resolver reaches only this file — not what a trace record
 *  holds, how one is built from a decision, or how a store writes it — because the identity model's rules version
 *  digests every source the resolver reaches (`IdentityRulesSources`): explaining a decision differently decides
 *  nothing differently, and must not re-resolve every worker's corpus. */
trait IdentityTraceSink {
  /** Drop the traces of the families `removed` names, then keep those of `settled`. A family handed over again is
   *  removed in the same call or an earlier one (the model re-resolved it), so its earlier traces never stand beside. */
  def settle(removed: Set[String], settled: Seq[SettledFamily]): Unit
}

/** One family as the resolver settled it: its stored id, the family, and the title rules each of its listings'
 *  titles took (by key) — read only when its traces are built, off the resolver's thread for a store that writes. */
final case class SettledFamily(id: String, family: IdentityResolver.RegionFamily, titleRules: ListingKey => Seq[String],
                               calibration: Option[IdentityCalibration])

object IdentityTraceSink {
  /** Keeps nothing, and builds nothing: a model whose decisions no one reads the rules of. */
  val Discard: IdentityTraceSink = (_: Set[String], _: Seq[SettledFamily]) => ()
}
