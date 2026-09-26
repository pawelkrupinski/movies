package services.readmodel

/** The name of one derivation: what a version of the projection code makes of an unchanged row. */
final case class DerivationVersion(value: String) {
  override def toString: String = value
}

/** What a derivation change moved, and so what the pass that owes it has to re-project. */
enum DerivationScope(val label: String) {
  /** Only what a CARD shows moved (title, poster, facts, synopses, ratings, trailers, age rating):
   *  every card projects from the slots-only read, which leaves the `screenings` collection —
   *  most of the bytes a whole-row read pulls (US 260 of 341 MB, 2026-09-26) — unread. */
  case Cards extends DerivationScope("cards")
  /** The screenings rows, or which cards a row produces, moved: every row is read whole. */
  case Full extends DerivationScope("full")
}

object DerivationScope {
  def parse(label: String): DerivationScope =
    values.find(_.label == label).getOrElse(throw new IllegalArgumentException(s"no derivation scope '$label'"))

  /** What a pass owes for several changes at once: cards alone only when every one of them was. */
  def union(scopes: Iterable[DerivationScope]): DerivationScope =
    if (scopes.forall(_ == Cards)) Cards else Full
}

final case class Derivation(version: DerivationVersion, scope: DerivationScope)

/**
 * Every derivation the projection has had, oldest first, the last being what this code derives.
 *
 * A new entry is owed exactly when projecting the checked-in derivation corpus — real stored rows,
 * `read-model-derivation-rows.jsonl` — gives a different read model than the one recorded beside
 * it (`read-model-derivation-hashes.tsv`): the SAME rows, the old code's output against the new
 * code's, so a scraper or fixture change that only moves the rows never counts. Until 2026-09-26
 * the version was the fingerprint of the whole read-model snapshot, which every scraper change
 * moved: five such deploys in an hour re-projected every country's corpus up to three times each,
 * ~43,000 whole-row reads that rewrote 36 documents. `FilmScheduleEndToEndSpec` regenerates the
 * corpus and names the entry to append; `ReadModelDerivationVersionSpec` holds this list to it.
 *
 * The history, not just the last entry, is what a worker needs: a store left several derivations
 * behind owes the union of their scopes (see [[owedSince]]).
 */
object ReadModelDerivation {
  val History: Seq[Derivation] = Seq(
    Derivation(DerivationVersion("2b3aef8d690fea26"), DerivationScope.Full),
    // web_screenings rows carry the listing keys of the slots they union (identity phase 4, §16).
    Derivation(DerivationVersion("e90186e6b0473c75"), DerivationScope.Full))

  def current: DerivationVersion = History.last.version

  /** The pass a store derived under `recorded` is owed: none when it is current, everything
   *  (`Full`) when the version is unknown to this code or none was ever recorded. */
  def owedSince(recorded: Option[DerivationVersion], history: Seq[Derivation] = History): Option[DerivationScope] =
    recorded match {
      case Some(version) if version == history.last.version => None
      case Some(version) =>
        history.indexWhere(_.version == version) match {
          case -1    => Some(DerivationScope.Full)
          case index => Some(DerivationScope.union(history.drop(index + 1).map(_.scope)))
        }
      case None => Some(DerivationScope.Full)
    }
}
