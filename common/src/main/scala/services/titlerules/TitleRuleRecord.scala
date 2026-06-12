package services.titlerules

/** A group of title rules sharing one scope (and, for `PerCinema`, one cinema),
 *  edited and stored as a unit. This is the shape the admin editor and the
 *  `titleRules` Mongo collection traffic in: one record per cinema for
 *  `PerCinema`, one record per global scope otherwise.
 *
 *  Each record holds two ordered lists — the normal `rules` and the `lastRules`
 *  that always fold after them within the scope. Position in a list IS the
 *  application order, so the editor reorders by drag rather than by editing an
 *  integer; `toRules` projects that back onto the flat `Seq[TitleRule]` the
 *  domain (`TitleRuleSet`) and the worker backfill consume, stamping `order`
 *  from the index and `last` from which list a rule sat in. */
case class TitleRuleRecord(
  id:        String,
  scope:     RuleScope,
  cinemaId:  Option[String],
  rules:     Seq[TitleRule],
  lastRules: Seq[TitleRule]
) {
  /** Flatten to the domain's `Seq[TitleRule]`: `order` from list position,
   *  `last` from which list, scope/cinema forced to the record's so an embedded
   *  rule can never disagree with its container. */
  def toRules: Seq[TitleRule] =
    rules.zipWithIndex.map { case (r, i) =>
      r.copy(scope = scope, cinemaId = cinemaId, last = false, order = i)
    } ++ lastRules.zipWithIndex.map { case (r, i) =>
      r.copy(scope = scope, cinemaId = cinemaId, last = true, order = i)
    }
}

object TitleRuleRecord {
  /** Non-composite record id: the cinema key for `PerCinema`, the scope name for
   *  the global tiers (which have no cinema). Unique because per-cinema keys are
   *  cinema slugs and never collide with a scope name. */
  def idFor(scope: RuleScope, cinemaId: Option[String]): String =
    cinemaId.getOrElse(scope.name)

  /** Group a flat rule seq into records — shared by the cache's default-seeding
   *  and the legacy-data migration. Each `(scope, cinemaId)` group becomes one
   *  record; within it, `last` rules split into `lastRules` and each sub-list is
   *  ordered by the rule's `(order, id)`. */
  def fromRules(rules: Seq[TitleRule]): Seq[TitleRuleRecord] =
    rules
      .groupBy(r => (r.scope, r.cinemaId))
      .toSeq
      .sortBy { case ((scope, cinemaId), _) => (scope.name, cinemaId.getOrElse("")) }
      .map { case ((scope, cinemaId), group) =>
        val (lasts, normals) = group.partition(_.last)
        def ordered(rs: Seq[TitleRule]): Seq[TitleRule] = rs.sortBy(r => (r.order, r.id))
        TitleRuleRecord(idFor(scope, cinemaId), scope, cinemaId, ordered(normals), ordered(lasts))
      }
}
