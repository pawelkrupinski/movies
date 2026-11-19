package services.identity

import services.titles.TitleNormalizer

import IdentityResolver.Mutation

/** Stage A of a resolve — candidate generation, scoring and families — wired once, for the resolve
 *  and for [[IdentityResolver.candidatesOf]]. */
private[identity] final class ResolveStages(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                         calibration: IdentityCalibration, mutation: Mutation, pins: PinConstraints, decorations: TitleDecorations,
                                         corpus: Option[CorpusContext] = None) {
  private val arrival = mutation == Mutation.LazyLookups || mutation == Mutation.FirstWins
  // One listing per key, the smallest by the total order — never the first to arrive.
  private val all     = listings.toSeq.sorted.distinctBy(_.key)
  val ordered: Seq[Listing] = if (arrival) listings.toSeq.distinctBy(_.sortKey).filter(all.toSet) else all
  val generation = new CandidateGeneration(ordered, lookups, normalizer, pins, decorations, lazyLookups = mutation == Mutation.LazyLookups, corpus)
  val acceptance = new Acceptance(calibration)
  val scoring    = new CandidateScoring(generation, calibration, acceptance.weights, pins)
  val links      = new TitleLinks(normalizer, pins, generation.context.wholeTitle, generation.context.bannerSegment)
  val families   = new Families(scoring, acceptance, links, normalizer, narrow = mutation == Mutation.NarrowFamilies)
}
