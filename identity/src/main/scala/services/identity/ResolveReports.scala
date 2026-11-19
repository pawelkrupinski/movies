package services.identity

import services.titles.TitleForms

/** The resolve's own stages told for a report: each candidate as a node's family scored it, a focused node's candidates
 *  and banners ([[candidatesOf]]). */
object ResolveReports {

  /** One candidate as a node's family scored it, for a report reading why a listing took what it took. */
  final case class CandidateScore(tmdbId: Int, title: String, year: Option[Int], probability: Double, searchRank: Option[Int],
                                  denial: Option[String], seasonProduction: Boolean, houseProduction: Boolean, explanation: String,
                                  untaken: Option[String] = None,
                                  /** The namesake the release veto denied it beside ([[ReleaseVeto]]). */
                                  releasedBeside: Option[Int] = None) {
    def denied: Boolean = denial.isDefined
    def render: String =
      f"$tmdbId%8d ${ResolverDecision.percent(probability)}%6s rank ${searchRank.fold("-")(_.toString)}%2s" +
        s"${denial.fold("")(why => s" DENIED ($why)")}${untaken.fold("")(why => s" UNTAKEN ($why)")}${if (seasonProduction) " season" else ""}${if (houseProduction) " house" else ""} " +
        s"'$title'${year.fold("")(filmYear => s" ($filmYear)")} — $explanation"
  }
  object CandidateScore {
    private[identity] def of(scored: services.identity.Scored, calibration: IdentityCalibration): CandidateScore =
      CandidateScore(scored.candidate.tmdbId, scored.candidate.film.title, scored.candidate.film.year, scored.probability, scored.rank,
        scored.denial.map(_.why), scored.seasonProduction, scored.houseProduction, calibration.explain(IdentityMeasures.ListingFilm, scored.measures),
        scored.untaken, scored.denial.flatMap(ReleaseVeto.releasedBeside))
  }

  /** A focused node, its candidates as its family scores them (best first), and each banner its
   *  candidates bill it under with the houses contending for that banner, best first, and the one learned. */
  final case class NodeCandidates(label: String, candidates: Seq[CandidateScore], banners: Seq[String])

  /** Every node holding a listing `focused` selects — the resolve's own stages, so the report shows
   *  what the resolve read. */
  def candidatesOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                   calibration: IdentityCalibration = IdentityCalibration.resolver,
                   pins: PinConstraints = PinConstraints(Nil),
                   decorations: TitleDecorations = TitleDecorations.resolver)(focused: Listing => Boolean): Seq[NodeCandidates] = {
    val stages = new ResolveStages(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None, pins, decorations)
    import stages.scoring.{houseRanking, houses}
    IdentityResolver.focusedScored(stages)(focused).map { case (node, scored) =>
      val banners = scored.flatMap(candidate => StageRelays.billing(node.evidence.measured, candidate.candidate.film)).map(_.listingHouse).distinct.sorted
      NodeCandidates(node.label, scored.map(CandidateScore.of(_, calibration)),
        banners.map(banner => s"banner '$banner' → ${houses.of.getOrElse(banner, "no house")}; contenders: " +
          houseRanking.getOrElse(banner, Nil).take(4).map(_.render).mkString(", ")))
    }
  }
}
