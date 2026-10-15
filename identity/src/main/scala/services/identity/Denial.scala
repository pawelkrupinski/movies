package services.identity

/** Why a candidate is ruled out for a listing: what the decision's explanation says of it (`why`), and the KIND of
 *  denial it is, which a rule asking for one compares exactly — never by the text, whose wording any rule may share. */
private[identity] final case class Denial(why: String, kind: Denial.Kind = Denial.Kind.Evidence) {
  override def toString: String = why
}

private[identity] object Denial {
  enum Kind {
    /** The listing's own evidence, a pin, or a family's rule. */
    case Evidence
    /** The calibration's probability cut alone ([[LearnedCannotLinks]]): the weakest denial. */
    case ProbabilityCut
    /** A season production its listing's banner is not the house of ([[CandidateScoring.evidenceDenial]]). */
    case AnotherHousesSeason
    /** The release veto ([[ReleaseVeto]]): a namesake never released in the venue's country, beside `releasedBeside`,
     *  which was. */
    case Vetoed(releasedBeside: Int)
  }

  /** A denial of the listing's evidence, said so. */
  def apply(why: String): Denial = Denial(why, Kind.Evidence)
}
