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
    /** The film is not the WORK the listing's billing or feed names, by its kind: a short beside a feed's link to a
     *  feature of the title ([[FamilyScope]]'s copied-title denial), a screen namesake of the work a stage house's banner
     *  bills ([[CandidateScoring.evidenceDenial]]). Unlike a denial by the listing's own facts (another year, another
     *  director: the listing is another film), it holds for every billing of that work ([[ClusterVoting]]) — but one
     *  `creditOverrules` lets a billing whose credited director is the film's keep it, as the screen-namesake rule does its
     *  own listing: a credit the venue publishes names its staging's record whatever TMDB titles it. */
    case NotTheWork(creditOverrules: Boolean)
    /** A learned listing-film rule ([[LearnedCannotLinks]]): the listing's own facts against the film, which
     *  [[Authority.withdraws]] weighs against its own credit. */
    case Learned
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
