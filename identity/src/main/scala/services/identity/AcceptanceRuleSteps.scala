package services.identity

import services.identity.Acceptance.Refused
import services.identity.AcceptanceRule.{First, Need, NeedOf, Veto, Weighing}
import services.identity.measures.IdentityMeasures.ListingFilm
import services.identity.measures.Scored.Accepted
import services.identity.measures.{IdentityMeasures, Measure, MultiFilmBill, Scored}

/** The STEPS several rows of the acceptance table share — what a rule takes its candidate at, the checks several rules
 *  make, the first veto, and what the rows' refusals say — each a step of [[AcceptanceRule]]'s shape, asking `factRules`
 *  whether a published fact contradicts the candidate. [[AcceptanceWiring]] makes the one the rows share. */
private[identity] final class AcceptanceRuleSteps(factRules: AcceptanceFactRules) {
  import factRules.{contradicted, contradiction}

  /** At its own probability. */
  private[identity] lazy val atItsOwn: AcceptanceRule.Take = (_, scored) => Right(atOwn(scored))

  private[identity] def atOwn(scored: Scored): Accepted = scored -> scored.probability

  /** At what the priors lend the best eligible candidate ([[AcceptanceRule.Weighing.lentToTheBest]]). */
  private[identity] lazy val atWhatThePriorsLend: AcceptanceRule.Take = (weighing, best) => Right(best -> weighing.lentToTheBest)

  /** At what its evidence CLASS measured, its evidence read as `measures` makes it ([[classAccepted]]). */
  private[identity] def atItsClass(measures: Scored => Map[String, Measure]): AcceptanceRule.Take =
    (weighing, scored) => classAccepted(weighing, scored, measures(scored))

  /** UNOPPOSED, read as `weighed` makes it (the candidate read as the rule reads its title): nothing it publishes speaks
   *  against it so read, no other of `among` fits its facts so read better, and its evidence class clears the cut
   *  ([[classAccepted]]). */
  private[identity] def unopposedAs(weighed: Scored => Scored, among: Weighing => Seq[Scored] = _.eligible): AcceptanceRule.Take =
    (weighing, scored) => {
      val read    = weighed(scored)
      val weights = weighing.weights
      if (weights.speaksAgainst(read)) Left(Refused("a published fact weighs against it", Some(scored.candidate.tmdbId), weights.against(read).mkString(" ")))
      else among(weighing).find(rival => (rival ne scored) && weights.fitsBetter(rival, read))
        .fold(classAccepted(weighing, scored, read.measures))(rival => Left(Refused("a rival fits its facts better", Some(scored.candidate.tmdbId), Refused.named(rival))))
    }

  /** `scored` at what its evidence CLASS measured (`IdentityCalibration.classProbability`), lending and never withdrawing —
   *  its class read with the namesakes the release veto took away counted as no rivals ([[Scored.unvetoed]]). */
  private[identity] def classAccepted(weighing: Weighing, scored: Scored, measures: Map[String, Measure]): AcceptanceRule.Verdict = {
    val calibration = weighing.weights.calibration
    calibration.classProbability(ListingFilm, scored.unvetoed(measures)).toRight(Refused("no evidence class measured for it", Some(scored.candidate.tmdbId)))
      .flatMap { classProbability =>
        val lent = math.max(scored.probability, classProbability)
        Either.cond(calibration.showsRatings(lent), scored -> lent, Refused("below the rating cut", Some(scored.candidate.tmdbId), cut(calibration, lent)))
      }
  }

  private[identity] def cut(calibration: IdentityCalibration, probability: Double) =
    f"${probability * 100}%.1f%% < ${calibration.ratingCut * 100}%.1f%%"

  /** What a listing's facts leave a director's rule to go on: its credit, or that it credits nobody. */
  private[identity] def credited(ranked: Seq[Scored]): String =
    ranked.headOption.map(_.listing.directors).filter(_.nonEmpty).fold("the listing credits no director")(names => s"credits ${names.mkString(", ")}")

  private[identity] def firstHit(ranked: Seq[Scored]): String = ranked.find(_.rank.contains(1)).fold("its search returned nothing")(hit => s"first hit ${Refused.named(hit)}")

  /** Why the best candidate is not eligible, if it is denied. */
  private[identity] def bestDenied(weighing: Weighing): String = weighing.ranked.headOption.flatMap(_.denial).fold("")(denial => s"best denied: $denial")

  private[identity] def titleRelation(scored: Scored): String = s"title=${scored.category("title").getOrElse("absent")}"

  private[identity] def titleYearDelta(scored: Scored): String = scored.number("titleYear.delta").fold("")(delta => f"titleYear.delta=$delta%.0f")

  private[identity] def numeral(scored: Scored): String = s"numeral=${scored.category("numeral").getOrElse("")}"

  private[identity] def joinedToAnotherWork(weighing: Weighing, scored: Scored): Boolean =
    IdentityMeasures.joinsAnotherWork(weighing.listing, scored.candidate.film)

  /** `found` without its HOLLOW records — no year, no running time — when it holds a full one beside them: TMDB's empty
   *  duplicate of a film ("Solo", 1450958, beside Sophie Dupuis's 2023 "Solo") is no second film of its director. */
  private[identity] def withoutHollow(found: Seq[Scored]): Seq[Scored] = {
    def hollow(scored: Scored) = scored.candidate.film.year.isEmpty && scored.candidate.film.runtime.forall(_ <= 0)
    val full = found.filterNot(hollow)
    if (full.nonEmpty && full.size < found.size) full else found
  }

  private[identity] lazy val HasCandidate = Need("no candidate", _.ranked.nonEmpty)

  /** A listing billing two works ([[MultiFilmBill.billsTwoWorks]]): a double bill is neither film. */
  private[identity] lazy val BillsOneWork = Need("a double bill", weighing => !MultiFilmBill.billsTwoWorks(weighing.listing))

  /** A film the listing's title joins another work to by a slash ([[IdentityMeasures.joinsAnotherWork]]): a bill of two
   *  works is neither, as a "+" one is. */
  private[identity] lazy val NotJoinedToAnotherWork = NeedOf("the title joins another work to it", (weighing, scored) => !joinedToAnotherWork(weighing, scored))

  private[identity] lazy val Uncontradicted = NeedOf("a published fact contradicts it", (_, scored) => !contradicted(scored), (_, scored) => contradiction(scored))

  /** The best eligible candidate's probability, the priors lending ([[AcceptanceRule.Weighing.lentToTheBest]]), clears the cut. */
  private[identity] lazy val ClearsTheCut = NeedOf("below the rating cut", (weighing, _) => weighing.weights.calibration.showsRatings(weighing.lentToTheBest),
    (weighing, _) => cut(weighing.weights.calibration, weighing.lentToTheBest))

  private[identity] lazy val BestEligible = First(_.eligible, "no eligible candidate", bestDenied)

  /** A film both titles number, differently: PL Cinema City's and UK Odeon's "League of Legends Worlds 26 | Finals in
   *  Cinema" is not TMDB's "League of Legends Worlds25 - Finals in Cinema", its only search hit. A number only one side
   *  carries is weaker — TMDB files a two-part film's halves by their subtitles ("De Gaulle: Liberté" is UK "De Gaulle:
   *  Liberté Part 2") — and stays the rules' own to weigh ([[ImdbSuggested]], [[SoleResult]]). */
  private[identity] lazy val NumbersItsOwnInstalment = Veto("the title numbers another instalment",
    (_, scored) => Option.when(scored.category("numeral").contains("different"))(numeral(scored)))
}
