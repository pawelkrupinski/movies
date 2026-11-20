package services.identity

import scala.annotation.tailrec

import services.identity.Acceptance.Refused
import services.identity.Scored.Accepted

/** An acceptance rule as one row of data ([[AcceptanceRuleTable]] holds every row): its NAME — the trace's
 *  `refused:<rule>:<why>`, the ablation's `accept:<rule>` — what it REQUIRES of the ranking before it weighs anyone, how
 *  it PICKS the one candidate it weighs, the CHECKS that candidate must pass, in order, and what it TAKES the candidate
 *  at. The first step that does not hold refuses, its refusal the fixed phrase the trace names it by; nothing else of a
 *  rule is code. */
private[identity] final case class AcceptanceRule(name: String, requires: List[AcceptanceRule.Need], picks: AcceptanceRule.Pick,
                                                  checks: List[AcceptanceRule.Check], takes: AcceptanceRule.Take) {
  import AcceptanceRule.*

  /** What the rule takes of `weighing`, or the first of its steps that refused. */
  def verdict(weighing: Weighing): Verdict =
    firstRefusal(requires, weighing, None).toLeft(()).flatMap(_ => picks.of(weighing))
      .flatMap(picked => firstRefusal(checks, weighing, Some(picked)).toLeft(picked))
      .flatMap(takes(weighing, _))

  /** [[verdict]], then every veto of `vetoes` asked of the film it took, in order. */
  def accepts(weighing: Weighing, vetoes: List[Veto]): Verdict =
    verdict(weighing).flatMap(accepted => firstRefusal(vetoes, weighing, Some(accepted._1)).toLeft(accepted))
}

private[identity] object AcceptanceRule {
  type Verdict = Either[Refused, Accepted]
  /** What a rule takes its picked candidate at, or why it refuses even so. */
  type Take = (Weighing, Scored) => Verdict

  /** Neither denied nor reached only by IMDb's suggestion ([[Scored.suggestedOnly]]): a candidate the rules may take. */
  def isEligible(scored: Scored): Boolean = !scored.denied && !scored.suggestedOnly

  /** A ranking — its candidates best first — as the rules weigh it: the views of it every rule reads, each worked out
   *  once however many rules read it. */
  final class Weighing(val ranked: Seq[Scored], weights: EvidenceWeights) {
    lazy val eligible: Seq[Scored]  = ranked.filter(isEligible)
    /** The undenied candidates, IMDb's own suggestions among them. */
    lazy val undenied: Seq[Scored]  = ranked.filterNot(_.denied)
    /** The undenied candidates IMDb suggested, by a place in its answer or by a title it lists them under. */
    lazy val suggested: Seq[Scored] = undenied.filter(scored => scored.imdb.isDefined || scored.imdbTitled.nonEmpty)
    /** The listing the first candidate scores: read only by a rule requiring a candidate. */
    def listing: MeasuredListing = ranked.head.listing
    /** The best eligible candidate's probability, the ranking priors lending and never withdrawing ([[EvidenceWeights.priorsLent]]). */
    lazy val lentToTheBest: Double = weights.priorsLent(eligible.head, eligible)
  }

  /** A step after the pick: refuses the picked candidate, or lets it pass. */
  sealed trait Check { def refuses(weighing: Weighing, picked: Option[Scored]): Option[Refused] }
  /** The ranking must hold `holds`, or the rule refuses naming no candidate. */
  final case class Need(refusal: String, holds: Weighing => Boolean) extends Check {
    def refuses(weighing: Weighing, picked: Option[Scored]): Option[Refused] = Option.unless(holds(weighing))(Refused(refusal))
  }
  /** The picked candidate must hold `holds`, or the rule refuses it, with what its evidence said (`detail`). */
  final case class NeedOf(refusal: String, holds: (Weighing, Scored) => Boolean, detail: (Weighing, Scored) => String = (_, _) => "") extends Check {
    def refuses(weighing: Weighing, picked: Option[Scored]): Option[Refused] = picked.filterNot(holds(weighing, _))
      .map(scored => Refused(refusal, Some(scored.candidate.tmdbId), detail(weighing, scored)))
  }
  /** No `rival` of the picked candidate may stand, or the rule refuses it, naming the rival. */
  final case class NoneOf(refusal: String, rival: (Weighing, Scored) => Option[Scored]) extends Check {
    def refuses(weighing: Weighing, picked: Option[Scored]): Option[Refused] = picked.flatMap(scored =>
      rival(weighing, scored).map(other => Refused(refusal, Some(scored.candidate.tmdbId), Acceptance.named(other))))
  }

  /** How a rule picks the one candidate it weighs. */
  sealed trait Pick { def of(weighing: Weighing): Either[Refused, Scored] }
  /** The only candidate of `among`: refused as `none` (with `noneDetail`) when there is none, as `many` (naming them) when
   *  there are more. */
  final case class One(among: Weighing => Seq[Scored], none: String, many: String, noneDetail: Weighing => String = _ => "") extends Pick {
    def of(weighing: Weighing): Either[Refused, Scored] = among(weighing) match {
      case Seq(only) => Right(only)
      case Seq()     => Left(Refused(none, None, noneDetail(weighing)))
      case found     => Left(Refused(many, None, found.map(Acceptance.named).mkString("; ")))
    }
  }
  /** The first candidate of `among`: refused as `none` (with `noneDetail`) when there is none. */
  final case class First(among: Weighing => Seq[Scored], none: String, noneDetail: Weighing => String) extends Pick {
    def of(weighing: Weighing): Either[Refused, Scored] = among(weighing).headOption.toRight(Refused(none, None, noneDetail(weighing)))
  }
  /** A candidate picked as `pick` names it, refusing in its own words. */
  final case class PickedBy(pick: Weighing => Either[Refused, Scored]) extends Pick {
    def of(weighing: Weighing): Either[Refused, Scored] = pick(weighing)
  }

  /** What refuses ANY rule's take, whichever rule took it: the film `against` holds something against, with what it held
   *  (the refusal's detail). */
  final case class Veto(refusal: String, against: (Weighing, Scored) => Option[String]) extends Check {
    def refuses(weighing: Weighing, taken: Option[Scored]): Option[Refused] = taken.flatMap(scored =>
      against(weighing, scored).map(detail => Refused(refusal, Some(scored.candidate.tmdbId), detail)))
  }

  @tailrec private def firstRefusal(checks: List[Check], weighing: Weighing, picked: Option[Scored]): Option[Refused] = checks match {
    case Nil           => None
    case check :: rest => check.refuses(weighing, picked) match {
      case None    => firstRefusal(rest, weighing, picked)
      case refused => refused
    }
  }
}
