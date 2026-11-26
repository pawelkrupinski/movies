package services.identity

import services.identity.AcceptanceRule.Weighing
import services.identity.measures.{FactRelations, IdentityMeasures, Scored}
import services.titles.YearWindow

/** What the acceptance rules that take a film by the listing's facts ask — a sole work and a dated title
 *  ([[AcceptanceFactRows]]) — and whether a fact the listing publishes contradicts a record or corroborates it, which
 *  rules of every kind ask. Functions of the candidates and the listing they score; [[AcceptanceWiring]] makes the one
 *  the rules ask. */
private[identity] final class AcceptanceFactRules {

  /** What [[contradicted]] read: the year distance and the runtime delta the listing published. */
  private[identity] def contradiction(scored: Scored): String =
    (scored.number("year.distance").map(d => f"year.distance=$d%.0f") ++ scored.number("runtime.delta").map(d => f"runtime.delta=$d%.0f")).mkString(" ")

  /** A fact the listing publishes agrees: the director it credits, or the year it gives, exactly. */
  private[identity] def corroborated(scored: Scored): Boolean =
    IdentityMeasures.sameDirector(scored.measures) || scored.number("year.distance").contains(0.0)

  /** A published year more than one off, or a runtime 30 minutes or more off: the listing's own facts against it. */
  private[identity] def contradicted(scored: Scored): Boolean =
    scored.number("year.distance").exists(_ > YearWindow.PublishedAdjacency) || IdentityMeasures.runtimeContradicts(scored.measures)

  /** The eligible first hits whose WORK the listing's whole title is, or bills under a shorter subtitle of its own, that
   *  nothing published contradicts: [[AcceptanceRuleTable.SoleWork]]'s to pick one of. */
  private[identity] def firstHitsTitlingTheirWork(weighing: Weighing): Seq[Scored] =
    weighing.eligible.filter(scored => scored.rank.contains(1) && (titledItsWork(scored) || billsItsWork(scored)) && !contradicted(scored))

  /** Another candidate the title names beside `work` — or, when the listing only bills `work` under a subtitle, another
   *  billing that work. */
  private[identity] def anotherNamedBesideItsWork(weighing: Weighing, work: Scored): Option[Scored] =
    weighing.eligible.find(other => (other ne work) && (other.titleNamesIt || (!titledItsWork(work) && sharesItsWork(other))))

  private def titledItsWork(scored: Scored) = IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)

  private def sharesItsWork(scored: Scored) = IdentityMeasures.billsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)

  private def billsItsWork(scored: Scored) = scored.category("title").contains("fragment") && sharesItsWork(scored)

  /** The eligible candidates the listing's title names, years aside, from the year its title dates it, that nothing
   *  published contradicts nor a credited director denies: [[AcceptanceRuleTable.DatedTitle]]'s to pick one of. */
  private[identity] def datedTitles(weighing: Weighing): Seq[Scored] =
    weighing.eligible.filter(candidate => namedButForItsYear(candidate) && candidate.number("titleYear.delta").exists(FactRelations.nearDelta) &&
      !contradicted(candidate) && !candidate.category("director").contains("different"))

  /** Why no candidate is dated by the title: none, or the title dates no year. */
  private[identity] def undated(weighing: Weighing): String =
    if (weighing.ranked.exists(_.number("titleYear.delta").isDefined)) "" else "the title dates no year"

  /** Is the listing's title, years aside, the candidate's own title or original title — or that
   *  title decorated along one edge ("La Fanciulla del West Encore" of the Met's "… 2026/27: La
   *  Fanciulla del West")? Asked only beside a title year that agrees ([[datedTitles]]). */
  private[identity] def namedButForItsYear(candidate: Scored): Boolean = {
    // Each title's tokens as the listing and the film hold them, worked out once (`ownYearlessTitles`, `billedTitles`,
    // `billedWorks`), not per candidate weighed: ~40 MB of a PL hard-cluster run's resolves (JFR 2026-10-06).
    val titles = candidate.candidate.film.ownYearlessTitles
    // One word is many films' title — but not as a whole piece beside the year the listing dates it by, which the
    // rule requires: "Akademia Kina Polskiego: Drogówka (2012)" is Smarzowski's 2013 film.
    // A one-word title must be a whole delimited PIECE of the listing's ("…: Drogówka (2012)"), not its last word
    // ("Fanciulla Encore (2027)" is no "Encore").
    lazy val pieces = candidate.listing.billedWorks   // the shapes' yearless tokens; a one-word title is never empty
    candidate.listing.billedTitles.exists(own => own.nonEmpty &&
      titles.exists(title => title == own || (title.sizeIs >= 2 && services.titles.TitleContainment.isTokenRun(title, own)) ||
        (title.sizeIs == 1 && pieces(title))))
  }
}
