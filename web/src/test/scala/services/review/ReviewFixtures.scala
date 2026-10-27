package services.review

import services.identity.ResolverDecision
import services.movies.ListingKey

/** A small corpus the review specs share: an unmatched PL cluster held just below the line, a vetoed one,
 *  one with no candidate, and a matched one. */
object ReviewFixtures {
  val Held    = ListingKey.Native("Kino Opalenica", "https://www.bilety24.pl/kino/967-franz-kafka-165208", "FRANZ KAFKA")
  val Vetoed  = ListingKey.Published("Kino Muza", "Macbeth", Some(1971), Seq("Roman Polański"))
  val Nothing = ListingKey.Native("Kino Kosmos", "https://kosmos/robaczki", "Filmowe popołudnie dla dzieci: Robaczki")
  val Matched = ListingKey.Native("Kino Bajka", "https://bajka/klondike", "Klondike")

  val heldDecision = ResolverDecision(Seq(Held), None, 0.14, ResolverDecision.Basis.BelowThreshold, Seq(
    "Kino Opalenica \"FRANZ KAFKA\": own match 1157322 at 41.0% (title exact)",
    "best rejected candidate 1157322 at 86.4% (title exact, year none)",
    "node N Kino Opalenica"), candidate = Some(ResolverDecision.Leaning(1157322, 22963134)))()
  val vetoedDecision = ResolverDecision(Seq(Vetoed), None, 0.37, ResolverDecision.Basis.Vetoed, Seq(
    "best vetoed candidate 1703622 at 62.5%, denied: another house's season production, (title overlap)"))()
  val nothingDecision = ResolverDecision(Seq(Nothing), None, 1.0, ResolverDecision.Basis.NoCandidate, Seq("node N Kino Kosmos"))()
  val matchedDecision = ResolverDecision(Seq(Matched), Some(913760), 0.62, ResolverDecision.Basis.OwnMatch, Seq(
    "Kino Bajka \"Klondike\": own match 913760 at 62.0% by favoured-calibrated (title exact)"))()

  /** Prod's "Odblask | UFF" at Kino Amondo: the resolver vetoed the festival-tag film "Uff" and stored 16878 as its candidate. */
  val Odblask = ListingKey.Native("Kino Amondo", "https://kinoamondo.pl/film/odblask-uff", "Odblask | UFF")
  val odblaskDecision = ResolverDecision(Seq(Odblask), None, 0.71, ResolverDecision.Basis.Vetoed, Seq(
    "best vetoed candidate 850957 at 28.9%, denied: its title names it only by a programme tag billed beside many titles, " +
      "(title=overlap(-3.95) search.rank=1(+3.48) rivals=0(+1.53))",
    "node N Kino Amondo"), candidate = Some(ResolverDecision.Leaning(16878, 0)))()
}
