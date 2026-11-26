package services.identity

import services.identity.measures.Resolution
import services.identity.vocabulary.RuleKind

/**
 * THE PER-RULE MONOTONICITY CONTRACT (structural step 4 of docs/investigations/2026-11-01-flip-unification.md): every rule
 * that can decide a listing's take declares how that take may depend on the POPULATION -- which other listings are
 * present -- by its id as [[DecidedBy.of]] names it (the id a take-change line, the rule-firing count and the identity
 * trace carry, so a dependency index of the rules that fired reads the same ids):
 *
 *  - [[RuleKind.PopulationIndependent]] (i): decided by the listing's own facts and TMDB alone. Whatever else is present,
 *    when it decides the listing it decides the same take.
 *  - [[RuleKind.Monotone]] (ii): it reads the population, but only one way -- as listings arrive its verdict on a listing
 *    never goes back to one it left (A -> B -> A).
 *  - [[RuleKind.NonMonotone]] (iii): it weighs the listing against the population (a shared candidate pool, a rival backed
 *    elsewhere, a sibling's denial, a family's or venues' vote), so it may go back as listings arrive.
 *
 * Nothing here decides a take: the contract is read by tests only. RuleContractSpec fails on a rule of a closed list with
 * no kind; the population walk (PopulationMonotonicitySpec, over every committed slice) fails on a rule it sees decide a
 * take with no kind, on a kind (i) or (ii) rule that reverses, on a flip only kind (i) and (ii) rules decided, and -- once
 * the model marks namesake-ambiguous listings (structural step 3, [[namesakeAmbiguous]]) -- on such a listing decided by a
 * kind (iii) rule ([[mayDecideNamesakeAmbiguous]]).
 *
 * A declared pattern is an exact id, or an id with `*` for a part filled in at run time (a feed's name, a country, a
 * learned rule a refit renames, a combination of agreeing families). An exact declaration overrides a pattern
 * (`withdrawal:judged-wrong` is a judgement, every other withdrawal the stage's). The kinds are the 2026-11-04 inventory's
 * (each from reading its rule); the walk is what holds them.
 *
 * Every `accept:` and `pooled:` rule is (iii) at the decision: whatever the rule itself reads, a take by it is dropped when
 * a corroborated suggested-only rival outweighs it (Acceptance's takenBy), its catalogue id is contested
 * (ResolverRun.contested) or a sibling denies its film (Families.deniedBySibling).
 */
object RuleContract {

  /** One declaration: the id (or `*` pattern) and why the rule is of its kind. */
  final case class Declared(pattern: String, why: String) {
    val exact: Boolean = !pattern.contains('*')
    private val regex  = pattern.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*").r
    def matches(ruleId: String): Boolean = if (exact) pattern == ruleId else regex.matches(ruleId)
  }

  /** A refusal's declaration: the why Decision.scala names a rule's refusal by (`rule:<DecisionTrace.id(why)>`), and why
   *  that refusal is of its kind. */
  private def refusal(pattern: String, why: String): Declared =
    Declared(pattern, s"refusal why (rule:<DecisionTrace.id(why)>, Decision.scala): $why")

  /** Kind (i), the vetoes: a veto by the listing's own facts and TMDB alone. */
  private val ownFactVetoes: Seq[Declared] = Seq(
    Declared("veto:learned-*", "learned listing-film cannot-link on the listing's own measures vs the TMDB record"),
    Declared("veto:learned-listing-film-probability-below-the-cannot-link-cut", "cut on weights.factsProbability: own facts only, rivals/rank excluded (EvidenceWeights.scala)"),
    Declared("veto:seasonsapart", "ListingConstraints.seasonsApart: listing's season vs film season/year (CannotLink.SeasonsApart.toString)"),
    Declared("veto:another-house-s-season-production", "StageRelays.namesSeasonProduction on listing banner vs film"),
    Declared("veto:a-screen-namesake-of-the-work-its-stage-house-s-billing-names", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-record-of-another-work-than-its-stage-house-s-billing-names", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:another-production-of-its-work-than-the-season-its-screenings-fall-in", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-catalogue-namesake-of-a-bare-billing-under-a-film-festival-s-banner", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:one-work-of-a-double-bill-whose-other-piece-none-of-its-titles-holds", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-part-its-title-gives-a-subtitle-none-of-the-film-s-titles-holds", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:an-instalment-its-title-numbers-whose-subtitle-none-of-the-film-s-titles-holds", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:another-year-s-edition-of-the-series-its-title-dates", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-namesake-of-the-film-whose-director-its-billing-names", "directorBilled: billing names a director, film's banner does not"),
    Declared("veto:another-instalment-than-the-one-the-title-numbers", "numeral category, own title vs film"),
    Declared("veto:pinned-never-this-film", "admin pin on the node's listing"),
    Declared("veto:its-title-names-it-only-by-the-venue-s-own-name", "namesOnlyItsVenue"),
    Declared("veto:its-title-names-it-only-by-a-festival-s-programme-slot", "namesOnlyAProgrammeSlot"),
    Declared("veto:its-title-bills-a-film-festival-by-its-name-alone-the-programme-no-film", "billingOf(node).festival"),
    Declared("veto:it-bills-a-silent-film-and-this-one-is-of-the-sound-era", "silentBillingDenies"),
    Declared("veto:its-venue-s-own-cast-field-names-none-of-its-top-billed-cast-and-no-fact-the-venue-states-agrees-with-it", "CastEvidence.mismatch over the node's listings"),
    Declared("veto:named-only-by-the-title-its-feed-*-copied*", "OPEN; feed-copied original title alone names it"),
    Declared("veto:a-short-its-feed-*-linking-*", "OPEN; short beside the feed's link")
  )

  /** Kind (i): the listing's own facts and TMDB alone, its vetoes ([[ownFactVetoes]]) and its refusals. */
  val populationIndependent: Seq[Declared] = ownFactVetoes ++ Seq(
    Declared("search:unanswered", "the node's own query unanswered (ResolverDecisions.scala)"),
    Declared("rule:none", "no refusal recorded"),
    refusal("rule:no-evidence-class-measured-for-it", "classProbability of own measures"),
    refusal("rule:a-published-fact-weighs-against-it", "speaksAgainst own facts"),
    refusal("rule:the-title-numbers-another-instalment", "numeral category"),
    refusal("rule:another-piece-of-its-billing-is-a-performance-the-film-is-not", "own billing"),
    refusal("rule:its-billing-credits-its-work-to-an-author-who-made-no-such-film", "own billing credit"),
    refusal("rule:its-billing-is-a-name-alone-which-the-record-s-longer-title-only-leads-and-no-fact-agrees", "aNameAlone"),
    refusal("rule:the-record-is-the-staged-show-s-own-a-pantomime-is-an-event-no-film", "own billing against the record's titles (ListingShape.showsOwnRecord)"),
    refusal("rule:the-billing-reads-as-no-film", "readsAsNoFilm: event words"),
    refusal("rule:a-double-bill", "MultiFilmBill.billsTwoWorks(listing)"),
    refusal("rule:the-title-joins-another-work-to-it", "joinsAnotherWork"),
    refusal("rule:its-house-s-record-is-from-another-year-than-its-title-dates", "titleYear.delta"),
    refusal("rule:it-bills-no-house-s-relay-of-a-work-in-a-season", "listing.relayBilled"),
    refusal("rule:it-credits-no-director", "listing.directors"),
    refusal("rule:it-bills-no-relay-s-banner", "RelayBanner.billed(listing's titles)"),
    refusal("rule:a-published-year-contradicts-it", "year.distance"),
    refusal("rule:it-credits-another-director", "director category"),
    refusal("rule:the-title-names-its-season", "listing.seasonYear"),
    refusal("rule:a-published-fact-contradicts-it", "contradicted()"),
    refusal("rule:the-year-it-states-is-another-season-s", "listing.year vs season"),
    refusal("rule:not-its-search-s-first-hit", "own rank"),
    refusal("rule:not-named-by-a-piece-of-the-title", "title category"),
    refusal("rule:its-piece-cannot-stand-for-the-whole-title", "standsForTheWhole"),
    refusal("rule:the-title-dates-another-year", "titleYear.delta"),
    refusal("rule:a-bare-billing-names-the-film-only-by-an-alternative-title-and-tmdb-s-search-of-it-finds-other-films-not-this-one", "own search ranks"),
    refusal("rule:the-title-does-not-name-its-search-s-only-film", "own title vs sole hit"),
    Declared("withdrawal:judged-wrong", "human judgement of this listing's take")
  )

  /** Kind (ii): reads the population, one way only. */
  val monotone: Seq[Declared] = Seq(
    Declared("search:found-nothing", "no candidate in the node's scoring of the family pool: siblings' searches can only add candidates"),
    refusal("rule:no-candidate", "absence; pool can only grow"),
    refusal("rule:no-first-hit-is-the-title-s-work", "absence over own first hits"),
    refusal("rule:no-film-of-its-credited-director-shares-its-work", "absence"),
    refusal("rule:no-film-of-its-exact-title-by-its-credited-director", "absence"),
    refusal("rule:no-film-of-its-title-from-the-year-its-title-dates", "absence"),
    refusal("rule:no-record-bills-its-work-under-its-house", "absence"),
    refusal("rule:no-record-of-its-house-s-relay-of-its-work-from-its-season", "absence"),
    refusal("rule:no-record-of-its-house-by-its-director-aired-in-the-season-it-screens-in", "absence"),
    refusal("rule:a-record-bills-its-work-under-its-house", "presence; pool can only grow"),
    refusal("rule:no-season-record-carries-its-title-or-its-work-under-its-house", "absence"),
    refusal("rule:no-film-its-whole-title-names-is-its-search-s-first-hit", "absence over own search ranks"),
    refusal("rule:the-title-names-no-film", "absence"),
    refusal("rule:imdb-suggests-none-of-its-candidates", "absence"),
    refusal("rule:no-search-of-its-returned-a-single-film", "absence"),
    Declared("pin", "Pinned if any cluster member is pinned (ResolverDecisions.scala): own or joined pin")
  )

  /** Kind (iii) acceptances: an `accept:` rule alone, a `pooled:` one over the cluster's pooled evidence. */
  private val nonMonotoneAcceptances: Seq[Declared] = Seq(
    Declared("accept:sole-work", "one() of first hits + noneOf 'the title names another film' over the family-pool ranking (AcceptanceRuleTable SoleWork)"),
    Declared("accept:favoured-calibrated", "runner-up comparison (eligible.lift(1)) + closerThan; probability carries the rivals count (AcceptanceRuleTable FavouredCalibrated)"),
    Declared("accept:exact-top-hit", "unique exact top hit ('two exact top hits'), unopposed(): no rival fitsBetter, class probability with rivals (AcceptanceRuleTable ExactTopHit)"),
    Declared("accept:segment-top-hit", "one() 'the title names two films' + unopposed rival check (AcceptanceRuleTable SegmentTopHit)"),
    Declared("accept:sole-result", "one() over soleResult + noneOf 'the title names another film' (AcceptanceRuleTable SoleResult)"),
    Declared("accept:imdb-suggested", "suggested.count(sameDirector)==1 / count(exact)==1, one(), TMDB ranks another (AcceptanceRuleTable ImdbSuggested)"),
    Declared("accept:directors-work", "billedAlike==1 count + one() 'two films of its director share its work' (AcceptanceRuleTable DirectorsWork)"),
    Declared("accept:directors-title", "one() 'two films of its title by its director' (AcceptanceRuleTable DirectorsTitle)"),
    Declared("accept:dated-title", "one() 'two films of its title from that year' (AcceptanceRuleTable DatedTitle)"),
    Declared("accept:house-production", "one() + noneOf 'another record carries its title or fits as well' (AcceptanceRuleTable HouseProduction)"),
    Declared("accept:season-record", "one() 'two seasons' records carry its title' (AcceptanceRuleTable SeasonRecord); pre-empts via relayOrBroadcastSeasonRecord"),
    Declared("accept:relay-record", "one() 'two records of its house's relay...' (AcceptanceRuleTable RelayRecord)"),
    Declared("accept:relay-director", "one() 'two records of its house by its director...' (AcceptanceRuleTable RelayDirector)"),
    Declared("accept:season-production", "seasonProduction: the sole one, or own(first)>own(second) tie-break (Acceptance.scala)"),
    Declared("accept:joined", "OwnMatch with no nameable rule for key or any member: film reached via join to another listing's own match; must-links/cannot-links from other listings decide it"),
    Declared("pooled:season-production", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala); same uniqueness/rival checks as alone rule"),
    Declared("pooled:season-record", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard; same uniqueness/rival checks as the alone rule"),
    Declared("pooled:unrivalled-calibrated", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala); same uniqueness/rival checks as alone rule ; unrivalled: 'a film the title names alike fits its facts better'"),
    Declared("pooled:exact-top-hit", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala); same uniqueness/rival checks as alone rule"),
    Declared("pooled:imdb-suggested", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala); same uniqueness/rival checks as alone rule"),
    Declared("pooled:unnamed", "PooledMatch whose film no pooled rule names: ClusterVoting.familyMajority ('N of M venue(s)' majority) or a vote ResolverRun.scala"),
    Declared("pooled:no-film", "no-match listing without a node trace (also IdentityTraceStore.scala): cluster-level no-film")
  )

  /** Kind (iii) vetoes and withdrawals: relative to another candidate, member or sibling. */
  private val nonMonotoneVetoes: Seq[Declared] = Seq(
    Declared("veto:denied", "fallback: pooled best candidate denied with no reason text (ResolverDecisions.scala picks the denying member)"),
    Declared("veto:imdb-suggested-only", "blocker: every candidate denied or suggested-only, none carrying a denial; over the family-pool ranking"),
    Declared("veto:bills-both-works", "billsSeveralWorks matches slash/'and' pieces against pool candidates + billingUnderBanner reads cluster siblings (NoFilmMembers)"),
    Declared("veto:billed-as-one-piece", "billedAsPiece reads max venues.corroborating across members and corpus-recurring segments (NoFilmMembers)"),
    Declared("veto:uncorroborated-namesake", "Corroboration.refused: nothing of its own corroborates the cluster's film beside others' backing (ResolverRun.scala)"),
    Declared("veto:a-member-s-own-evidence-rules-it-out", "pooled candidate denied because ANOTHER member's own scoring denies it"),
    Declared("veto:vetoed-not-released-in-*-as-namesake-*-is", "OPEN; relative to a released namesake in the pool, lifted by venues.corroborating (ListingScoring.scala, the release veto)"),
    Declared("veto:a-film-of-the-venue-s-own-language-its-title-shares-no-word-with", "ownLanguageUntitled: probability >= every named candidate's (ListingScoring.scala, the beside denials)"),
    Declared("veto:another-record-of-a-broadcast-its-film-database-holds-twice", "anotherRecordOfItsBroadcast: relative to another pool record"),
    Declared("veto:its-director-s-other-film-which-the-title-does-not-name", "needs another candidate the title names by the same director (titled)"),
    Declared("veto:a-namesake-of-the-film-its-credited-director-made-of-its-title", "creditedNamesake / directedBy filmography reach: relative to another candidate"),
    Declared("veto:another-house-s-staging-of-its-work-beside-its-own-house-s-record", "besideItsOwnHouse: relative to another housed candidate"),
    Declared("veto:its-title-names-it-only-by-a-programme-tag-billed-beside-many-titles", "namesOnlyATag: corpus count of titles a tag is billed beside"),
    Declared("veto:its-venue-s-own-text-credits-another-film-s-director-and-none-of-this-relay-s", "RelayRecords.contradictedByOwnText reads the other reached films"),
    Declared("veto:its-feed-s-link-*-*-overruled-*", "OPEN; rival backed by >= OverrulingSources venues / more venues billing a current release: counts"),
    Declared("withdrawn:sibling-denies-film", "Families.deniedBySibling: a title-linked sibling's evidence denies its own match")
  )

  /** Kind (iii) refusals: a rival, a uniqueness or a pool-relative denial. */
  private val nonMonotoneRefusals: Seq[Declared] = Seq(
    refusal("rule:bills-several-works", "Acceptance.scala 'alone' refusal; billsSeveralWorks matches pieces against pool candidates"),
    refusal("rule:its-own-facts-do-not-favour-it-over-the-runner-up", "favoured-calibrated runner-up (preferred blocker, Decision.scala)"),
    refusal("rule:its-title-sits-inside-a-closer-record", "closerThan another candidate"),
    refusal("rule:no-eligible-candidate", "eligibility via denials, some pool/sibling-relative"),
    refusal("rule:below-the-rating-cut", "probability carries rivals count / priorsLent over eligible"),
    refusal("rule:a-rival-fits-its-facts-better", "fitsBetter rival"),
    refusal("rule:its-title-names-nothing-of-it-and-another-film-of-its-director-it-names-or-dates-as-closely-stands-beside-it", "anotherOfItsDirector rival"),
    refusal("rule:untaken", "Scored.untaken via shortUntaken reads pool features (ListingScoring.scala, shortUntaken)"),
    refusal("rule:its-page-reads-as-reused-only-a-copied-original-title-names-a-film-of-it-and-the-billing-says-more", "another eligible candidate copiedBeyondItsBilling"),
    refusal("rule:two-first-hits-are-the-title-s-work", "uniqueness"),
    refusal("rule:the-title-names-another-film", "noneOf other candidate"),
    refusal("rule:two-films-of-its-director-share-its-work", "uniqueness"),
    refusal("rule:two-films-of-its-title-by-its-director", "uniqueness"),
    refusal("rule:two-films-of-its-title-from-that-year", "uniqueness"),
    refusal("rule:two-records-bill-its-work-under-its-house", "uniqueness"),
    refusal("rule:another-record-carries-its-title-or-fits-as-well", "rival"),
    refusal("rule:two-records-of-its-house-s-relay-of-its-work-from-its-season", "uniqueness"),
    refusal("rule:two-records-of-its-house-by-its-director-aired-in-the-season-it-screens-in", "uniqueness"),
    refusal("rule:two-seasons-records-carry-its-title", "uniqueness"),
    refusal("rule:two-exact-top-hits", "uniqueness"),
    refusal("rule:its-exact-top-hit-is-denied", "denial may be pool/sibling-relative"),
    refusal("rule:the-title-names-two-films", "uniqueness"),
    refusal("rule:the-film-it-names-is-denied", "denial may be pool-relative"),
    refusal("rule:no-imdb-suggestion-is-taken-by-its-year-its-director-or-as-the-only-one", "count()==1 rungs"),
    refusal("rule:two-imdb-suggestions-qualify", "uniqueness"),
    refusal("rule:tmdb-ranks-another-film-the-title-names-above-it", "rival ranked above"),
    refusal("rule:its-searches-returned-different-single-films", "uniqueness")
  )

  /** Kind (iii) votes and the agreement stage: a family's, the venues' or the feeds' say. */
  private val nonMonotoneVotes: Seq[Declared] = Seq(
    Declared("agreed:*", "several film-database families agree on a film for a cluster TMDB matched to nothing: the family's votes are pooled"),
    Declared("poster:vote", "PosterEvidence.vote: the ONE candidate any venue poster matches, no other within VoteBits"),
    Declared("broadcast", "Broadcast.take: the one record the cluster's screening days name, none denied"),
    Declared("catalogue", "Catalogue.take: refused if any stated listing contradicts the record (Catalogue.scala)"),
    Declared("event", "NonFilmEvents.of: EVERY listing must read as an event (NonFilmEvents.scala); one more listing can remove it"),
    Declared("fill:*", "the agreement stage fills a no-match from its family's and venues' takes (identity-unified-rules.json fill)"),
    Declared("correction:*", "the agreement stage switches a take on evidence its family and the feeds give"),
    Declared("withdrawal:*", "the agreement stage withdraws a take on evidence its family and the feeds give")
  )

  /** Kind (iii): weighs the listing against the population. */
  val nonMonotone: Seq[Declared] = nonMonotoneAcceptances ++ nonMonotoneVetoes ++ nonMonotoneRefusals ++ nonMonotoneVotes

  val declared: Seq[(Declared, RuleKind)] =
    populationIndependent.map(_ -> RuleKind.PopulationIndependent) ++ monotone.map(_ -> RuleKind.Monotone) ++
      nonMonotone.map(_ -> RuleKind.NonMonotone)

  private val exactly: Map[String, RuleKind] = declared.collect { case (d, kind) if d.exact => d.pattern -> kind }.toMap
  private val patterns                       = declared.filterNot(_._1.exact)

  /** `ruleId`'s declared kind: its exact declaration's, else the one pattern it matches; none when undeclared. */
  def kindOf(ruleId: String): Option[RuleKind] =
    exactly.get(ruleId).orElse(patterns.collectFirst { case (d, kind) if d.matches(ruleId) => kind })

  /** May a rule of this id decide a namesake-ambiguous listing? Only a declared kind (i) or (ii) one: a namesake's take
   *  must not hang on which of its title's other listings are present. */
  def mayDecideNamesakeAmbiguous(ruleId: String): Boolean = kindOf(ruleId).exists(_ != RuleKind.NonMonotone)

  /** The listings of `resolution` the model marked NAMESAKE-AMBIGUOUS. None until structural step 3
   *  (identity/namesake-ambiguous) lands: its `Resolution.namesakeAmbiguous` is read here then, and every check of the
   *  contract (the population walk's, [[namesakeBreaches]]) holds it from that commit on. */
  def namesakeAmbiguous(@scala.annotation.unused resolution: Resolution): Set[models.ListingKey] = Set.empty

  /** The namesake-ambiguous listings of `resolution` a rule outside kinds (i) and (ii) decided: (listing, its rule). */
  def namesakeBreaches(resolution: Resolution): Seq[(models.ListingKey, String)] = {
    val ambiguous = namesakeAmbiguous(resolution)
    resolution.decisions.flatMap(d => d.members.filter(ambiguous).map(key => key -> DecidedBy.of(d, key)))
      .filterNot { case (_, rule) => mayDecideNamesakeAmbiguous(rule) }
  }
}
