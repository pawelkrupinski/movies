package services.identity

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
enum RuleKind(val label: String) {
  case PopulationIndependent extends RuleKind("i")
  case Monotone extends RuleKind("ii")
  case NonMonotone extends RuleKind("iii")
}

object RuleContract {

  /** One declaration: the id (or `*` pattern) and why the rule is of its kind. */
  final case class Declared(pattern: String, why: String) {
    val exact: Boolean = !pattern.contains('*')
    private val regex  = pattern.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*").r
    def matches(ruleId: String): Boolean = if (exact) pattern == ruleId else regex.matches(ruleId)
  }

  /** Kind (i): the listing's own facts and TMDB alone. */
  val populationIndependent: Seq[Declared] = Seq(
    Declared("veto:learned-*", "learned listing-film cannot-link on the listing's own measures vs the TMDB record"),
    Declared("veto:learned-listing-film-probability-below-the-cannot-link-cut", "cut on weights.factsProbability: own facts only, rivals/rank excluded (EvidenceWeights.scala:38-42)"),
    Declared("veto:seasonsapart", "ListingConstraints.seasonsApart: listing's season vs film season/year (CannotLink.SeasonsApart.toString)"),
    Declared("veto:another-house-s-season-production", "StageRelays.namesSeasonProduction on listing banner vs film"),
    Declared("veto:a-screen-namesake-of-the-work-its-stage-house-s-billing-names", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-record-of-another-work-than-its-stage-house-s-billing-names", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:another-production-of-its-work-than-the-season-its-screenings-fall-in", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-catalogue-namesake-of-a-bare-billing-under-a-film-festival-s-banner", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:one-work-of-a-double-bill-whose-other-piece-none-of-its-titles-holds", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-part-its-title-gives-a-subtitle-none-of-the-film-s-titles-holds", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:an-instalment-its-title-numbers-whose-subtitle-none-of-the-film-s-titles-holds", "CandidateScoring.evidenceDenial: listing's own billing vs the film record"),
    Declared("veto:a-namesake-of-the-film-whose-director-its-billing-names", "directorBilled: billing names a director, film's banner does not"),
    Declared("veto:another-instalment-than-the-one-the-title-numbers", "numeral category, own title vs film"),
    Declared("veto:pinned-never-this-film", "admin pin on the node's listing"),
    Declared("veto:its-title-names-it-only-by-the-venue-s-own-name", "namesOnlyItsVenue"),
    Declared("veto:its-title-names-it-only-by-a-festival-s-programme-slot", "namesOnlyAProgrammeSlot"),
    Declared("veto:its-title-bills-a-film-festival-by-its-name-alone-the-programme-no-film", "billingOf(node).festival"),
    Declared("veto:it-bills-a-silent-film-and-this-one-is-of-the-sound-era", "silentBillingDenies"),
    Declared("veto:its-venue-s-own-cast-field-names-none-of-its-top-billed-cast-and-no-fact-the-venue-states-agrees-with-it", "CastEvidence.mismatch over the node's listings"),
    Declared("veto:named-only-by-the-title-its-feed-*-copied*", "OPEN; feed-copied original title alone names it"),
    Declared("veto:a-short-its-feed-*-linking-*", "OPEN; short beside the feed's link"),
    Declared("search:unanswered", "the node's own query unanswered (ResolverDecisions.scala:149-153)"),
    Declared("rule:none", "no refusal recorded"),
    Declared("rule:no-evidence-class-measured-for-it", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): classProbability of own measures"),
    Declared("rule:a-published-fact-weighs-against-it", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): speaksAgainst own facts"),
    Declared("rule:the-title-numbers-another-instalment", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): numeral category"),
    Declared("rule:another-piece-of-its-billing-is-a-performance-the-film-is-not", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): own billing"),
    Declared("rule:its-billing-credits-its-work-to-an-author-who-made-no-such-film", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): own billing credit"),
    Declared("rule:its-billing-is-a-name-alone-which-the-record-s-longer-title-only-leads-and-no-fact-agrees", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): aNameAlone"),
    Declared("rule:the-billing-reads-as-no-film", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): readsAsNoFilm: event words"),
    Declared("rule:a-double-bill", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): MultiFilmBill.billsTwoWorks(listing)"),
    Declared("rule:the-title-joins-another-work-to-it", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): joinsAnotherWork"),
    Declared("rule:its-house-s-record-is-from-another-year-than-its-title-dates", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): titleYear.delta"),
    Declared("rule:it-bills-no-house-s-relay-of-a-work-in-a-season", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): listing.relayBilled"),
    Declared("rule:a-published-year-contradicts-it", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): year.distance"),
    Declared("rule:it-credits-another-director", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): director category"),
    Declared("rule:the-title-names-its-season", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): listing.seasonYear"),
    Declared("rule:a-published-fact-contradicts-it", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): contradicted()"),
    Declared("rule:the-year-it-states-is-another-season-s", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): listing.year vs season"),
    Declared("rule:not-its-search-s-first-hit", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): own rank"),
    Declared("rule:not-named-by-a-piece-of-the-title", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): title category"),
    Declared("rule:its-piece-cannot-stand-for-the-whole-title", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): standsForTheWhole"),
    Declared("rule:the-title-dates-another-year", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): titleYear.delta"),
    Declared("rule:a-bare-billing-names-the-film-only-by-an-alternative-title-and-tmdb-s-search-of-it-finds-other-films-not-this-one", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): own search ranks"),
    Declared("rule:the-title-does-not-name-its-search-s-only-film", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): own title vs sole hit"),
    Declared("withdrawal:judged-wrong", "human judgement of this listing's take")
  )

  /** Kind (ii): reads the population, one way only. */
  val monotone: Seq[Declared] = Seq(
    Declared("search:found-nothing", "no candidate in the node's scoring of the family pool: siblings' searches can only add candidates"),
    Declared("rule:no-candidate", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence; pool can only grow"),
    Declared("rule:no-first-hit-is-the-title-s-work", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence over own first hits"),
    Declared("rule:no-film-of-its-credited-director-shares-its-work", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:no-film-of-its-exact-title-by-its-credited-director", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:no-film-of-its-title-from-the-year-its-title-dates", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:no-record-bills-its-work-under-its-house", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:no-record-of-its-house-s-relay-of-its-work-from-its-season", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:no-season-record-carries-its-title-or-its-work-under-its-house", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:no-film-its-whole-title-names-is-its-search-s-first-hit", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence over own search ranks"),
    Declared("rule:the-title-names-no-film", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:imdb-suggests-none-of-its-candidates", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("rule:no-search-of-its-returned-a-single-film", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): absence"),
    Declared("pin", "Pinned if any cluster member is pinned (ResolverDecisions.scala:16): own or joined pin")
  )

  /** Kind (iii): weighs the listing against the population. */
  val nonMonotone: Seq[Declared] = Seq(
    Declared("accept:sole-work", "one() of first hits + noneOf 'the title names another film' over the family-pool ranking (AcceptanceFactRules soleWorkWhy)"),
    Declared("accept:favoured-calibrated", "runner-up comparison (eligible.lift(1)) + closerThan; probability carries the rivals count (AcceptanceCalibratedRules:21)"),
    Declared("accept:exact-top-hit", "unique exact top hit ('two exact top hits'), unopposed(): no rival fitsBetter, class probability with rivals (AcceptanceTitleRules topHitWhy)"),
    Declared("accept:segment-top-hit", "one() 'the title names two films' + unopposed rival check (AcceptanceTitleRules segmentTopHitWhy)"),
    Declared("accept:sole-result", "one() over soleResult + noneOf 'the title names another film' (AcceptanceTitleRules soleResultWhy)"),
    Declared("accept:imdb-suggested", "suggested.count(sameDirector)==1 / count(exact)==1, one(), TMDB ranks another (AcceptanceTitleRules imdbSuggestedWhy)"),
    Declared("accept:directors-work", "billedAlike==1 count + one() 'two films of its director share its work' (AcceptanceFactRules directorsWorkWhy)"),
    Declared("accept:directors-title", "one() 'two films of its title by its director' (AcceptanceFactRules directorsTitleWhy)"),
    Declared("accept:dated-title", "one() 'two films of its title from that year' (AcceptanceFactRules datedTitleWhy)"),
    Declared("accept:house-production", "one() + noneOf 'another record carries its title or fits as well' (AcceptanceStageRules houseProductionWhy)"),
    Declared("accept:season-record", "one() 'two seasons' records carry its title' (AcceptanceStageRules seasonRecordWhy); pre-empts via relayOrBroadcastSeasonRecord"),
    Declared("accept:relay-record", "one() 'two records of its house's relay...' (AcceptanceStageRules relayRecordWhy)"),
    Declared("accept:season-production", "seasonProduction: the sole one, or own(first)>own(second) tie-break (Acceptance.scala:423)"),
    Declared("accept:joined", "OwnMatch with no nameable rule for key or any member: film reached via join to another listing's own match; must-links/cannot-links from other listings decide it"),
    Declared("pooled:season-production", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala:380); same uniqueness/rival checks as alone rule"),
    Declared("pooled:season-record", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard; same uniqueness/rival checks as the alone rule"),
    Declared("pooled:unrivalled-calibrated", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala:380); same uniqueness/rival checks as alone rule ; unrivalled: 'a film the title names alike fits its facts better'"),
    Declared("pooled:exact-top-hit", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala:380); same uniqueness/rival checks as alone rule"),
    Declared("pooled:imdb-suggested", "cluster's POOLED evidence (scope.pooled) + tiedAtTop guard (Acceptance.scala:380); same uniqueness/rival checks as alone rule"),
    Declared("pooled:unnamed", "PooledMatch whose film no pooled rule names: ClusterVoting.familyMajority ('N of M venue(s)' majority) or a vote ResolverRun.scala:90-102"),
    Declared("pooled:no-film", "no-match listing without a node trace (also IdentityTraceStore.scala:42): cluster-level no-film"),
    Declared("veto:denied", "fallback: pooled best candidate denied with no reason text (ResolverDecisions.scala:163-166 picks the denying member)"),
    Declared("veto:imdb-suggested-only", "blocker: every candidate denied or suggested-only, none carrying a denial; over the family-pool ranking"),
    Declared("veto:bills-both-works", "billsSeveralWorks matches slash/'and' pieces against pool candidates + billingUnderBanner reads cluster siblings (ResolverRun.scala:113-114)"),
    Declared("veto:billed-as-one-piece", "billedAsPiece reads max venues.corroborating across members and corpus-recurring segments (ResolverRun.scala:148-149)"),
    Declared("veto:uncorroborated-namesake", "Corroboration.refused: nothing of its own corroborates the cluster's film beside others' backing (ResolverRun.scala:133)"),
    Declared("veto:a-member-s-own-evidence-rules-it-out", "pooled candidate denied because ANOTHER member's own scoring denies it"),
    Declared("veto:vetoed-not-released-in-*-as-namesake-*-is", "OPEN; relative to a released namesake in the pool, lifted by venues.corroborating (FamilyScope.scala:98-101)"),
    Declared("veto:a-film-of-the-venue-s-own-language-its-title-shares-no-word-with", "ownLanguageUntitled: probability >= every named candidate's (FamilyScope.scala:~160)"),
    Declared("veto:another-record-of-a-broadcast-its-film-database-holds-twice", "anotherRecordOfItsBroadcast: relative to another pool record"),
    Declared("veto:its-director-s-other-film-which-the-title-does-not-name", "needs another candidate the title names by the same director (titled)"),
    Declared("veto:a-namesake-of-the-film-its-credited-director-made-of-its-title", "creditedNamesake / directedBy filmography reach: relative to another candidate"),
    Declared("veto:another-house-s-staging-of-its-work-beside-its-own-house-s-record", "besideItsOwnHouse: relative to another housed candidate"),
    Declared("veto:its-title-names-it-only-by-a-programme-tag-billed-beside-many-titles", "namesOnlyATag: corpus count of titles a tag is billed beside"),
    Declared("veto:its-venue-s-own-text-credits-another-film-s-director-and-none-of-this-relay-s", "RelayRecords.contradictedByOwnText reads the other reached films"),
    Declared("veto:its-feed-s-link-*-*-overruled-*", "OPEN; rival backed by >= OverrulingSources venues / more venues billing a current release: counts"),
    Declared("withdrawn:sibling-denies-film", "Families.deniedBySibling: a title-linked sibling's evidence denies its own match"),
    Declared("rule:bills-several-works", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): Acceptance.scala:231 'alone' refusal; billsSeveralWorks matches pieces against pool candidates"),
    Declared("rule:its-own-facts-do-not-favour-it-over-the-runner-up", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): favoured-calibrated runner-up (preferred blocker, Decision.scala:235)"),
    Declared("rule:its-title-sits-inside-a-closer-record", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): closerThan another candidate"),
    Declared("rule:no-eligible-candidate", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): eligibility via denials, some pool/sibling-relative"),
    Declared("rule:below-the-rating-cut", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): probability carries rivals count / priorsLent over eligible"),
    Declared("rule:a-rival-fits-its-facts-better", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): fitsBetter rival"),
    Declared("rule:its-title-names-nothing-of-it-and-another-film-of-its-director-it-names-or-dates-as-closely-stands-beside-it", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): anotherOfItsDirector rival"),
    Declared("rule:untaken", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): Scored.untaken via shortUntaken reads pool features (FamilyScope.scala:~150)"),
    Declared("rule:its-page-reads-as-reused-only-a-copied-original-title-names-a-film-of-it-and-the-billing-says-more", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): another eligible candidate copiedBeyondItsBilling"),
    Declared("rule:two-first-hits-are-the-title-s-work", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:the-title-names-another-film", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): noneOf other candidate"),
    Declared("rule:two-films-of-its-director-share-its-work", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:two-films-of-its-title-by-its-director", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:two-films-of-its-title-from-that-year", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:two-records-bill-its-work-under-its-house", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:another-record-carries-its-title-or-fits-as-well", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): rival"),
    Declared("rule:two-records-of-its-house-s-relay-of-its-work-from-its-season", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:two-seasons-records-carry-its-title", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:two-exact-top-hits", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:its-exact-top-hit-is-denied", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): denial may be pool/sibling-relative"),
    Declared("rule:the-title-names-two-films", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:the-film-it-names-is-denied", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): denial may be pool-relative"),
    Declared("rule:no-imdb-suggestion-is-taken-by-its-year-its-director-or-as-the-only-one", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): count()==1 rungs"),
    Declared("rule:two-imdb-suggestions-qualify", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("rule:tmdb-ranks-another-film-the-title-names-above-it", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): rival ranked above"),
    Declared("rule:its-searches-returned-different-single-films", "refusal why (rule:<DecisionTrace.id(why)>, Decision.scala:235): uniqueness"),
    Declared("agreed:*", "several film-database families agree on a film for a cluster TMDB matched to nothing: the family's votes are pooled"),
    Declared("poster:vote", "PosterEvidence.vote: the ONE candidate any venue poster matches, no other within VoteBits"),
    Declared("broadcast", "Broadcast.take: the one record the cluster's screening days name, none denied"),
    Declared("catalogue", "Catalogue.take: refused if any stated listing contradicts the record (Catalogue.scala:88)"),
    Declared("event", "NonFilmEvents.of: EVERY listing must read as an event (NonFilmEvents.scala:91-96); one more listing can remove it"),
    Declared("fill:*", "the agreement stage fills a no-match from its family's and venues' takes (identity-unified-rules.json fill)"),
    Declared("correction:*", "the agreement stage switches a take on evidence its family and the feeds give"),
    Declared("withdrawal:*", "the agreement stage withdraws a take on evidence its family and the feeds give")
  )

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
  def namesakeAmbiguous(@scala.annotation.unused resolution: Resolution): Set[services.movies.ListingKey] = Set.empty

  /** The namesake-ambiguous listings of `resolution` a rule outside kinds (i) and (ii) decided: (listing, its rule). */
  def namesakeBreaches(resolution: Resolution): Seq[(services.movies.ListingKey, String)] = {
    val ambiguous = namesakeAmbiguous(resolution)
    resolution.decisions.flatMap(d => d.members.filter(ambiguous).map(key => key -> DecidedBy.of(d, key)))
      .filterNot { case (_, rule) => mayDecideNamesakeAmbiguous(rule) }
  }
}
