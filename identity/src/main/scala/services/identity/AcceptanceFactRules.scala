package services.identity

import services.identity.AcceptanceRule.{Weighing, isEligible}
import services.identity.measures.{FactRelations, Film, IdentityMeasures, ListingShape, MultiFilmBill, Scored, SearchMeasures,
  StageRelays, TitleTextMeasures}
import services.titles.YearWindow

/** What the acceptance rules that take a film by the listing's facts ask — a sole work, a director's work or title, a
 *  dated title, and the calibrated rules, favoured and unrivalled ([[AcceptanceRuleTable]]) — and the fallback a no-match
 *  may fall back to, unless its facts contradict it, and the candidate its evidence leans to.
 *  Functions of the candidates and the listing they score, and of the evidence `weights` where one asks them; whether
 *  the listing bills several works is `titleRules`' to say. [[Acceptance]] makes the one its rules ask. */
private[identity] final class AcceptanceFactRules(weights: EvidenceWeights, titleRules: AcceptanceTitleRules) {
  import titleRules.billsSeveralWorks

  /** What [[contradicted]] read: the year distance and the runtime delta the listing published. */
  private[identity] def contradiction(scored: Scored): String =
    (scored.number("year.distance").map(d => f"year.distance=$d%.0f") ++ scored.number("runtime.delta").map(d => f"runtime.delta=$d%.0f")).mkString(" ")

  /** The film a cluster no TMDB film was taken for FALLS BACK to, from a fallback source's pool (a `fallback`
   *  [[FamilyScope]]): the best eligible film whose record carries the listing's title as its own — its title, original
   *  or an alternative title — CORROBORATED by a fact the listing publishes, its director or its very year, contradicted by
   *  none, on a listing billing one work, with no rival so titled that its facts do not favour it over. Stricter than any
   *  TMDB rule: a fallback record has no search rank or popularity to lend, and a wrong film is worse than none (measured
   *  2026-09-30: every correct rating page for a film TMDB lacks came from a row publishing a year or a director). */
  def fallback(ranked: Seq[Scored]): Option[Scored] = {
    // on the source's own record only — one parsed from IMDb's answer always holds its principal credits — never the
    // title and year a suggestion gave before the record was filed, which no credit or running time could rule out
    val titled = ranked.filter(isEligible).filter(scored => scored.category("title").exists(IdentityMeasures.Rivalling) && scored.candidate.film.directors.isDefined)
    titled.headOption.filter(best => !MultiFilmBill.billsTwoWorks(best.listing) && !contradicted(best) && corroborated(best) &&
      !best.category("director").contains("different") && titled.drop(1).forall(weights.favours(best, _)))
  }

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

  /** Does the listing bill a NAME alone ([[services.titles.billing.Credits.billsOnlyAName]]) that the record's longer title merely
   *  leads, with no fact the venue states agreeing — its year, its running time, its director, an original title naming
   *  the record whole? A performer billed bare is the performer's evening, not a recording of one programme: DE Filmforum
   *  Landsberg's "Helmut Schleich" (the Kabarettist live) is not "Helmut Schleich: Kauf, du Sau" (2018), UK Pavilions
   *  Teignmouth's "Patrick Monahan" not "Patrick Monahan: Live" (2011). Dated alike, Lichtburg's "Berliner Philharmoniker"
   *  [2026] is their "Silvesterkonzert 2026". Only a record titled as a programme after the name ([[titledAsAProgramme]]):
   *  PL SDK's "Left-Handed Girl" is the film TMDB titles "Left-Handed Girl. To była ręka… diabła!", a title and its
   *  distributor's subtitle. */
  private[identity] def aNameAlone(scored: Scored): Boolean = {
    val listing = scored.listing
    val billed  = listing.rawTitle.getOrElse(listing.title)
    scored.category("title").contains("fragment") && services.titles.billing.Credits.billsOnlyAName(billed) &&
      titledAsAProgramme(billed, scored.candidate.film) &&
      !scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency) &&
      !IdentityMeasures.runtimeGap(scored.measures).exists(_ <= BilledRuntime) &&
      !IdentityMeasures.sameDirector(scored.measures) && !scored.category("originalTitle").contains("match")
  }

  /** Does one of the record's titles continue `name` as a programme of its: "<name>: …" or "<name> - …" ("Helmut Schleich:
   *  Kauf, du Sau", "Patrick Monahan: Live"), case and spacing aside — not a sentence after it, a film's subtitle. */
  private[identity] def titledAsAProgramme(name: String, film: Film): Boolean = {
    val words     = name.trim.split("\\s+").map(java.util.regex.Pattern.quote).mkString("\\s+")
    val programme = ("(?iu)^" + words + """\s*(?::|\s[-–—]\s)""").r
    (film.title +: (film.originalTitle.toSeq ++ film.alternativeTitles)).exists(title => programme.findPrefixOf(title.trim).isDefined)
  }

  /** How far a billed work's running time may stand from the venue's: a broadcast's interval and introduction. */
  private[identity] val BilledRuntime = 15
  /** The candidates whose WORK the listing bills under another subtitle, or publishes alone, by the director it credits,
   *  that no published year or runtime contradicts (`IdentityMeasures.sharesWork`, `titleIsWorkOf`, or the original title
   *  the venue publishes, `originalTitleIsWorkOf`: DE "Ein Hund namens Quill", published as "Quill" by Yōichi Sai, 2004,
   *  is his "Quill - Ein Freund für´s Leben"; three venues' "Leonas" is Cotelo's "Leonas, el instinto más salvaje", fifth
   *  in TMDB's search for the word): [[AcceptanceRuleTable.DirectorsWork]]'s to pick one of.
   *
   *  Both titles bill the work under a banner ("Royal Shakespeare Company: Macbeth", "RSC Live: Macbeth") and the running
   *  times agree: a staging the credited director made, not one of the word's many films — when it is the only record
   *  billing that work, whatever its running time: two seasons of one staging (Oliver Mears's Tosca, 2025/26 and
   *  2026/27, the later with no credits or running time yet) are the season's to tell apart, not this rule's — counting
   *  every record billing it that credits no OTHER director: a season's record often credits nobody yet. */
  private[identity] def directorsWorks(weighing: Weighing): Seq[Scored] = {
    lazy val billedAlike = weighing.eligible.count(scored => billsAStaging(scored) && !scored.category("director").contains("different"))
    def billedWork(scored: Scored) = scored.ownCredit && billsAStaging(scored) && billedAlike == 1 &&
      IdentityMeasures.runtimeGap(scored.measures).exists(_ <= BilledRuntime)
    weighing.eligible.filter(scored => scored.ownCredit &&
      (IdentityMeasures.sharesWork(scored.listing, scored.candidate.film) || bareWork(scored) || billedWork(scored)) && !contradicted(scored))
  }
  private def bareWork(scored: Scored) =
    (IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film) orElse IdentityMeasures.originalTitleIsWorkOf(scored.listing, scored.candidate.film))
      .exists(words => words >= 2 || scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency))
  private def billsAStaging(scored: Scored) = StageRelays.billings(scored.listing, scored.candidate.film).nonEmpty

  /** The eligible candidates the listing's title — or the original title its venue publishes — names EXACTLY that credit
   *  the director the listing credits, when no published year or runtime contradicts them: PL Kinoteka's "Podróżnik",
   *  published as Kiarostami's original, is his film ([[AcceptanceRuleTable.DirectorsTitle]]'s to pick one of).
   *
   *  The original title names it only when it stands apart from the billing ([[MeasuredListing.originalStandsApart]])
   *  and the billed title names no eligible record exactly: a ticket page that copies the original title and director of
   *  the record a search for its billing hit states nothing that record does not, so the two cannot confirm it (DE
   *  kinoheld's "Tino Bomelino" as Tino {Anna Lucía Alcalá Pérez}, "Ganes" as Ganges; Fontane Klub's "Ich bin Dein",
   *  Iram Haq's film by its title and runtime, credited to another "Jeg er din"'s director — run 38578038277).
   *
   *  A whole delimited piece of the billing names it as its bare title does, the rest a banner, as `segment-top-hit` reads
   *  it ([[byPiece]]) — one word too, the credited director telling the word's films apart: UK Keswick Alhambra's "KFC
   *  Presents: Hen" {György Pálfi} is his 2026 "Hen", not the Afrikaans 2025 one TMDB ranks first. */
  private[identity] def directorsTitles(weighing: Weighing): Seq[Scored] = {
    val eligible = weighing.eligible
    lazy val titled = eligible.exists(_.category("title").contains("exact"))
    def byOriginal(candidate: Scored) =
      candidate.category("originalTitle").contains("match") && candidate.listing.originalStandsApart && !titled
    eligible.filter(candidate => (candidate.category("title").contains("exact") || byOriginal(candidate) || byPiece(candidate)) &&
      candidate.ownCredit && !contradicted(candidate))
  }

  /** Does a whole delimited piece of the listing's OWN billing (a banner's rest, after its colon, dash or pipe:
   *  `SearchTitles.candidates` of its title alone) name the candidate, standing for the whole
   *  ([[SearchMeasures.pieceStandsForTheWhole]], one word enough) in a billing of one work? Not a shape its published
   *  original title or its search titles give it: a ticket page copying the original title and director of the record a
   *  search hit (DE kinoheld's "DAS PACK" as Pack {Peiqi Peng}) states nothing that record does not. */
  private[identity] def byPiece(candidate: Scored): Boolean =
    candidate.category("title").contains("segment") && billedAsAPiece(candidate) && !MultiFilmBill.billsTwoWorks(candidate.listing) &&
      !IdentityMeasures.joinsAnotherWork(candidate.listing, candidate.candidate.film) &&
      SearchMeasures.pieceStandsForTheWhole(candidate.listing, candidate.candidate.film, words = 1)

  private def billedAsAPiece(candidate: Scored): Boolean = {
    val listing = candidate.listing
    val whole   = listing.ownTitles.map(IdentityMeasures.key).toSet
    val pieces  = listing.ownTitles.flatMap(services.titles.SearchTitles.candidates(_, None)).map(IdentityMeasures.key)
      .filter(key => key.nonEmpty && !whole(key)).toSet
    candidate.candidate.film.ownTitles.exists(title => pieces(IdentityMeasures.key(title)))
  }

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

  // ── what the calibrated rules ask ───────────────────────────────────────────────────────────

  /** The runner-up `best`'s own facts must outweigh and do not ([[AcceptanceRuleTable.FavouredCalibrated]]): another
   *  film the billing could name, one the listing's own evidence reached. */
  private[identity] def runnerUpItsFactsDoNotBeat(weighing: Weighing, best: Scored): Option[Scored] =
    weighing.eligible.filterNot(r => (r eq best) || !r.ownReach || IdentityMeasures.eventNamed(r.listing, r.candidate.film)).headOption
      .filterNot(weights.favours(best, _))
  /** `best` and the eligible records the listing's own evidence reached: a sibling's search alone brings a record and
   *  takes it away. */
  private[identity] def ownReached(weighing: Weighing, best: Scored): Seq[Scored] = weighing.eligible.filter(r => (r eq best) || r.ownReach)

  /** Another eligible film of `best`'s credited director that the title names, or that the listing's year dates as
   *  closely, when the title names nothing of `best` (`title=none`): walking a director's credit reaches his films, and a
   *  title naming none of two his facts fit alike picks neither — DE "München Mord: Leben und Sterben in Schwabing" [2019]
   *  {Sascha Bigler}, a TV episode, is neither of his 2019 films; UK "Gremlins Special Event" {Joe Dante} is not his
   *  "Innerspace", however its 120 minutes fit, beside the Gremlins it names. A translation the facts single out stands
   *  (PL "Fach" is Rossi's "Il Mestiere"). */
  private[identity] def anotherOfItsDirector(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    if (!best.category("title").contains("none") || !IdentityMeasures.sameDirector(best.measures) || inflects(best)) None
    else eligible.find(other => (other ne best) && IdentityMeasures.sameDirector(other.measures) &&
      (other.titleNamesIt || (best.number("year.delta").isDefined && other.number("year.delta") == best.number("year.delta"))))
  /** Does a word of the billing inflect a word of the record's title ("Osobliwy świat Hieronymusa Boscha" of "The
   *  Curious World of Hieronymus Bosch"): a title in another language naming the film, though it shares no word whole. */
  private[identity] def inflects(scored: Scored): Boolean = {
    import services.titles.TitleContainment.tokens
    val billed = scored.listing.ownTitles.flatMap(tokens)
    scored.candidate.film.titles.flatMap(tokens).filter(_.length >= IdentityMeasures.StemLetters).exists(word => billed.exists(_.startsWith(word)))
  }

  /** The film a node's evidence LEANS to though no rule took it: the best eligible candidate on a listing billing one
   *  work, at [[LeanMargin]] times the runner-up's probability — every fact the listing publishes weighed in it, the
   *  priors lending — and no closer record. Not the runner-up rule's facts alone: a bare title's "facts" are the
   *  records' missing credits (US "Lady Frankenstein", 35.8% beside 2025's "Frankenstein" at 4.0%, lost on its record
   *  crediting a director the listing does not name). Not a match: the film a no-match's card may keep the ratings of, when an
   *  earlier answer gave the card that film's ([[IdentityProjectionPlan]]). PL "Tatarak" read 18.4% for Wajda's 2009
   *  film, its 1965 namesake 3.3%; "Lalka" leans to the 2026 film at 68.1%, not the 1968 one the old pipeline rated
   *  it as; "Bolek i Lolek" (28.9% beside "Reksio" at 28.9%) and the 1986 and 2025 "Caravaggio" lean to neither. */
  def leaning(ranked: Seq[Scored]): Option[Scored] =
    Option.when(ranked.headOption.forall(any => !MultiFilmBill.billsTwoWorks(any.listing)) && !billsSeveralWorks(ranked))(ranked.filter(isEligible))
      .flatMap(eligible => eligible.headOption.filter(_.untaken.isEmpty).filter(best => eligible.lift(1).forall(runnerUp =>
        best.probability >= Acceptance.LeanMargin * runnerUp.probability) && closerThan(best, eligible).isEmpty))

  /** The eligible record the listing's title sits inside ([[titledCloser]]) whose answered facts
   *  `best`'s do not beat, if any: then `best`, which the title only overlaps, is not the listing's
   *  film on that evidence — on its own or pooled. */
  private[identity] def closerThan(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    eligible.find(closer => (closer ne best) && titledCloser(closer, best) && weights.factsAnswered(best, closer) <= weights.factsAnswered(closer, closer))

  /** Does `closer`'s record hold the listing's title, or name a whole piece of it
   *  ([[IdentityMeasures.ContainingRelations]]), while `other`'s only overlaps it, names it by no
   *  original or alternative title (PL "Following" is Nolan's "Śledząc" by its original title), and
   *  does not hold `closer`'s title itself ("English National Ballet presents The Sleeping Beauty"
   *  holds "The Sleeping Beauty")? Then `other` is taken over it only when the facts `closer`'s record
   *  answers favour `other` ([[EvidenceWeights.factsAnswered]]). ES "BTS World Tour 'ARIRANG' In
   *  Buenos Aires: Live" ×82 took the Busan concert, and PL Kinoteka's double bill "Basia. Humor w
   *  paski mam + Kocia Szajka…" took "Basia. Radzę sobie!", on a credit and a runtime the record their
   *  title names merely does not state. */
  private[identity] def titledCloser(closer: Scored, other: Scored): Boolean = {
    def containing(scored: Scored) = scored.category("title").exists(IdentityMeasures.ContainingRelations)
    def words(scored: Scored) = services.titles.TitleContainment.tokens(scored.candidate.film.title).toSet
    containing(closer) && !containing(other) && !other.titleNamesIt && !(words(closer) subsetOf words(other))
  }

  /** A rival the title names by the very pieces it names `best` by whose pooled FACTS ([[EvidenceWeights.facts]]) fit
   *  better — or, named apart by disjoint pieces ([[IdentityMeasures.namedApart]]), at least as well. */
  private[identity] def namedAlikeAndFittingBetter(weighing: Weighing, best: Scored): Option[Scored] =
    weighing.eligible.find(rival => (rival ne best) && {
      val apart = IdentityMeasures.namedApart(best.listing, best.candidate.film, rival.candidate.film)
      val facts = weights.facts
      (namedAlike(best, rival) && facts(rival) > facts(best)) || (apart && facts(rival) >= facts(best))
    })
  /** A newer film named alike that a festival's bare billing names rather than `best` ([[ListingShape.festivalsOlderNamesake]]). */
  private[identity] def newerNamedAlikeByAFestival(weighing: Weighing, best: Scored): Option[Scored] =
    weighing.eligible.find(rival => (rival ne best) && namedAlike(best, rival) &&
      ListingShape.festivalsOlderNamesake(best.listing, best.candidate.film, rival.candidate.film))

  /** Does the billing name `rival` by the very pieces it names `best` by ([[IdentityMeasures.namingPieces]])? */
  private def namedAlike(best: Scored, rival: Scored): Boolean = {
    val pieces = IdentityMeasures.namingPieces(rival.listing, rival.candidate.film)
    pieces.nonEmpty && pieces == IdentityMeasures.namingPieces(best.listing, best.candidate.film)
  }

  /** Does the venue's billing name `best` only WITHIN another eligible candidate's longer title — the agreement's same
   *  concept ([[TitleTextMeasures.namedWithinAnother]]), read where pooled evidence alone would take it? PL "Kacper i
   *  Emma – najlepsi przyjaciele Edukacja Młode Horyzonty" names the 2026 short "Najlepsi przyjaciele" only as part of
   *  the 2013 "Kacper i Emma: Najlepsi przyjaciele". */
  private[identity] def namedWithinAnother(best: Scored, eligible: Seq[Scored]): Boolean = eligible.sizeIs > 1 && {
    val billed = Seq(services.titles.TitleContainment.tokens(best.listing.rawTitle.getOrElse(best.listing.title)))
    def spans(film: Film) = TitleTextMeasures.spansIn(billed, Seq(film))
    TitleTextMeasures.namedWithinAnother(spans(best.candidate.film), eligible.filterNot(_ eq best).map(other => spans(other.candidate.film)))
  }
}
