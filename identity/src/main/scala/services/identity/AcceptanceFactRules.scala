package services.identity

import services.identity.AcceptanceRule.{Weighing, isEligible}
import services.identity.measures.{FactRelations, Film, IdentityMeasures, Measure, MultiFilmBill, Scored, SearchMeasures, StageRelays}
import services.titles.YearWindow

/** What the acceptance rules that take a film by the listing's facts ask — a sole work, a director's work or title, a
 *  dated title ([[AcceptanceRuleTable]]) — and the fallback a no-match may fall back to, unless its facts contradict it.
 *  Functions of the candidates and the listing they score, and of the evidence weights where one asks them. */
private[identity] object AcceptanceFactRules {

  /** What [[contradicted]] read: the year distance and the runtime delta the listing published. */
  private[identity] def contradiction(scored: Scored): String =
    (scored.number("year.distance").map(d => f"year.distance=$d%.0f") ++ scored.number("runtime.delta").map(d => f"runtime.delta=$d%.0f")).mkString(" ")

  /** The film a cluster no TMDB film was taken for FALLS BACK to, from a fallback source's pool (a `fallback`
   *  [[FamilyScope]]): the best eligible film whose record carries the listing's title as its own — its title, original
   *  or an alternative title — CORROBORATED by a fact the listing publishes, its director or its very year, contradicted by
   *  none, on a listing billing one work, with no rival so titled that its facts do not favour it over. Stricter than any
   *  TMDB rule: a fallback record has no search rank or popularity to lend, and a wrong film is worse than none (measured
   *  2026-09-30: every correct rating page for a film TMDB lacks came from a row publishing a year or a director). */
  def fallback(weights: EvidenceWeights, ranked: Seq[Scored]): Option[Scored] = {
    // on the source's own record only — one parsed from IMDb's answer always holds its principal credits — never the
    // title and year a suggestion gave before the record was filed, which no credit or running time could rule out
    val titled = ranked.filter(isEligible).filter(scored => scored.category("title").exists(IdentityMeasures.Rivalling) && scored.candidate.film.directors.isDefined)
    titled.headOption.filter(best => !MultiFilmBill.billsTwoWorks(best.listing) && !contradicted(best) && corroborated(best) &&
      !best.category("director").contains("different") && titled.drop(1).forall(weights.favours(best, _)))
  }

  /** A fact the listing publishes agrees: the director it credits, or the year it gives, exactly. */
  private[identity] def corroborated(scored: Scored): Boolean =
    IdentityMeasures.sameDirector(scored.measures) || scored.number("year.distance").contains(0.0)

  /** The candidate read as its title's exact top hit, unrivalled — how a rule taking a film by another title (an IMDb
   *  suggestion, a search's only result) has its own facts weighed, the title's evidence granted. */
  private[identity] def asItsTitlesTopHit(scored: Scored): Scored = scored.copy(measures = scored.measures ++ TopHitMeasures)
  private[identity] val TopHitMeasures = Map("title" -> Measure.Category("exact"), "search.rank" -> Measure.Number(1),
    "rivals" -> Measure.Number(0))

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
}
