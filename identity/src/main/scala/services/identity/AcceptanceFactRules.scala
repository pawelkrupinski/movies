package services.identity

import services.resolution.YearWindow

/** The acceptance rules that take a film by the listing's facts: a sole work, a director's work or title, a dated title —
 *  and the fallback a no-match may fall back to, unless its facts contradict it.
 *  Part of [[Acceptance]], which mixes it in. */
private[identity] trait AcceptanceFactRules { this: Acceptance =>
  import weights.favours

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
    val titled = eligibleOf(ranked).filter(scored => scored.category("title").exists(IdentityMeasures.Rivalling) && scored.candidate.film.directors.isDefined)
    titled.headOption.filter(best => !MultiFilmBill.billsTwoWorks(best.listing) && !contradicted(best) && corroborated(best) &&
      !best.category("director").contains("different") && titled.drop(1).forall(favours(best, _)))
  }

  /** A fact the listing publishes agrees: the director it credits, or the year it gives, exactly. */
  private[identity] def corroborated(scored: Scored): Boolean =
    IdentityMeasures.sameDirector(scored.measures) || scored.number("year.distance").contains(0.0)

  /** The candidate read as its title's exact top hit, unrivalled — how a rule taking a film by another title (an IMDb
   *  suggestion, a search's only result) has its own facts weighed, the title's evidence granted. */
  private[identity] def asItsTitlesTopHit(scored: Scored): Scored = scored.copy(measures = scored.measures ++ TopHitMeasures)
  private[identity] val TopHitMeasures = Map("title" -> IdentityMeasures.Category("exact"), "search.rank" -> IdentityMeasures.Number(1),
    "rivals" -> IdentityMeasures.Number(0))

  /** A published year more than one off, or a runtime 30 minutes or more off: the listing's own facts against it. */
  private[identity] def contradicted(scored: Scored): Boolean =
    scored.number("year.distance").exists(_ > YearWindow.PublishedAdjacency) || IdentityMeasures.runtimeContradicts(scored.measures)

  /** The film whose WORK the listing's whole title is, when TMDB ranks it first, it is the only such
   *  candidate, and no other candidate is one the title names — the rest only a director's
   *  filmography reached. US venues' "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES" (×1,915 with São
   *  Paulo) is "…: Live Viewing", not the 2022 Seoul concert film its director also made; neither
   *  pipeline matched them. A work of one word is too many films' title ("It" of "It: Chapter Two").
   *  The listing may bill that work under a shorter subtitle of its own, the whole title running along
   *  the record's (DE ×140 and ES ×82 "…In Buenos Aires: Live"), when no other candidate bills the
   *  work: a banner leading the title is no work ("The Metropolitan Opera: La Fanciulla del West
   *  Encore" is not its 2018 staging). */
  private[identity] def soleWorkWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    def isWork(scored: Scored) = IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)
    def sharesItsWork(scored: Scored) = IdentityMeasures.billsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)
    def billsWork(scored: Scored) = scored.category("title").contains("fragment") && sharesItsWork(scored)
    for {
      work <- one(eligible.filter(scored => scored.rank.contains(1) && (isWork(scored) || billsWork(scored)) && !contradicted(scored)),
                "no first hit is the title's work", "two first hits are the title's work", firstHit(ranked))
      _    <- noneOf(eligible.find(other => (other ne work) && (other.titleNamesIt || (!isWork(work) && sharesItsWork(other)))),
                "the title names another film", work)
    } yield atOwn(work)
  }

  /** Does the listing bill a NAME alone ([[services.billing.Credits.billsOnlyAName]]) that the record's longer title merely
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
    scored.category("title").contains("fragment") && services.billing.Credits.billsOnlyAName(billed) &&
      titledAsAProgramme(billed, scored.candidate.film) &&
      !scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency) &&
      !IdentityMeasures.runtimeGap(scored.measures).exists(_ <= BilledRuntime) &&
      !IdentityMeasures.sameDirector(scored.measures) && !scored.category("originalTitle").contains("match")
  }

  /** Does one of the record's titles continue `name` as a programme of its: "<name>: …" or "<name> - …" ("Helmut Schleich:
   *  Kauf, du Sau", "Patrick Monahan: Live"), case and spacing aside — not a sentence after it, a film's subtitle. */
  private[identity] def titledAsAProgramme(name: String, film: IdentityMeasures.Film): Boolean = {
    val words     = name.trim.split("\\s+").map(java.util.regex.Pattern.quote).mkString("\\s+")
    val programme = ("(?iu)^" + words + """\s*(?::|\s[-–—]\s)""").r
    (film.title +: (film.originalTitle.toSeq ++ film.alternativeTitles)).exists(title => programme.findPrefixOf(title.trim).isDefined)
  }

  /** How far a billed work's running time may stand from the venue's: a broadcast's interval and introduction. */
  private[identity] val BilledRuntime = 15
  /** The one candidate whose WORK the listing bills under another subtitle, or publishes alone, by
   *  the director it credits, that no published year or runtime contradicts
   *  (`IdentityMeasures.sharesWork`, `titleIsWorkOf`, or the original title the venue publishes, `originalTitleIsWorkOf`:
   *  DE "Ein Hund namens Quill", published as "Quill" by Yōichi Sai, 2004, is his "Quill - Ein Freund für´s Leben"; three venues' "Leonas" is Cotelo's "Leonas, el
   *  instinto más salvaje", fifth in TMDB's search for the word): Multikino's "Cirque du Soleil:
   *  Kurios - Gabinet osobliwości" by Michel Laprise is his "…: KURIOS - Cabinet des curiosités".
   *  The pipeline took such hits by their director on 3,247 listings with no wrong match; two
   *  candidates fitting alike (a director's sequels of one work) are no answer. A one-word work
   *  also needs the published year, a word being many films' title. */
  private[identity] def directorsWorkWhy(ranked: Seq[Scored]): Verdict = {
    def bareWork(scored: Scored) =
      (IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film) orElse IdentityMeasures.originalTitleIsWorkOf(scored.listing, scored.candidate.film))
        .exists(words => words >= 2 || scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency))
    // Both titles bill the work under a banner ("Royal Shakespeare Company: Macbeth", "RSC Live: Macbeth") and the
    // running times agree: a staging the credited director made, not one of the word's many films — when it is the
    // only record billing that work, whatever its running time: two seasons of one staging (Oliver Mears's Tosca,
    // 2025/26 and 2026/27, the later with no credits or running time yet) are the season's to tell apart, not this rule's
    // — counting every record billing it that credits no OTHER director: a season's record often credits nobody yet.
    def bills(scored: Scored) = StageRelays.billings(scored.listing, scored.candidate.film).nonEmpty
    lazy val billedAlike = eligibleOf(ranked).count(scored => bills(scored) && !scored.category("director").contains("different"))
    def billedWork(scored: Scored) = IdentityMeasures.sameDirector(scored.measures) && bills(scored) && billedAlike == 1 &&
      IdentityMeasures.runtimeGap(scored.measures).exists(_ <= BilledRuntime)
    val found = eligibleOf(ranked).filter(scored => IdentityMeasures.sameDirector(scored.measures) &&
      (IdentityMeasures.sharesWork(scored.listing, scored.candidate.film) || bareWork(scored) || billedWork(scored)) && !contradicted(scored))
    one(withoutHollow(found), "no film of its credited director shares its work", "two films of its director share its work", credited(ranked)).map(atOwn)
  }

  /** The one eligible candidate the listing's title — or the original title its venue publishes — names EXACTLY that
   *  credits the director the listing credits, when no published year or runtime contradicts it: a title and its director
   *  name one film together, however the database's search ranks it or however few venues list it
   *  (US "Man of Iron" {Andrzej Wajda}, seventh in TMDB's search behind "Iron Man"; PL Kinoteka's "Podróżnik", published
   *  as Kiarostami's original). Two such records are no answer.
   *
   *  The original title names it only when it stands apart from the billing ([[IdentityMeasures.Listing.originalStandsApart]])
   *  and the billed title names no eligible record exactly: a ticket page that copies the original title and director of
   *  the record a search for its billing hit states nothing that record does not, so the two cannot confirm it (DE
   *  kinoheld's "Tino Bomelino" as Tino {Anna Lucía Alcalá Pérez}, "Ganes" as Ganges; Fontane Klub's "Ich bin Dein",
   *  Iram Haq's film by its title and runtime, credited to another "Jeg er din"'s director — run 38578038277). */
  private[identity] def directorsTitleWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    lazy val titled = eligible.exists(_.category("title").contains("exact"))
    def byOriginal(candidate: Scored) =
      candidate.category("originalTitle").contains("match") && candidate.listing.originalStandsApart && !titled
    one(withoutHollow(eligible.filter(candidate =>
      (candidate.category("title").contains("exact") || byOriginal(candidate)) &&
      IdentityMeasures.sameDirector(candidate.measures) && !contradicted(candidate))),
      "no film of its exact title by its credited director", "two films of its title by its director", credited(ranked)).map(atOwn)
  }

  /** The one eligible candidate the listing's title names EXACTLY from the year its title dates it
   *  (a year off at most, as a release and a premiere differ), when nothing it publishes contradicts
   *  it — a credited director included: US "Troll (1986)" is the 1986 film, though TMDB ranks "Troll 2"
   *  and a 2022 "Troll" above it ([[namedButForItsYear]]: decorated, as "… Encore (2027)", too). Two
   *  such records are no answer. */
  private[identity] def datedTitleWhy(ranked: Seq[Scored]): Verdict =
    one(eligibleOf(ranked).filter(candidate => namedButForItsYear(candidate) &&
      candidate.number("titleYear.delta").exists(FactRelations.nearDelta) && !contradicted(candidate) &&
      !candidate.category("director").contains("different")),
      "no film of its title from the year its title dates", "two films of its title from that year",
      if (ranked.exists(_.number("titleYear.delta").isDefined)) "" else "the title dates no year").map(atOwn)

  /** Is the listing's title, years aside, the candidate's own title or original title — or that
   *  title decorated along one edge ("La Fanciulla del West Encore" of the Met's "… 2026/27: La
   *  Fanciulla del West")? Asked only beside a title year that agrees ([[datedTitleWhy]]). */
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
      titles.exists(title => title == own || (title.sizeIs >= 2 && services.movies.TitleContainment.isTokenRun(title, own)) ||
        (title.sizeIs == 1 && pieces(title))))
  }
}
