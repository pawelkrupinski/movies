package services.identity.measures

import services.titles.billing.Credits
import services.titles.{TitleContainment, TitleSeason}

import IdentityMeasures.*
import Measure.Category

/** The searches a listing issues (a stage relay's by its work and its season among them), the works it credits, the
 *  IMDb ids and titles it reads, and the title groups and rivals of its candidates.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object SearchMeasures {
  /** The title searches a listing's evidence issues: every title shape and its original title,
   *  each asked WITHOUT a year (TMDB dates a film by first release, a venue by production or
   *  re-release). ONE definition: the calibration's candidate pools, the resolver's queries and
   *  the recording sweep all read it. */
  def searchQueries(l: MeasuredListing): Seq[String] = {
    val asked = l.shapes ++ l.originalTitle ++ seasonProductionQueries(l) ++ stageWorkQueries(l) ++ MultiFilmBill.billedWorks(l) ++ uncredited(l) ++
      IdentityMeasures.shapes(RelayStage.shortYearCut(l))
    (asked ++ asked.flatMap(beforeItsYear)).map(_.trim).filter(_.nonEmpty).distinct
  }

  /** A stage broadcast a venue bills with no season but with its year — DE "Royal Ballet & Opera im Kino: Manon"
   *  [2026] — searched as its work's name and that year ("Manon 2026"): the house's season record carries both, while
   *  the work alone ranks it below every namesake film. A relay billing its house, a credit or a running time beside it,
   *  is searched with the season its screenings fall in too: UK Picture House Uckfield's "Tosca (ROH)" {Oliver Mears}
   *  [205′] and Flicks' "Royal Opera House: Carmen" [220′] ×13 screen the RBO 2026/27 relays, whose records the work's
   *  name alone never reaches — which season's record it is stays the season rules' to tell ([[MeasuredListing.screenedSeason]]).
   *  So is a bare work beside a running time ([[StageRelays.barelyTimed]]: UK Empire Theatre Halstead's "Romeo and Juliet" [170′]).
   *  Beside a year it states as well: Flicks dates "Royal Opera House: Carmen" 2024, the year of the page it links, and
   *  "Carmen 2024" finds the 2024 revival, never the 2026/27 broadcast its venues screen in November 2026.
   *  KNOWN GAP (convergence 38627416973, accepted.tsv): UK Lonsdale Alhambra Penrith's "RB&O 26/27: <work>" ×3 reached
   *  the season's record only by joining its other billings' cluster; once those took it alone, the learned
   *  listing-listing cut kept Penrith apart and it takes none. Alone it takes the record (IdentityResolverCasesSpec's
   *  short-season case): the follow-up is the corpus-level join. */
  private[identity] def stageWorkQueries(l: MeasuredListing): Seq[String] =
    if (l.seasonYear.isDefined) Nil
    // the year as the relay's record reads it ([[RelayStage]]): its field, its title's bracket, its banner ("RBO live 2027: Tosca"), else its
    // broadcast season -- or nothing searched "Tosca 2027", and the house's 2026/27 record could stay out of the pool
    else (l.statedYear.orElse(StageRelays.bannerYear(l)).orElse(l.dating.broadcastSeason).toSeq ++
      l.dating.screenedSeason.filter(_ => RelayStage.searchedInItsSeason(l) || StageRelays.barelyTimed(l)) ++ closedSeason(l)).distinct.flatMap(year => workSearches(l, year))

  /** The season its screenings fall in, when the year it states is the one that season CLOSES: a season record carries
   *  the year it opens ("Royal Ballet & Opera 2026/27: Romeo and Juliet"), which TMDB's search finds by "Romeo and Juliet
   *  2026", never by "Romeo and Juliet 2027". UK Riverside Theatre Woodbridge's "Romeo and Juliet (2027)", screening on
   *  25 May 2027, reached the RBO relay aired that day only by joining a sibling whose search did: a take that came and
   *  went with the family's venues (structural fix 2). A function of the listing's own year and days. */
  private def closedSeason(l: MeasuredListing): Option[Int] = l.dating.screeningsSeason.filter(season => l.statedYear.contains(season + 1))

  /** The work `l` bills searched beside `year`, under each of its search names ("Manon 2026"). */
  private[identity] def workSearches(l: MeasuredListing, year: Int): Seq[String] =
    l.billedStageWorks.toSeq.sorted.flatMap(StageWorks.resolver.searchNames).map(name => s"$name $year")

  /** A season production searched as its WORK AND ITS SEASON ("Manon 2026"): a house's record of
   *  it ("Royal Ballet & Opera 2026/27: Manon") carries both, however the venue spells the house,
   *  while the work alone ranks it below every namesake. Only the pieces of a title naming the
   *  season ask it, without a bracketed year. */
  private[identity] def seasonProductionQueries(l: MeasuredListing): Seq[String] =
    l.seasonYear.toSeq.flatMap { season =>
      val seasonTitles = l.ownTitles.filter(t => TitleSeason.of(Seq(t)).isDefined)
      val works = shapes(seasonTitles).filter(t => TitleSeason.of(Seq(t)).isEmpty).map(withoutYears(_).trim).filter(_.nonEmpty).distinct
      // a work named in another language than the record's is asked for by its search name too ("The Nutcracker 2026")
      // — including one named before a translated subtitle ("Cosi fan tutte. Tak czynią wszystkie"), as the match reads it
      // — and by every other name of the work it is filed under, a house's own ("Samson et Dalila 2026"), the work billed
      // within the piece too ("Camille Saint-Saëns SAMSON ET DALILA")
      val translated = works.flatMap(work => (StageWorks.resolver.named(key(work)) ++ StageWorks.resolver.named(key(StageRelays.beforeItsSubtitle(work))) ++
        StageRelays.stageWorksBilled(Seq(work), seasonNamed = true).toSeq).toSeq.sorted.distinct.flatMap(StageWorks.resolver.searchNames))
      (works ++ translated).distinct.map(work => s"$work $season")
    }

  /** A title up to the year it dates itself by in a bracket ("Człowiek z żelaza (1981) 4K" →
   *  "Człowiek z żelaza"): TMDB's search finds nothing for the bracketed spelling, and a search is
   *  asked without a year. */
  private[identity] def beforeItsYear(title: String): Option[String] =
    BracketedYear.findFirstMatchIn(title).map(m => title.take(m.start).trim).filter(_.exists(_.isLetter))

  /** The title without an anniversary it dates ("… 20th Anniversary") and a director's possessive
   *  credit before it ("Guillermo del Toro's …"): the work a re-release bills under both, which no
   *  piece of the title is. A credit is a name of two words or more, so "Schindler's List" keeps its own. */
  private def uncredited(l: MeasuredListing): Seq[String] = (Seq(l.title) ++ l.originalTitle).flatMap { title =>
    // the original title too: ES "Drácula. 30 Aniversario" is "Bram Stoker's Dracula 30th Anniversary" in it
    val undated = AnniversarySuffix.replaceFirstIn(title.trim, "").trim
    Seq(undated, PossessiveCredit.replaceFirstIn(undated, "").trim).filter(q => q.nonEmpty && q != title.trim)
  }
  private val AnniversarySuffix = """(?i)\s*[-–—:]?\s*\(?\d{1,3}(?:st|nd|rd|th)\s+anniversary\)?\s*$""".r
  private val PossessiveCredit  = """^\p{Lu}[\p{L}.-]*(?:\s+[\p{L}.-]+){1,3}['’]s\s+(?=\S)""".r
  /** `title` without the author's credit closing it ([[Credits.authorCredited]], the billing grammar's: "Fallen Angels by
   *  Noël Coward", "Die Physiker von Friedrich Dürrenmatt", "Wesele autorstwa Stanisława Wyspiańskiego"), when it has one
   *  and a title is left. A name of one word, or not capitalised, is the title's own: "Stand by Me", "Death by Chocolate",
   *  Polish "Żyć by tańczyć". A name after an accompaniment's word credits the musician, not the work's author: DE
   *  Kinostar Arthaus Heilbronn's "THE GENERAL - Stummfilm mit Klavierbegleitung von Andreas Benz" is "The General" still.
   *  Only a credit CLOSING the title: one a bracket follows ("On Screen: Fallen Angels by Noel Coward (15)") leaves the
   *  title whole, as the English reader did (UK Lowther Pavilion lost its take of the capture otherwise, run 1057).
   *  Allocates nothing on a title holding no authorship word. */
  private[identity] def creditedWork(title: String): Option[String] =
    Credits.authorCredited(title).filter(credit => title.trim.endsWith(credit.person)).map(_.work)
      .filter(work => work.exists(_.isLetter) && !creditsAnAccompanist(work))
  /** Does the work end on an accompaniment's word ("Klavierbegleitung", "live music", "akompaniament"): the name after
   *  it plays the score. */
  private def creditsAnAccompanist(work: String): Boolean = {
    val last = work.substring(work.lastIndexWhere(c => !c.isLetter) + 1).toLowerCase(java.util.Locale.ROOT)
    AccompanimentWords.exists(last.endsWith)
  }
  private val AccompanimentWords = Seq("begleitung", "musik", "music", "accompaniment", "akompaniament", "akompaniamentem",
    "score", "vertonung")

  /** A spaced slash joining two titles of one standing ("Barwy ochronne / Camouflage"): neither piece is a banner. */
  private val SlashJoin = """\s+/{1,2}\s+""".r
  /** Does a slash ([[SlashJoin]]) join a piece naming a work ([[MultiFilmBill.namesAWork]]: no format, no event) that
   *  holds none of `f`'s titles? Then the title bills two works, and the piece naming `f` is no banner's rest: DE
   *  Filmhaus Kino Köln's "SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE" bills two shorts, and took the second. "Barwy
   *  ochronne / Camouflage" joins the film's English title, "Warsztat Świętego Mikołaja / dubbing - Nasze Kino" its
   *  format and its venue. */
  def joinsAnotherWork(l: MeasuredListing, f: Film): Boolean = l.ownTitles.exists { title =>
    val parts = withoutPeeledBanner(l, SlashJoin.split(title).toSeq.map(_.trim).filter(_.nonEmpty))
    lazy val filmTitles = f.titles.map(TitleContainment.tokens).filter(_.nonEmpty)
    parts.sizeIs >= 2 && parts.exists(part => MultiFilmBill.namesAWork(part) && !filmTitles.exists(TitleContainment.tokens(part).containsSlice(_)))
  }

  /** `parts` without the banner the title rules peel off their head: a piece no search title holds, before one a search
   *  title does — Kino Sokolnia Kępno's "„WAJDA: re-wizje” 2026 / "BEZ ZNIECZULENIA" (1985)", searched as the film after
   *  the slash. "SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE" is searched by its first piece, and bills two works. */
  private def withoutPeeledBanner(l: MeasuredListing, parts: Seq[String]): Seq[String] = {
    val searched = l.searchTitles.map(TitleContainment.tokens).filter(_.nonEmpty)
    def held(part: String) = { val p = TitleContainment.tokens(part); p.nonEmpty && searched.exists(s => s.containsSlice(p) || p.containsSlice(s)) }
    val first = parts.indexWhere(held)
    if (first > 0) parts.drop(first) else parts
  }

  /** May the piece of `l`'s title that names `f` stand for the whole, the rest a banner? Two words at least —
   *  a one-word piece is many films' title ("Bhutan – Trails of Happiness" is not the 1928 "Bhutan") — and no
   *  Roman numeral in the rest: a numbered set ("Bolek i Lolek – zestaw IV") is an instalment, not the film;
   *  and a year the rest states agrees with the record's. */
  def standsForTheWhole(l: MeasuredListing, f: Film): Boolean = pieceStandsForTheWhole(l, f, words = 2)

  /** [[standsForTheWhole]] for a piece of `words` words at least: one, where a fact of the listing's own (the director
   *  it credits) tells the word's many films apart ([[AcceptanceRuleTable.DirectorsTitle]]). */
  def pieceStandsForTheWhole(l: MeasuredListing, f: Film, words: Int): Boolean = {
    val pieces = namingPieces(l, f).filter(_.sizeIs >= words)
    pieces.nonEmpty && {
      val rest  = l.ownForms.flatMap(_.words).toSet -- pieces.flatten
      // a year the rest states is the record's: "Disney Junior Cinema Club 2026" is not the 2024 edition
      val years = rest.filter(TitleYear.matches).map(_.toInt)
      !rest.exists(numbersAnInstalment) &&
        (years.isEmpty || f.year.forall(y => years.exists(FactRelations.yearsNear(_, y))))
    }
  }

  /** The title relations under which another film RIVALS a listing's film: the listing's title
   *  names it as closely (`rivals`). */
  val Rivalling: Set[String] = Set("exact", "original", "alternative")
  /** An IMDb id's title number ("tt0064570" → 64570), 0 for anything else. */
  def imdbNumber(imdbId: String): Int =
    Option.when(imdbId.startsWith("tt"))(imdbId.drop(2)).flatMap(_.toIntOption).filter(_ > 0).getOrElse(0)
  /** The IMDb id of a title number (64570 → "tt0064570"); none for 0, no number. */
  def imdbId(imdbNumber: Int): Option[String] = Option.when(imdbNumber > 0)(f"tt$imdbNumber%07d")

  /** The relations under which a film carries the listing's title as one of its own: whole, or as a delimited piece. */
  val TitlesItsOwn: Set[String] = Rivalling + "segment"
  /** The ONE film the listing's search titles name on IMDb ([[CandidateQuery.ImdbTitled]]), with the titles that name
   *  it: a title IMDb lists under several films names none of them ("Santiago", the original Helios publishes beside
   *  "Camino dla opornych", is a dozen films), and the films the rest name must be one. `titled`: each film with the
   *  titles IMDb lists it under. */
  def soleImdbTitled(titled: Map[Int, Set[String]]): Option[(Int, Set[String])] = {
    val sole = titled.toSeq.flatMap { case (film, titles) => titles.filter(_.exists(_.isLetter)).map(_ -> film) }
      .groupMap(_._1)(_._2).collect { case (title, Seq(film)) => film -> title }.toSeq.groupMap(_._1)(_._2)
    Option.when(sole.sizeIs == 1)(sole.head).map { case (film, titles) => film -> titles.toSet }
  }

  /** May the listing take a title IMDb lists `f` under (an AKA TMDB does not carry) as `f`'s own: no year the title
   *  writes, or the venue states, against the film's ("Miłość 2024" is not Haneke's 2012 film), no director it credits
   *  against the film's, no numbered set ("Bolek i Lolek – zestaw
   *  IV"), and no stage work broadcast as a non-season film (the Met's "Così fan tutte" is not Tinto Brass's). Which
   *  films stand beside it is the caller's to read: the ONE IMDb lists so, and none carrying the title already. */
  def takesImdbTitle(l: MeasuredListing, f: Film): Boolean = {
    def near(year: Int) = f.year.forall(FactRelations.yearsNear(_, year))
    // nor a year or director the venue publishes against it: "Afrykanska Przygoda 3D IMAX" [2007] {Ben Stassen} is not
    // the 1954 film IMDb also calls "Afrykańska przygoda"
    yearsWritten(l).forall(near) && l.statedYear.forall(near) &&
      !f.directorCredits.exists(credits => CreditRelation.of(l.directorCredits, credits) == Category("different")) &&
      !numbersASet(l) && (StageRelays.stageWorks(l).isEmpty || f.season.isDefined)
  }

  /** The years the listing's title writes anywhere, bracketed or bare ("Miłość 2024"). */
  def yearsWritten(l: MeasuredListing): Set[Int] = l.ownForms.flatMap(_.words).filter(TitleYear.matches).map(_.toInt).toSet
  /** Does the listing's title number a set with a Roman numeral ("Bolek i Lolek – zestaw IV")? */
  def numbersASet(l: MeasuredListing): Boolean =
    l.ownForms.flatMap(_.words).exists(numbersAnInstalment)

  /** How many films of `pool` other than `film` the listing's title names as closely as a
   *  rival does — the `rivals` measure, over whatever pool the caller searched. */
  def rivals(l: MeasuredListing, pool: Map[Int, Film], film: Int): Int =
    pool.count { case (id, f) => id != film && Rivalling(titleRelation(l, f).value) }

  /** The films a listing's title names EXACTLY that one of its own title searches returned FIRST,
   *  from `pool` (each candidate with its best 1-based rank over the listing's searches, `None` when
   *  none returned it). A banner segment's first hit is not one: the listing's whole title must be
   *  the film's. The resolver's top-hit acceptance and the evidence class it is measured as
   *  (`scripts.IdentityEvidenceClasses`) read this one definition. */
  def exactTopHits(l: MeasuredListing, pool: Seq[(Int, Film, Option[Int])]): Seq[Int] =
    pool.collect { case (id, f, Some(1)) if titleRelation(l, f) == Category("exact") => id }.distinct.sorted

  /** The title groups (by [[key]]) whose venues' listings corroborate `l`: its own title's, and the
   *  title a learned venue decoration wraps (`TitleDecorations`) — the other venues list "Mistyczka
   *  2D PL" as "Mistyczka". */
  def titleGroups(l: MeasuredListing): Seq[String] =
    (key(l.title) +: l.decoratedOnlyShapes.map(key)).filter(_.nonEmpty).distinct

  /** The title groups (by [[key]]) of the titles `l`'s search asks for — banners and screening notes off
   *  (`searchTitles`) — that are `f`'s own title, original or alternative: "Kino Konesera: Róża" is the
   *  "Róża" other venues list with Schleinzer and 2026 (as the old pipeline folded rows by their search
   *  key), but "ANDRÉ RIEU - NIECH ŻYJE MAASTRICHT!", searched as "André Rieu", is no concert's title. */
  def searchGroups(l: MeasuredListing, f: Film): Seq[String] = {
    val titles = f.titleKeys
    l.searchKeys.filter(titles).distinct
  }
  /** Every group [[searchGroups]] can name for `l`, whatever the film — what a family's resolve may read. */
  def searchGroupsAny(l: MeasuredListing): Seq[String] = l.searchTitles.map(key).filter(_.nonEmpty).distinct
}
