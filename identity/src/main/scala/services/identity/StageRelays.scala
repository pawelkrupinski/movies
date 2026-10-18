package services.identity

import services.identity.IdentityMeasures.*
import services.movies.{TitleContainment, TitleSeason}
import services.resolution.SearchTitles

/**
 * How a STAGE HOUSE's relay is billed — a season production, a house's banner on a work, the stage works a title names
 * in whatever language — read for the identity measures ([[IdentityMeasures]]): seasons and works, banners and the
 * houses they are learned to be ([[StageRelays.Houses]]), relays ([[StageRelays.Relay]]) and the searches a relay asks.
 * Measures only: what any of it is worth is the calibration's.
 */
object StageRelays {

  /** Are two records ONE BROADCAST its film database holds twice: one house's ([[namesOneHouse]]) record of one stage
   *  work ([[StageWorks]]), aired the same day? TMDB holds the Royal Opera's 10 November 2026 "Carmen" as 1151422 "Royal
   *  Opera House: Carmen" and as 1702759 "Royal Ballet & Opera 2026/27: Carmen". */
  def sameBroadcast(a: Film, b: Film): Boolean =
    a.released.isDefined && a.released == b.released && a.stageWorkIds.exists(b.stageWorkIds) &&
      a.houseNames.exists(x => b.houseNames.exists(namesOneHouse(x, _)))
  /** Which of one broadcast's records ([[sameBroadcast]]) every listing of it takes, first in this order: the one dating
   *  its season (the series the house files its season under), then one IMDb also holds, then the lower id — so all of
   *  a broadcast's listings land on one card, whichever record each one's evidence reaches first. */
  def broadcastRecordOrder(id: Int, f: Film): (Int, Int, Int) = (if (f.season.isDefined) 0 else 1, if (f.imdbNumber > 0) 0 else 1, id)

  /** Does the film's own title name the listing's SEASON PRODUCTION: both name the same season,
   *  and they share a whole title segment outside it — the work. "Met Opera 2026-27: Samson et
   *  Dalila" and the film database's "The Metropolitan Opera 2026/27: Samson et Dalila" are one
   *  season's production of one work, however each spells the house. Segments are the listing's
   *  own delimiters (`SearchTitles.candidates`) on both sides; a segment carrying the season is
   *  the banner, never the work. */
  def namesSeasonProduction(l: Listing, f: Film): Boolean = {
    val shared = seasonWorks(l) intersect seasonWorks(f)
    shared.exists { case (work, _) => !work.startsWith("work:") } || (shared.nonEmpty && bannersMeet(l.ownTitles, f.titles))
  }

  /** A season production met only through a work named in two languages ([[StageWorks]]) is the record's only when
   *  the two titles' banners — the pieces naming no work — share a word: PL "Balet z Opery Paryskiej 2026-2027:
   *  Jezioro łabędzie" ×16 is the Paris Opera Ballet's Swan Lake, which TMDB has no record of, not "Royal Ballet & Opera
   *  2026/27: Swan Lake"; "Royal Ballet and Opera Sezon Kinowy 2026-27: Dziadek do orzechów" shares "royal", "ballet",
   *  "opera" with its record. Words of four letters or more, so "the" or "and" meets nothing. A banner naming a house in
   *  particular meets only by such a word — never by the art form ([[HouseCommonWords]]) every house's name carries: DE
   *  "Opera de Paris: Romeo et Juliette (2026/27)" ×7 took the Royal Ballet's record by "opera". One naming none ("OPERA
   *  2026/2027 - MAKBET") meets by the art form. */
  private def bannersMeet(listingTitles: Seq[String], filmTitles: Seq[String]): Boolean = {
    def bannerWords(titles: Seq[String]) = titles.flatMap(pieces)
      .filter(piece => StageWorks.resolver.named(key(piece)).isEmpty)
      .flatMap(TitleContainment.tokens).filter(word => word.length >= 4 && !word.forall(_.isDigit)).toSet
    val listing    = bannerWords(listingTitles)
    val particular = listing.filterNot(HouseCommonWords)
    ((if (particular.nonEmpty) particular else listing) intersect bannerWords(filmTitles)).nonEmpty
  }

  /** The (work, season) pairs a listing's title names: each whole segment outside its season, keyed,
   *  beside the season. [[namesSeasonProduction]] is exactly two sides' pairs meeting, so a record
   *  of the listing's season production can be found by pair, whoever searched it up. */
  def seasonWorks(l: Listing): Set[(String, Int)] =
    l.seasonYear.fold(Set.empty[(String, Int)])(season => seasonlessWorks(l.shapes).map(_ -> season))

  /** A film's (work, season) pairs, as [[seasonWorks]] reads a listing's. */
  def seasonWorks(f: Film): Set[(String, Int)] =
    f.season.fold(Set.empty[(String, Int)])(season => seasonlessWorks(f.titles.flatMap(SearchTitles.candidates(_, None))).map(_ -> season))

  /** Each segment's key — and each stage work it names, in whatever language ([[StageWorks]]): PL's "Royal Ballet and
   *  Opera Sezon Kinowy 2026-27: Dziadek do orzechów" is the season's "Royal Ballet & Opera 2026/27: The Nutcracker". */
  private val SentenceStop = java.util.regex.Pattern.compile("""\.\s+""")
  private def seasonlessWorks(titles: Seq[String]): Set[String] = {
    val seasonless = titles.filter(t => TitleSeason.of(Seq(t)).isEmpty)
    val keys = seasonless.map(key).filter(_.nonEmpty).toSet
    // a work's name may run on into a translated subtitle after a full stop: "Cosi fan tutte. Tak czynią wszystkie"
    val named = keys ++ seasonless.map(t => key(SentenceStop.split(t, 2).head)).filter(_.nonEmpty)
    keys ++ named.flatMap(StageWorks.resolver.named).map(work => s"work:$work")
  }

  /** A title read as a HOUSE BILLING A WORK: the work both titles carry as a whole delimited
   *  piece (a title shape of each), and what each title adds to it along one edge — its banner,
   *  "how the title spells its house" ("NT Live: Dr. Strangelove" and "National Theatre Live:
   *  Dr. Strangelove" bill `drstrangelove` as `ntlive` and `nationaltheatrelive`). Seasons and
   *  bracketed years are no part of a house's name, on either side. Keys, by [[key]]. */
  final case class Billing(listingWords: Seq[String], filmWords: Seq[String], work: String) {
    def listingHouse: String = listingWords.mkString
    def filmHouse: String    = filmWords.mkString
  }

  /** How the listing and the film bill one work (the longest they share), when both titles add a
   *  banner to it. `None`: no shared work, or one title is the work alone. */
  def billing(l: Listing, f: Film): Option[Billing] = billings(l, f).headOption

  /** EVERY way the listing and the film bill the longest work they share under banners, in
   *  [[billing]]'s order: a record carrying its broadcast title beside an alternative streaming
   *  title ("National Theatre at Home: …") bills the work under both. */
  def billings(listing: Listing, film: Film): Seq[Billing] = {
    val works = (listing.billedWorks intersect film.billedWorks).toSeq.sortBy(work => (-work.length, work.mkString(" ")))
    works.iterator.map(work => billedUnder(listing, work, film, work, work.mkString)).find(_.nonEmpty).getOrElse(Nil)
  }

  /** The banners a title puts on a work it carries — its words before or after the work. */
  private def bannerOf(title: Seq[String], work: Seq[String]): Option[Seq[String]] =
    Option.when(title.lengthIs > work.length)(
      if (title.endsWith(work)) Some(title.dropRight(work.length)) else if (title.startsWith(work)) Some(title.drop(work.length)) else None
    ).flatten

  /** Every way the listing bills `listingWork` and the film `filmWork` under banners, as the one work `work`. */
  private def billedUnder(listing: Listing, listingWork: Seq[String], film: Film, filmWork: Seq[String], work: String): Seq[Billing] =
    (for {
      listingHouse <- listing.billedTitles.flatMap(bannerOf(_, listingWork))
      filmHouse    <- film.billedTitles.flatMap(bannerOf(_, filmWork))
    } yield Billing(listingHouse, filmHouse, work))
      .sortBy(billed => (billed.listingHouse, billed.filmHouse, billed.listingWords.mkString(" "), billed.filmWords.mkString(" ")))

  /** How the listing and the film bill one STAGE WORK under banners, each in its own language ([[StageWorks]]), when no
   *  work of theirs is the same words: PL "Makbet | metropolitan opera: live in hd 2026/27" and the Met's "The
   *  Metropolitan Opera 2026/27: Macbeth". What a season's banner is learned from ([[Houses.evidence]]): billed by its
   *  literal works alone, Kino 1410's Met banner met only Royal Ballet & Opera's records of "Carmen" and "Così fan
   *  tutte", and was learned as RBO's — whose season productions it then took. */
  def stageBilling(listing: Listing, film: Film): Option[Billing] = {
    def staged(works: Set[Seq[String]]) = works.toSeq.flatMap(work => StageWorks.resolver.named(key(work.mkString(" "))).map(_ -> work))
    val filmWorks = staged(film.billedWorks).groupMap(_._1)(_._2)
    staged(listing.billedWorks).sortBy { case (id, work) => (-work.length, id, work.mkString(" ")) }.iterator
      .map { case (id, listingWork) => filmWorks.getOrElse(id, Nil).sortBy(_.mkString(" ")).flatMap(billedUnder(listing, listingWork, film, _, s"work:$id")) }
      .find(_.nonEmpty).flatMap(_.headOption)
  }

  /** Words naming an opera or ballet HOUSE, or its relay, in the languages venues bill one in: a title carrying one bills a
   *  STAGING ("Paris Opera Ballett: …", "RB&O Live: Tosca (Oper)", "Balet z Opery Paryskiej …", "RBO …", "ROH …"). The
   *  word as its own token: "Dreigroschenoper" or "Operation" is no house. */
  private def StageHouseWords: Set[String] = services.movies.StageHouse.Words

  /** Is `f` a SCREEN NAMESAKE of the work a stage house's relay bills: the listing's title names an opera or ballet house
   *  beside the work ([[StageHouseWords]]) and none of the film's own titles bills one? A house's banner names its
   *  STAGING: DE "Paris Opera Ballett: Romeo und Julia" is the ballet's relay, not Zeffirelli's 1968 film of the play,
   *  however TMDB ranks it, and "RB&O Live: Tosca (Oper)" not the 1976 film. A staging's record bills its house ("RBO
   *  2026/27: …", "The Metropolitan Opera: …", "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE"); a bare one is no house's
   *  that the listing can tell. */
  def screenNamesake(listing: Listing, film: Film): Boolean =
    listing.billsAStageHouse && !film.ownTitleTokens.exists(_.exists(StageHouseWords))
  /** Does one of `titles` name an opera or ballet house ([[StageHouseWords]])? A house's initials only standing alone, or
   *  split by punctuation ([[splitHouseInitials]]: UK Penrith's "RB&O 26/27: Carmen", DE "Carmen (RB+O 2026/27)"): DE
   *  "Jin-Roh - Die Wolfsbrigade" is Okiura's anime, no Royal Opera House relay, whatever its hyphen splits off. */
  private[identity] def billsAStageHouse(titles: Seq[String]): Boolean = services.movies.StageHouse.billed(titles)
  /** Does a title fragment name a house: a house word, or a house's split initials? */
  private def namesAHouse(fragment: String): Boolean = {
    val words = TitleContainment.tokens(fragment)
    words.exists(StageHouseWords) || services.movies.StageHouse.splitInitials(words)
  }

  /** What a stage house's relay title bills, read off its FRAGMENTS — its delimited pieces ([[pieces]]), each bracket
   *  apart from what it follows, digits out: a fragment naming a house ([[StageHouseWords]]) is a HOUSE, every other one
   *  may be the WORK ("RBO Live 2026/27: Schwanensee (Royal Ballet)" bills "Schwanensee" under "RBO Live" and "Royal
   *  Ballet"; "Live aus der MET: Verdi: Macbeth" bills "Live aus der MET", "Verdi" or "Macbeth"). `workWords`: the work
   *  fragments' words of four letters or more, beside the house's common ones ([[HouseCommonWords]]) — a venue adds a
   *  composer ("Giuseppe Verdi MACBETH"), a tag ("Macbeth ENCORE") or a translation ("Cosi fan tutte. Tak czynią
   *  wszystkie") to the work's name; `workIds` the stage works the fragments name in any language ([[StageWorks]]);
   *  `fragmentKeys` every fragment's words run together, a leading article off ([[wholeKey]]), a house's or not ("Das
   *  Phantom der Oper") — never one of the art form alone ([[namesOnlyTheArt]]); `houses` the house fragments' words ([[houseWords]]). */
  final case class Relay(workWords: Set[String], workIds: Set[String], fragmentKeys: Set[String], houses: Seq[Seq[String]])
  object Relay {
    val Unbilled: Relay = Relay(Set.empty, Set.empty, Set.empty, Nil)
    def of(titles: Seq[String]): Relay = {
      val all             = titles.flatMap(fragments).distinct
      val (houses, works) = all.partition(namesAHouse)
      Relay(works.flatMap(wordsOf).filter(word => word.length >= 4 && !HouseCommonWords(word)).toSet,
        works.flatMap(billedIn(_, seasonNamed = true)).toSet, all.filterNot(namesOnlyTheArt).map(wholeKey).filter(_.nonEmpty).toSet,
        houses.map(houseWords).filter(_.nonEmpty).distinct)
    }
  }
  /** Is `fragment` only the art form, a relay's word or a year ("Opera 2026", "Live")? It names no work and no house, so
   *  no record carries it whole: UK "Macbeth - Opera 2026" took Argento's 1987 "Opera" so. */
  private def namesOnlyTheArt(fragment: String): Boolean = wordsOf(fragment).forall(HouseCommonWords)
  private val BracketedFragment = """[(\[]([^()\[\]]*)[)\]]""".r
  /** A title's fragments: each delimited piece, and each bracket in it apart from what it follows. */
  private def fragments(title: String): Seq[String] = pieces(title).flatMap { piece =>
    BracketedFragment.findAllMatchIn(piece).map(_.group(1)).toSeq :+ BracketedFragment.replaceAllIn(piece, " ")
  }.map(_.trim).filter(_.exists(_.isLetter))
  /** A fragment's words, numbers out: a season ("26/27") or a screening's year ("Macbeth 2026") dates the relay, never names it. */
  private def wordsOf(fragment: String): Seq[String] = TitleContainment.tokens(fragment).filterNot(_.forall(_.isDigit))
  /** A house fragment's words, a stage work's name it opens or ends with out ([[billedIn]]'s runs): "OPERA-MANON" and
   *  "OPERA- SILENT NIGHT" bill the work in the house's word, and name no house "Manon" that "BALLET LIVE. MANON. ROYAL
   *  ÓPERA HOUSE" would share. */
  private def houseWords(fragment: String): Seq[String] = {
    val words = wordsOf(fragment).toIndexedSeq
    val runs  = words.indices.map(_ -> words.length) ++ (1 until words.length).map(0 -> _)
    val work  = runs.filter { case (from, until) => StageWorks.resolver.named(words.slice(from, until).mkString).nonEmpty }
      .flatMap { case (from, until) => from until until }.toSet
    words.indices.filterNot(work).map(words)
  }
  /** A fragment or a whole title as one key: its words ([[wordsOf]]) run together, a leading article off — "Das Phantom
   *  der Oper" is TMDB's 1943 "Phantom der Oper" whole. */
  private[identity] def wholeKey(title: String): String = withoutLeadingArticle(wordsOf(title)).mkString

  /** Does the film's record bill ANOTHER WORK than the listing's stage-house relay: no title of the record carries a word
   *  of the relay's work fragments ([[Relay]]) or is one of its fragments whole, nor names a stage work they name in
   *  another language? DE "RBO Live 2026/27: Schwanensee (Royal Ballet)" ×80 took the house's "RBO 2026/27: Tosca (The
   *  Royal Opera)" by pooled evidence, and Cinefactory's "MET Opera: Macbeth 2026" — its credits, running time and original title copied from last
   *  season's Onegin — "MET Opera: Eugen Onegin" by those facts. A house bills many works; the title says which. A
   *  title whose every fragment names its house says no work, and denies none. */
  def namesAnotherWork(listing: Listing, film: Film): Boolean = listing.billsAStageHouse && {
    val relay = listing.relay
    (relay.workWords.nonEmpty || relay.workIds.nonEmpty) && !relay.workWords.exists(film.titleWords) &&
      !relay.workIds.exists(film.stageWorkIds) && !relay.fragmentKeys.exists(film.wholeKeys)
  }
  /** [[namesAnotherWork]], the relay's work a STAGE WORK the record names in none of its languages: what a venue's title
   *  is never learned as the record's own by ([[venueTitles]], [[titlesByFacts]]) — a misspelt work ("RBO Live 2026/27:
   *  Gtterdmmerung (Royal Opera)") names no stage work, and is still its record's spelling. */
  private[identity] def namesAnotherStageWork(listing: Listing, film: Film): Boolean = listing.relay.workIds.nonEmpty && namesAnotherWork(listing, film)

  /** Does one of the film's titles name the house the listing's relay bills ([[Relay.houses]], [[namesOneHouse]]) — by
   *  a word of its banner other than `besides`, the words a rival's titles share with it? PL "OPERA- SILENT NIGHT" names
   *  no house by "silent" against the Met's "Silent Night", whose own title carries the word. */
  def billsItsHouse(listing: Listing, film: Film, besides: Set[String] = Set.empty): Boolean =
    listing.relay.houses.map(_.filterNot(besides)).exists(house => film.houseNames.exists(namesOneHouse(house, _)))
  /** Do the film's titles name a house, and none the listing's relay bills: UK's "English National Ballet: The Sleeping
   *  Beauty" and TMDB's 2017 "The Sleeping Beauty", also "The Sleeping Beauty (The Royal Ballet)"? */
  def billsAnotherHouse(listing: Listing, film: Film): Boolean = film.houseNames.nonEmpty && !billsItsHouse(listing, film)
  /** Is `f` ANOTHER PRODUCTION than the stage house's relay the listing screens — a record of its work from another season
   *  than the one its screenings fall in ([[Listing.screenedSeason]]): another house's, or any house's season record? Nothing but the work's name then
   *  ties the record to the listing.
   *  - A banner naming a house in particular ([[Relay.houses]]) is not the record's: UK Savoy's "Opéra De Paris Ballet -
   *    Notre-Dame de Paris" ×6, screening in March 2027, is no 2013 "La Scala Ballet: Notre-Dame de Paris". Another
   *    house's record of the screenings' own season stays a candidate: UK's "RBO Cinema Season" relays the Met's
   *    productions of that season.
   *  - A banner naming no house in particular ("Ballet - Notre-Dame de Paris") may be the record's house, so only a record
   *    from long before or after ([[datedAway]]), on a listing dating it by no year of its own and billing the stage work
   *    apart from its banner ([[Relay.workIds]]), is another production: UK Film Theatre Leiston's, screening in March
   *    2027, is not La Scala's 2013 record; DE Kleines Theater's "RB&O - Ballett "La Fille mal gardée"" in 2026 is the
   *    house's 2025/26 encore; and UK Chichester's "Phantom of the Opera" bills no banner at all, "opera" being the work's
   *    own word.
   *  Any house's SEASON record ([[anotherSeasonsRelay]]) two seasons or more from the screenings' is another production too.
   *  Never otherwise the listing's own house's record of another year: a house's recordings are encored years after it
   *  staged them (UK "NT Live: A Streetcar Named Desire" is the 2014 broadcast, "Royal Ballet: La Bayadère" the 2018 recording), and a
   *  title's banner shared by a film series reads as a house (DE "Der Herr der Ringe - Die Gefährten (Extended Version)").
   *  KNOWN GAP: so US "NT Live: Hamlet", screening November 2026 — the National Theatre's 2026 Hamlet, which no search of
   *  it reaches — keeps the 2015 broadcast the shared Flicks page dates it by. */
  def anotherProduction(listing: Listing, film: Film): Boolean =
    listing.billsAStageHouse && listing.screenedSeason.exists(season =>
      anotherSeasonsRelay(film, season) || billsAnotherHouse(listing, film) && (
        if (listing.relay.houses.exists(house => particularWords(services.movies.StageHouse.initialsJoined(house)).nonEmpty))
          services.movies.ListingConstraints.seasonsApart(Some(season), film.season, film.year).isDefined
        else listing.year.isEmpty && listing.relay.workIds.nonEmpty && datedAway(film, season)))
  /** Is the record a SEASON's relay ([[Film.season]]: its title names the season, "Royal Ballet & Opera 2024/25: …") from
   *  two seasons or more before or after `season`, whatever house it bills? A season's record is the broadcast of that
   *  season's staging, encored the next season at most: a house restaging a work relays it anew, under the new season's
   *  record — UK Cineworld's "Royal Ballet and Opera: Alice’s Adventures in Wonderland" ×126, screening on 23 March 2027,
   *  the day TMDB's 2026/27 record airs, is not the house's 2024/25 relay. A record dated by a year alone ("Opéra national
   *  de Paris 2024: Schwanensee") names no season, and stays [[datedAway]]'s. */
  private def anotherSeasonsRelay(film: Film, season: Int): Boolean =
    film.season.exists(recorded => math.abs(recorded - season) > SeasonEncoreLag)
  /** How many seasons after its own a season's record is still encored: the next one. */
  private val SeasonEncoreLag = 1
  /** Is a record naming no season dated by its year more than [[RecordingLag]] years from `season`? A season's record is
   *  [[anotherSeasonsRelay]]'s to judge. */
  private def datedAway(film: Film, season: Int): Boolean =
    film.season.isEmpty && film.year.exists(dated => math.abs(dated - season) > RecordingLag)
  /** How many years a house's record may stand from the season a venue screens it in and still be the relay it airs: an
   *  encore of last season's, or a recording a house releases to cinemas seasons after it staged it — DE Cinema
   *  Wolfhagen's "National Opera Paris: Schwanensee", screening December 2026, is the Paris Opera's "Opéra national de
   *  Paris 2024: Schwanensee", which its venues bill so. */
  private val RecordingLag = 2

  /** A relay's own words, which a house's name is billed WITH but never abbreviated by ("NT Live" of "National Theatre
   *  Live" is "NT"). */
  private val RelayWords = Set("live", "hd", "kino", "cinema", "im", "in", "aus", "at", "from", "presents", "season", "saison", "sezon", "kinowy")
  /** Words a house's name shares with any other's — the art form in each language, the kind of institution ("National",
   *  "Staatsoper", "Opera House", "Theatre", "Company": English National Ballet is not Dutch National Ballet, nor the Wiener
   *  Staatsoper the Bayerische), articles, prepositions and a relay's own words ("Live", "im Kino", "Season") — which say
   *  nothing about WHICH house. */
  private val HouseCommonWords: Set[String] = RelayWords ++ Set("opera", "operas", "oper", "opern", "opery", "operze", "ballet", "ballets",
    "ballett", "balet", "baletu", "balletto", "national", "nationale", "nacional", "nazionale", "narodowa", "narodowy", "staatsoper", "state",
    "house", "company", "theatre", "theater", "teatro", "teatr", "the", "and", "und", "of", "de", "du", "des", "di", "del", "der", "die",
    "das", "la", "le", "les", "el", "los", "las", "z", "w", "na")
  /** A house name's words that say WHICH house: two letters or more, no number, none of [[HouseCommonWords]]. */
  private def particularWords(name: Seq[String]): Seq[String] =
    name.filter(word => word.length >= 2 && !word.forall(_.isDigit) && !HouseCommonWords(word))
  /** Do two house names NAME one house: a word in common beyond [[HouseCommonWords]], one's word a prefix of the other's
   *  ("Met", "Metropolitan"), or one's word the other's initials ("RBO" of "Royal Ballet & Opera", "ROH" of "Royal Opera
   *  House"), or two names of one house ([[services.movies.StageHouse]]: "ROH" and "Royal Ballet & Opera", the house renamed)? Never a
   *  name of common words alone ("Opera", "The Ballet"): it names no house in particular. Numbers are no part of a name
   *  ("RBO 26/27"). */
  private[identity] def namesOneHouse(a: Seq[String], b: Seq[String]): Boolean = {
    def own(name: Seq[String]) = particularWords(services.movies.StageHouse.initialsJoined(name))
    def initials(name: Seq[String]) = services.movies.StageHouse.initialsOf(name)
    val (ownA, ownB) = (own(a), own(b))
    ownA.exists(x => ownB.exists(y => x == y || (x.length >= 3 && y.startsWith(x)) || (y.length >= 3 && x.startsWith(y)))) ||
      ownA.exists(x => x.length >= 2 && x == initials(b)) || ownB.exists(y => y.length >= 2 && y == initials(a)) ||
      oneHouseRenamed(ownA.toSet, ownB.toSet)
  }
  /** Each multi-named house's names ([[services.movies.StageHouse]]) as the words that say which house ([[particularWords]]). */
  private lazy val housesByName: Seq[Set[Set[String]]] =
    services.movies.StageHouse.namedWithInitials.map(_.map(name => particularWords(name).toSet).filter(_.nonEmpty).toSet)
  /** Are `a` and `b` — particular words — exactly two names of one house: a banner spelling one of its names, nothing
   *  more ("Royal Shakespeare" spells none of the Royal Opera House's)? */
  private def oneHouseRenamed(a: Set[String], b: Set[String]): Boolean =
    a.nonEmpty && b.nonEmpty && housesByName.exists(names => names(a) && names(b))

  /** Is one house name the other's ABBREVIATION — one short word, its relay's words aside, the initials of the other's
   *  several ("NT Live" of "National Theatre Live", "RBO" of "Royal Ballet & Opera")? A name, whatever house its banner
   *  was learned as: UK "NT Live" was learned as itself, from one record filing "NT Live: …" among its alternatives, and
   *  Flicks' "NT Live: Hay Fever" ×131 read its own record as an overlap. Initials only, never a word two houses share:
   *  "Royal Shakespeare Company" is no "Royal Ballet & Opera", nor "National Theatre at Home" "NT Live". */
  private[identity] def abbreviates(a: Seq[String], b: Seq[String]): Boolean = {
    def words(name: Seq[String]) = name.filterNot(word => word.forall(_.isDigit) || RelayWords(word) || services.movies.StageHouse.UninitialledWords(word))
    def short(name: Seq[String]) = words(name) match {
      case Seq(one) if one.length >= 2 && one.length <= 4 => Some(one)
      case _                                             => None
    }
    def initials(name: Seq[String]) = Option(words(name)).filter(_.sizeIs >= 2).map(_.flatMap(_.headOption).mkString)
    short(a).exists(initials(b).contains) || short(b).exists(initials(a).contains)
  }

  /** Does the film's record bill the listing's work under the listing's OWN house — the banner the
   *  listing puts on the work, spelt as the record spells it or learned to be it (`Houses.same`)?
   *  "NT Live: The Misanthrope" and "National Theatre Live: The Misanthrope" do — by any of the
   *  record's titles, its "National Theatre at Home" alternative aside ([[billings]]); a record titled
   *  the work alone ("The Misanthrope") bills no house at all. A listing naming a season takes only a
   *  record naming none — one naming a season is `namesSeasonProduction`'s to read — whose year the
   *  season spans (`ListingConstraints.seasonsApart` denies the rest): TMDB's US title of the Met's
   *  2026 Così is "The Metropolitan Opera: Così fan tutte", which US "Met Opera 2026-27: Così fan
   *  tutte" ×356 is. A season's record is a season-free listing's only when the
   *  listing's banner spells the house ([[spellsItsHouse]]): TMDB filing the Paris Opera's works only
   *  under the Met's 2026/27 records teaches the Paris banner to be the Met. And not a banner numbering its edition otherwise than the record's: a
   *  house's name carries no number ("League of Legends Worlds 26" is not "… Worlds25"). Nor is a CUT or an edition a
   *  house ([[DecorationWords.EditionWords]]): DE "Das Boot - Director s Cut" is Petersen's film, not TMDB's yearless
   *  "Das Boot - Directors Cut" record its "house" bills alike. */
  def billsUnderItsHouse(listing: Listing, film: Film, houses: Houses): Boolean =
    (listing.seasonYear.isEmpty || film.season.isEmpty) &&
      billings(listing, film).exists(billed => houses.same(billed) && numbersIn(billed.listingHouse) == numbersIn(billed.filmHouse) &&
        !namesAnEdition(billed.listingWords) && !namesAnEdition(billed.filmWords) && (film.season.isEmpty || spellsItsHouse(billed)))
  /** A banner of edition words alone, a possessive's lone letter aside ("director s cut"). */
  private def namesAnEdition(banner: Seq[String]): Boolean = {
    val words = banner.filter(_.lengthIs > 1)
    words.nonEmpty && words.forall(DecorationWords.EditionWords)
  }
  /** Does the listing's banner SPELL the record's house — two of its words or more ("Royal Ballet and
   *  Opera" of "Royal Ballet & Opera")? A season-free listing takes a season's record only then: the
   *  Paris Opera's banner, learned as the Met from TMDB's filing, shares only "opera". */
  private def spellsItsHouse(billed: Billing): Boolean = (billed.listingWords.toSet intersect billed.filmWords.toSet).sizeIs >= 2
  private val Digits = "\\d+".r
  private def numbersIn(house: String): Set[String] = Digits.findAllIn(house).toSet

  /** Which house each listing banner is, LEARNED from how the film records of its works bill them
   *  (`learn`): no house, banner or abbreviation is known in advance. */
  final case class Houses(of: Map[String, String]) {
    /** Do the listing and the film bill the work under one house — the same spelling, or the house
     *  the listing's banner was learned to be? */
    def same(b: Billing): Boolean =
      b.listingHouse == b.filmHouse || of.get(b.listingHouse).contains(b.filmHouse) ||
        // an abbreviation, unless the banner was learned to be another house: "NT Live" learned as National Theatre Live
        // is no "New Theatre", however the initials meet
        (abbreviates(b.listingWords, b.filmWords) && of.get(b.listingHouse).forall(_ == b.listingHouse))
    /** Is the listing's banner known to be ANOTHER house than the film's — and the two names not one house's
     *  ([[namesOneHouse]])? DE's "RBO Live" banner was learned as the house records spelling it so bill, and then denied
     *  "RBO Live 2026/27: Schwanensee (Royal Ballet)" ×80 the season's record billing it "RBO 26/27": one house, two
     *  spellings. Nor is a banner of common words alone ([[particularWords]]) another house: it names none. A record
     *  learning a venue's "Götterdämmerung ROYAL OPERA HOUSE" bills "Götterdämmerung Royal Opera" under "House", and once
     *  "26/27" was a season that billing denied DE "RBO 26/27: Götterdämmerung (Royal Opera)" ×26 its own record. */
    def other(b: Billing): Boolean =
      !same(b) && of.contains(b.listingHouse) && particularWords(b.filmWords).nonEmpty && !namesOneHouse(b.listingWords, b.filmWords)
  }
  object Houses {
    val Unknown: Houses = Houses(Map.empty)

    /** A banner's house, among the houses whose records bill its works: the one whose name shares
     *  the most of the banner's words ("metropolitan opera: live in hd" is the Metropolitan Opera,
     *  though Royal Ballet & Opera's records bill more of its works), else — on a tie in words —
     *  the one billing the most of its DISTINCT works, at least two: "RBO Cinema Season" is Royal
     *  Ballet & Opera because its Swan Lake and Alice are, though TMDB files its Manon only under
     *  the Met; "NT Live" is National Theatre Live because every play it bills is. One work is a
     *  coincidence — a programme banner showing a film a house also staged — not a house, unless the
     *  banner's words name it. A tie on both is no house. */
    def learn(billings: Iterable[Billing]): Houses =
      Houses(ranking(billings).flatMap { case (banner, ranked) => chosen(ranked).map(banner -> _.house) })

    /** One house a banner's billings name, with how many of the banner's words its name shares
     *  (of the `named` words its name has) and how many of the banner's DISTINCT works it bills. */
    final case class Contender(house: String, spelt: Int, works: Int, named: Int = 1) {
      /** Does the banner NAME the house — two of its words, or a one-word house's one? One word of more
       *  ("maastricht" of "Love in Maastricht") is a coincidence of vocabulary, as one work is of billing. */
      def spells: Boolean = spelt >= 2 || (spelt >= 1 && spelt == named)
      def render: String = s"$house (words $spelt, works $works)"
    }

    /** Each banner's contending houses, best first — what [[learn]] chooses among. */
    def ranking(billings: Iterable[Billing]): Map[String, Seq[Contender]] =
      billings.toSeq.distinct.groupBy(_.listingHouse).map { case (banner, bannerBillings) =>
        val words = bannerBillings.flatMap(_.listingWords).toSet
        banner -> bannerBillings.groupBy(_.filmHouse).toSeq.map { case (house, houseBillings) =>
          val houseWords = houseBillings.flatMap(_.filmWords).toSet
          Contender(house, (houseWords intersect words).size, houseBillings.map(_.work).distinct.size, houseWords.size)
        }.sortBy(contender => (-contender.spelt, -contender.works, contender.house))
      }

    /** The house [[learn]] takes from a banner's ranked contenders, if any. */
    def chosen(ranked: Seq[Contender]): Option[Contender] = {
      val best = ranked.head
      val next = ranked.lift(1)
      val spelt  = best.spells && next.forall(_.spelt < best.spelt)
      val billed = best.works >= 2 && next.forall(runnerUp => runnerUp.spelt < best.spelt || runnerUp.works < best.works)
      Option.when(spelt || billed)(best)
    }

    /** What a listing's candidates say about its banner: how each record of its work bills it — of
     *  its season, when the listing names one (another season's record says nothing about which
     *  house this season's broadcast is). */
    def evidence(l: Listing, films: Iterable[Film]): Iterable[Billing] =
      films.filter(f => l.seasonYear.isEmpty || namesSeasonProduction(l, f))
        .flatMap(f => billing(l, f).orElse(Option.when(l.seasonYear.isDefined)(stageBilling(l, f)).flatten))
  }

  /** Do `titles` (a listing's, `rawTitle` the venue's own) bill a stage work — the work named whole, or within a piece:
   *  run into a house's word ("OPERA-COSI FAN TUTTE"), after its composer? */
  def billsStageWork(titles: Seq[String], rawTitle: String): Boolean =
    stageWorksBilled(titles, TitleSeason.of(titles).isDefined).nonEmpty ||
      titles.map(title => Listing(title, Some(rawTitle).filter(_ != title))).exists(stageWorks(_).nonEmpty)

  /** The stage works ([[StageWorks]]) the titles BILL: a piece naming one whole, or ending in one's name — after its
   *  composer (DE "Met Opera 2026/27: Camille Saint-Saëns SAMSON ET DALILA") or a house's word run on ("OPERA-MAKBET") —
   *  or, in a title naming its season (`seasonNamed`), opening with one before a venue's tag ("SAMSON I DALILA-
   *  RETRANSMISJA"). Never a work a title only opens with otherwise: "Manon des sources" is no broadcast of "Manon". */
  def stageWorksBilled(titles: Seq[String], seasonNamed: Boolean): Set[String] =
    titles.map(billable).flatMap(t => (pieces(t) ++ fragments(t)).distinct :+ t).flatMap(billedIn(_, seasonNamed)).toSet

  /** A box office's running time ending a title ("RB&O Romeo & Juliet (12 TBC) 170 mins"), and a bracket it never closes
   *  ("The Metropolitan Opera: Macbeth [New Production"): neither part of the work, both hiding the end it is read off. */
  private val TrailingRunningTime = """(?i)\s*\d{2,3}\s*mins?\.?\s*$""".r
  private val UnclosedBracket     = """\s*[(\[][^)\]]*$""".r
  /** Initials an ampersand joins ("RB&O"): one abbreviation, "RBO" — never "Romeo & Juliet", spaced. */
  private val AmpersandInitials   = """(?<=\b\p{Lu}{1,3})&(?=\p{Lu}{1,3}\b)""".r
  /** A title as its stage works and house are read off it ([[stageWorksBilled]], [[bannerNamesItsHouse]]): initials an
   *  ampersand joins run together, any other "&" read as "and", its running time and an unclosed bracket's tail cut off
   *  (the bracket's words kept as a piece of their own). */
  private def billable(title: String): String = {
    val untimed = TrailingRunningTime.replaceFirstIn(AmpersandInitials.replaceAllIn(title, "").replace("&", " and "), "")
    UnclosedBracket.findFirstMatchIn(untimed).fold(untimed)(m => s"${m.before} | ${m.matched.trim.drop(1)}")
  }

  /** The stage works one title piece bills ([[stageWorksBilled]]) — a broadcast's year shortened behind the work out:
   *  prod UK "RBO: Tosca 27", "RBO: Tosca '27", "The Met: Macbeth 26". Its words as a work's name is keyed, a possessive
   *  off ([[IdentityMeasures.withoutPossessives]]): "Alice’s Adventures in Wonderland" read as "alice", "s", … named no
   *  work, so UK Cineworld's "Royal Ballet and Opera: Alice’s Adventures in Wonderland" ×126 was no relay of one. */
  private[identity] def billedIn(piece: String, seasonNamed: Boolean): Set[String] = {
    val tokens = IdentityMeasures.words(piece).toIndexedSeq
      .reverse.dropWhile(token => token.length == 2 && token.forall(_.isDigit)).reverse
    val runs   = tokens.indices.map(tokens.drop) ++ (if (seasonNamed) (1 until tokens.size).map(tokens.take) else Nil)
    runs.flatMap(run => StageWorks.resolver.named(run.mkString)).toSet
  }

  /** The stage works ([[StageWorks]]) a listing's title pieces name, in whatever language. */
  def stageWorks(l: Listing): Set[String] = stageWorksNamed(l.ownTitles.flatMap(t => pieces(t) :+ t))
  /** The stage works a film's own titles name, as [[stageWorks]] reads a listing's. */
  def stageWorks(f: Film): Set[String] = stageWorksNamed(f.titles.flatMap(pieces))
  /** The stage works ([[StageWorks]]) the keys of `parts` name. */
  private def stageWorksNamed(parts: Seq[String]): Set[String] = parts.map(key).filter(_.nonEmpty).flatMap(StageWorks.resolver.named).toSet
  /** A stage broadcast a venue bills with no season but with its year — DE "Royal Ballet & Opera im Kino: Manon"
   *  [2026] — searched as its work's name and that year ("Manon 2026"): the house's season record carries both, while
   *  the work alone ranks it below every namesake film. A relay billing its house, a credit or a running time beside it,
   *  is searched with the season its screenings fall in too: UK Picture House Uckfield's "Tosca (ROH)" {Oliver Mears}
   *  [205′] and Flicks' "Royal Opera House: Carmen" [220′] ×13 screen the RBO 2026/27 relays, whose records the work's
   *  name alone never reaches — which season's record it is stays the season rules' to tell ([[Listing.screenedSeason]]).
   *  So is a bare work beside a running time ([[barelyTimed]]: UK Empire Theatre Halstead's "Romeo and Juliet" [170′]).
   *  Beside a year it states as well: Flicks dates "Royal Opera House: Carmen" 2024, the year of the page it links, and
   *  "Carmen 2024" finds the 2024 revival, never the 2026/27 broadcast its venues screen in November 2026.
   *  KNOWN GAP (convergence 38627416973, accepted.tsv): UK Lonsdale Alhambra Penrith's "RB&O 26/27: <work>" ×3 reached
   *  the season's record only by joining its other billings' cluster; once those took it alone, the learned
   *  listing-listing cut kept Penrith apart and it takes none. Alone it takes the record (IdentityResolverCasesSpec's
   *  short-season case): the follow-up is the corpus-level join. */
  private[identity] def stageWorkQueries(l: Listing): Seq[String] =
    if (l.seasonYear.isDefined) Nil
    // the year as stage-production reads it: its field, its title's bracket, its banner ("RBO live 2027: Tosca"), else its
    // broadcast season -- or nothing searched "Tosca 2027", and the house's 2026/27 record could stay out of the pool
    else (l.statedYear.orElse(bannerYear(l)).orElse(l.broadcastSeason).toSeq ++ l.screenedSeason.filter(_ => l.billsAStageHouse || barelyTimed(l))).distinct.flatMap(year =>
      l.billedStageWorks.toSeq.sorted.flatMap(StageWorks.resolver.searchNames).map(name => s"$name $year"))

  /** Is the listing's whole title a stage work, stating no year and no credit — a running time at most, which
   *  [[Listing.broadcastSeason]] reads as naming its staging? A running time names no staging by itself: UK Empire Theatre
   *  Halstead's bare "Romeo and Juliet" [170′], screening on 25 May 2027, is the RBO 2026/27 relay aired that day, which
   *  no search of the work's name alone reaches — the broadcast join ([[agreement.Broadcast]]) tells it from a film by
   *  its day. A credit does name one (Jacquot's "Tosca"), and a title going on past the work is a film's ("Lord Of The
   *  Rings: The Fellowship Of The Ring", the musical's name only its head). */
  private def barelyTimed(l: Listing): Boolean =
    l.statedYear.isEmpty && l.directors.isEmpty && l.ownTitles.exists(title => StageWorks.resolver.named(key(title)).nonEmpty)

  private val BannerYear = """(?<![\p{N}])(20\d{2})(?![\p{N}/–—-])""".r
  /** The one year a relay's banner dates its broadcast by — a piece of its title naming no stage work, beside one that
   *  does: DE "RBO live 2027: Tosca". A year beside the work is the work's own ("Macbeth 2026" is [[stageWorkQueries]]'). */
  def bannerYear(l: Listing): Option[Int] = {
    val (works, banners) = l.ownTitles.flatMap(t => PieceBreak.split(withoutSeasons(t)).toSeq)
      .partition(piece => StageWorks.resolver.named(key(BannerYear.replaceAllIn(piece, ""))).nonEmpty)
    Option.when(works.nonEmpty)(banners.flatMap(BannerYear.findAllMatchIn).map(_.group(1).toInt).distinct).collect { case Seq(one) => one }
  }

  /** Is `f` the house's record of the very season `l`'s screenings broadcast its work in ([[Listing.screenedSeason]]): a
   *  season record naming a stage work the listing bills, under the listing's house ([[bannerNamesItsHouse]])? A relay
   *  airs on its house's published dates, so a venue screening it then screens that season's broadcast. */
  def screenedSeasonRecord(l: Listing, f: Film): Boolean =
    l.screenedSeason.exists(f.season.contains) &&
      stageWorks(f).exists(l.billedStageWorks) && bannerNamesItsHouse(l, f) && airsOnItsDay(l, f)

  /** Does a listing whose year is more than a season before its screened season ([[Listing.airDays]]) screen on the
   *  record's air day — its release date, give or take [[AirDayWindow]] days? A nearer year passes with no test. */
  private def airsOnItsDay(l: Listing, f: Film): Boolean =
    l.airDays.isEmpty || f.released.exists(aired => l.airDays.exists(day => math.abs(java.time.temporal.ChronoUnit.DAYS.between(aired, day)) <= AirDayWindow))
  /** How many days from a relay's release date a screening still airs it: a venue's own date, a time zone apart. */
  val AirDayWindow = 3

  /** Does the listing's banner name the house the record bills? A banner naming a house in particular — words beside
   *  [[HouseCommonWords]], read off the title's fragments, its stage works out ([[houseWords]]) — must name the record's
   *  ([[namesOneHouse]]: a prefix, "MET" of "The Metropolitan Opera"; initials, "RBO" of "Royal Ballet & Opera"): DE
   *  "PARIS OPERA: ROMEO UND JULIA" is no Royal Ballet relay, nor UK "La Fanciulla Del West - Royal Opera" the Met's,
   *  however "opera" meets. A relay's banner naming no house ("OPERA-PARSIFAL") names the record by its one stage work
   *  alone — never by a name two works share: ES "Manon de Jules Massenet - Ópera" is no Royal Ballet "Manon". */
  def bannerNamesItsHouse(l: Listing, f: Film): Boolean = bannerNamesItsHouse(l)(f)
  /** [[bannerNamesItsHouse]] with the listing's side read once, for a rule weighing every candidate against it. */
  def bannerNamesItsHouse(l: Listing): Film => Boolean = {
    val particular = l.ownTitles.map(billable).flatMap(fragments).map(houseWords).map(particularWords).filter(_.nonEmpty)
    if (particular.nonEmpty) f => particular.exists(banner => f.houseNames.exists(namesOneHouse(banner, _)))
    else { val namesByItsWork = l.billsAStageHouse && l.billedStageWorks.sizeIs == 1; _ => namesByItsWork }
  }

  /** A season production searched as its WORK AND ITS SEASON ("Manon 2026"): a house's record of
   *  it ("Royal Ballet & Opera 2026/27: Manon") carries both, however the venue spells the house,
   *  while the work alone ranks it below every namesake. Only the pieces of a title naming the
   *  season ask it, without a bracketed year. */
  private[identity] def seasonProductionQueries(l: Listing): Seq[String] =
    l.seasonYear.toSeq.flatMap { season =>
      val seasonTitles = l.ownTitles.filter(t => TitleSeason.of(Seq(t)).isDefined)
      val works = shapes(seasonTitles).filter(t => TitleSeason.of(Seq(t)).isEmpty).map(withoutYears(_).trim).filter(_.nonEmpty).distinct
      // a work named in another language than the record's is asked for by its search name too ("The Nutcracker 2026")
      // — including one named before a translated subtitle ("Cosi fan tutte. Tak czynią wszystkie"), as the match reads it
      // — and by every other name of the work it is filed under, a house's own ("Samson et Dalila 2026"), the work billed
      // within the piece too ("Camille Saint-Saëns SAMSON ET DALILA")
      val translated = works.flatMap(work => (StageWorks.resolver.named(key(work)) ++ StageWorks.resolver.named(key(SentenceStop.split(work, 2).head)) ++
        stageWorksBilled(Seq(work), seasonNamed = true).toSeq).toSeq.sorted.distinct.flatMap(StageWorks.resolver.searchNames))
      (works ++ translated).distinct.map(work => s"$work $season")
    }
}
