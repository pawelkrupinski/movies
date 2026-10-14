package services.identity

/**
 * How far each SOURCE of a listing's facts may be trusted, fact by fact — in one place.
 *
 * A listing's year, directors, running time, original title, poster and synopsis are its venue's own statement, unless an
 * aggregator's showtimes feed linked the screening to its catalogue entry and copied the entry's facts onto it (Webedia's
 * Filmstarts/SensaCine ids, Cineamo's TMDB-keyed ids, kinoprogramm.com's and kinoheld.de's film pages), or the listing's
 * page is a listings site's catalogue entry (Flicks' `/movie/<slug>/`). Such facts are that catalogue's claim of WHICH
 * film the entry is: they agree with the entry by construction, so they cannot prove the link right, and one wrong entry
 * would otherwise speak for every venue the feed lists it at. Each rule that reads a listing's facts asks this model how
 * far it may trust the one it reads ([[Trust]]): a newly found quirk of a feed is then one change to that feed's profile
 * here, not a new guard in each rule.
 *
 * What each trust means to the readers (who reads which fact is in [[Fact]]'s doc):
 *  - [[Trust.Decide]]: the fact rules a film in or out on its own — a venue's year vetoes another film's record;
 *  - [[Trust.Support]]: it corroborates a take other evidence reaches, never alone — one vote however many venues carry
 *    it, one dissent rather than a veto, a link taken only once something the feed did not copy confirms it;
 *  - [[Trust.Ignore]]: no evidence at all.
 */
object SourceReliability {

  enum Trust { case Decide, Support, Ignore }

  /** The facts a source states, and who reads each. */
  enum Fact {
    /** A catalogue id naming the film: `agreement.Catalogue` (taken directly when Decide), `ConstraintEdges.catalogueKey`
     *  (must-links listings alone when Decide, beside an agreeing title otherwise), `FeedLink` and `UnifiedEvidence`'s
     *  `family.feedLink` (a TMDB-keyed link). */
    case Link
    /** `Agreement.factVotes` / `contradictedByTheListing` / `contradictedByTheCatalogue`, `agreement.Catalogue`'s
     *  corroboration, `SuspectTakes.era`. A silent-film billing dates the film too, and is read as its year. */
    case Year
    /** As the year, and `UnifiedEvidence`'s `family.director`. */
    case Directors
    /** `Agreement.listingVotes`' running times. */
    case Runtime
    /** The original title copied beside the venue's title. */
    case OriginalTitle
    /** `Posters.shows`. */
    case Poster
    /** The people a synopsis and cast field name: `Listing.names`, the agreement's venue names. */
    case Names
  }

  final case class Rule(trust: Trust, why: String)

  /** How a source is recognised on a listing: by the catalogue ids it attaches, by the page it serves as the listing's own,
   *  or by a listings site's catalogue entry page — whose facts are the site's only when read off that page
   *  ([[Listing.pageFacts]]): the day listing's own card stays the venue's. */
  enum Recognition {
    case Own
    case Ids(sources: Set[String])
    case Pages(prefixes: Seq[String])
    case EntryPages(prefixes: Seq[String])
  }

  /** A source: how it is recognised, how far each fact it states is trusted and why, and — for a feed whose catalogue is
   *  TMDB's own — the TMDB film an id of it names. */
  final case class Source(name: String, recognition: Recognition, rules: Map[Fact, Rule], tmdbFilm: String => Option[Int] = _ => None) {
    require(Fact.values.forall(rules.contains), s"$name: every fact needs a rule")
    def trust(fact: Fact): Trust = rules(fact).trust
    def decides(fact: Fact): Boolean = trust(fact) == Trust.Decide
  }

  private def rules(entries: (Fact, Trust, String)*): Map[Fact, Rule] = entries.map((f, t, why) => f -> Rule(t, why)).toMap

  /** The facts a feed copies from its catalogue entry: the entry's claim, one vote for its film, never a veto. */
  private val CopiedFromTheEntry = "copied from the feed's catalogue entry: the entry's claim of which film it is, which agrees with " +
    "the entry by construction — one vote however many venues the feed lists it at, one dissent rather than a veto " +
    "(DE \"To The Bone\": Filmstarts' feed credits Erin Li's 2014 short to a venue billing Noxon's 2017 feature)"
  private def copied(facts: Fact*): Seq[(Fact, Trust, String)] = facts.map(f => (f, Trust.Support, CopiedFromTheEntry))
  private val EntrysPoster = (Fact.Poster, Trust.Ignore, "the entry's poster, which votes for the entry whether or not it is the venue's film")
  private val EntrysText   = (Fact.Names, Trust.Ignore, "the synopsis and cast describe the entry, maybe another film than the venue's")

  /** The venue's own statement. */
  val Venue: Source = Source("venue", Recognition.Own, Fact.values.map(f => f -> Rule(Trust.Decide, "the venue's own statement")).toMap)

  /** Webedia's film id (Filmstarts in Germany, SensaCine in Spain), attached by its showtimes feed. */
  val Webedia: Source = Source(CatalogueSources.Webedia.source, Recognition.Ids(Set(CatalogueSources.Webedia.source)), rules(
    (Fact.Link, Trust.Support, "an aggregator's link of the screening to its entry, which can be the wrong entry (Filmstarts' " +
      "placeholder 51988 \"Film Program\" at two venues' different films): taken only when a venue's own facts or the title's " +
      "search corroborate it, and joining only listings whose titles already agree")) ++
    rules(copied(Fact.Year, Fact.Directors, Fact.Runtime, Fact.OriginalTitle)*) ++ rules(EntrysPoster, EntrysText))

  private val CineamoFilm = """(\d+)m""".r
  /** Cineamo's film record id ("9695m"), attached to every German showing its feed lists — TMDB's own id and "m". */
  val Cineamo: Source = Source(CatalogueSources.Cineamo, Recognition.Ids(Set(CatalogueSources.Cineamo)), rules(
    (Fact.Link, Trust.Support, "the venue operator's pick from a TMDB search, a HINT: overruled when no other source backs the " +
      "linked film and two back a rival the title names, or the title names another film exactly (FeedLink: DE \"Karla\" " +
      "linked to the 2006 film, \"Jin-Roh\" to Kim Jee-woon's 2018 \"Illang\"); a fill it backs needs a database family's take " +
      "too (family.feedLink)")) ++
    rules(copied(Fact.Year, Fact.Directors, Fact.Runtime)*) ++
    rules((Fact.OriginalTitle, Trust.Support, CopiedFromTheEntry + "; and the linked film's English title restated, so a namesake " +
      "only it names, or a short where the venue bills another title, is the copy's pull and denied the listing when the link " +
      "names another film (FamilyScope.namedOnlyByCopiedTitle, FeedLink.deniesShort: DE \"Rodychi\" took the 2018 \"Kin\", " +
      "\"L`Estive\" a 3-minute \"Summer Meadow\")")) ++ rules(EntrysPoster, EntrysText),
    tmdbFilm = id => CineamoFilm.unapplySeq(id).flatMap(_.headOption).flatMap(_.toIntOption))

  /** kinoprogramm.com, by the catalogue page it serves as every venue's listing (`/kinofilm/<slug>-<id>`). */
  val Kinoprogramm: Source = Source("kinoprogramm", Recognition.Pages(Seq("https://www.kinoprogramm.com/", "https://kinoprogramm.com/")), rules(
    (Fact.Link, Trust.Support, "a page, not a catalogue id: names no film another database maps")) ++
    rules(copied(Fact.Year, Fact.Directors, Fact.Runtime, Fact.OriginalTitle)*) ++ rules(EntrysPoster, EntrysText))

  /** kinoheld.de, by the film page of the film it linked a show to (`/film/<slug>`, `KinoheldClient`). */
  val Kinoheld: Source = Source("kinoheld", Recognition.Pages(Seq("https://www.kinoheld.de/film/")), rules(
    (Fact.Link, Trust.Support, "a page, not a catalogue id: names no film another database maps")) ++
    rules(copied(Fact.Year, Fact.Directors, Fact.Runtime, Fact.OriginalTitle)*) ++ rules(EntrysPoster, EntrysText))

  /** Flicks' film page, linked by every UK and US venue's listing of the film: the facts read off it (`FlicksFilmPage`:
   *  year, directors, countries) are Flicks' claim of which film the entry is — one mislinked entry would otherwise veto
   *  the right film at every venue showing it. */
  val Flicks: Source = Source("flicks", Recognition.EntryPages(Seq("https://www.flicks.co.uk/movie/", "https://www.flicks.us/movie/")), rules(
    (Fact.Link, Trust.Support, "a page, not a catalogue id: names no film another database maps"),
    (Fact.Year, Trust.Support, "read off the listings site's catalogue entry, not the venue's card"),
    (Fact.Directors, Trust.Support, "read off the listings site's catalogue entry, not the venue's card"),
    (Fact.Runtime, Trust.Support, "read off the listings site's catalogue entry, not the venue's card"),
    (Fact.OriginalTitle, Trust.Support, "read off the listings site's catalogue entry, not the venue's card"),
    EntrysPoster, EntrysText))

  /** The aggregators' showtimes feeds. */
  val Feeds: Seq[Source] = Seq(Webedia, Cineamo, Kinoprogramm, Kinoheld)
  /** Every source but the venue's own. */
  val Others: Seq[Source] = Feeds :+ Flicks

  /** The catalogue sources whose ids a feed attaches. */
  val FeedIdSources: Set[String] = Feeds.flatMap(_.recognition match { case Recognition.Ids(sources) => sources; case _ => Set.empty }).toSet

  private def byId(id: CatalogueId): Option[Source] =
    Feeds.find(_.recognition match { case Recognition.Ids(sources) => sources(id.source); case _ => false })
  private def byPage(page: String): Option[Source] =
    Feeds.find(_.recognition match { case Recognition.Pages(prefixes) => prefixes.exists(page.startsWith); case _ => false })
  private def entryOf(page: String): Option[Source] =
    Others.find(_.recognition match { case Recognition.EntryPages(prefixes) => prefixes.exists(page.startsWith); case _ => false })

  /** Is `page` a listings site's catalogue entry ([[Recognition.EntryPages]]: Flicks' film page)? */
  def listingsSiteEntry(page: String): Boolean = entryOf(page).isDefined

  /** Whose claim a listing's facts are: the feed that linked it (by its id, else its page), else the listings site whose
   *  entry page the facts were read off ([[Listing.pageFacts]]), else its venue. */
  def factsOf(catalogueIds: Seq[CatalogueId], page: Option[String], pageFacts: Boolean): Source =
    catalogueIds.iterator.flatMap(byId).nextOption()
      .orElse(page.flatMap(byPage))
      .orElse(page.filter(_ => pageFacts).flatMap(entryOf))
      .getOrElse(Venue)
  def factsOf(listing: Listing): Source = factsOf(listing.catalogueIds, listing.page, listing.pageFacts)

  def trust(listing: Listing, fact: Fact): Trust = factsOf(listing).trust(fact)
  def decides(listing: Listing, fact: Fact): Boolean = trust(listing, fact) == Trust.Decide

  /** Are the facts a feed's claim — its id or its page on the listing — whether or not they were read ([[factsOf]] with no
   *  page facts): what a listing held as its ids and page alone carries (the review pages' stored listing). */
  def feedStated(catalogueIds: Seq[CatalogueId], page: Option[String]): Boolean =
    factsOf(catalogueIds, page, pageFacts = false) ne Venue

  /** Does any source but the venue speak for the listing — a feed's id or page, or a listings site's entry as its page,
   *  its facts read off it or not? What the broadcast join requires a confirming year or director to be free of. */
  def venueOnly(listing: Listing): Boolean =
    (factsOf(listing) eq Venue) && !listing.page.exists(listingsSiteEntry)

  /** The source of a catalogue id: the feed attaching it, else the venue's own (an id its client or page captured). */
  def linkOf(id: CatalogueId): Source = byId(id).getOrElse(Venue)
  /** May the id name the film on its own ([[Fact.Link]] Decide), or only once something the feed did not copy confirms it? */
  def linkDecides(id: CatalogueId): Boolean = linkOf(id).decides(Fact.Link)

  /** The TMDB film a TMDB-keyed feed linked `listing` to, with the id that links it. */
  def tmdbLinked(listing: Listing): Option[(CatalogueId, Int)] =
    listing.catalogueIds.iterator.flatMap(id => byId(id).flatMap(_.tmdbFilm(id.id)).map(id -> _)).nextOption()

  /** Whose claim `listing`'s facts are, for counting the sources that back a film ([[VenueBacking]]): the
   *  feed entry they were copied from — one claim however many venues the feed lists it at: its id, or its page's last
   *  segment (kinoprogramm.com serves one entry under every venue's path) — else its venue's own: the venue's name itself,
   *  so the corpus's title groups hold no new string for a venue's own claim ([[Backer]]). A feed's
   *  claim is keyed "c:…" or "p:…", no venue's name. */
  def claimOf(listing: Listing): String =
    listing.catalogueIds.find(id => byId(id).isDefined).map(_.key)
      .orElse(listing.page.flatMap(page => Feeds.iterator.flatMap(_.recognition match {
        case Recognition.Pages(prefixes) => prefixes.find(page.startsWith)
        case _                           => None
      }).nextOption().map(prefix => s"p:$prefix${page.reverse.takeWhile(_ != '/').reverse}")))
      .getOrElse(listing.venue)

  /** `listing` with only the facts its source states with `trust` — the others empty: a reader weighing the venues' own
   *  facts reads `stated(listing, Decide)`, one weighing a feed's claim `stated(listing, Support)`. A listing whose every
   *  fact is held so is itself. */
  def stated(listing: Listing, trust: Trust): Listing = {
    val source = factsOf(listing)
    def keeps(fact: Fact) = source.trust(fact) == trust
    if (Seq(Fact.Year, Fact.Directors, Fact.Runtime, Fact.OriginalTitle).forall(keeps)) listing
    else listing.copy(year = listing.year.filter(_ => keeps(Fact.Year)), directors = if (keeps(Fact.Directors)) listing.directors else Nil,
      runtime = listing.runtime.filter(_ => keeps(Fact.Runtime)), originalTitle = listing.originalTitle.filter(_ => keeps(Fact.OriginalTitle)))
  }
}
