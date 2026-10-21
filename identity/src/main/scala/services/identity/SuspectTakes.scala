package services.identity

import services.billing.DecorationWords
import services.identity.agreement.Agreement
import services.movies.{ListingKey, TitleContainment, TitleWords}

/**
 * SUSPECT TAKES: a film a listing's cluster took that the listing's own evidence argues against — not a verdict, a
 * question for a person to judge (`labels.tsv`). Every guard the resolver and the agreement stage hold already stood
 * aside for the take, so a suspicion is never a veto: it reads the take after the fact, over the whole corpus, to find the
 * wrong takes no fixture replays (2026-10-12: a capture surfaced 100+ long-standing ones in one batch — "Herr der Ringe -
 * Rückkehr des Königs (ext.)" taken as a 2025 14-minute short, "The Tea Alchemist - World Premiere + Q&A" as 1941's
 * "World Premiere").
 *
 * Each [[Signal]] says why in words a judge checks against the listing without reading code. Pure: a listing, the film
 * record taken, the decision that took it, and the year it screens in.
 */
object SuspectTakes {

  enum Signal(val label: String) {
    /** The film's title covers only a part of the billed title, and the rest is no decoration, format, event word or the
     *  film's own director: "Giorgia Fumo - OUT OF OFFICE" is no 2022 "Out of Office". */
    case Fragment  extends Signal("fragment")
    /** The film is far older than the screening with nothing billing an old film (an anniversary, an edition, a silent
     *  film, a retrospective, a re-release TMDB dates), or newer than the year the listing states: "ATMOS: Michael" is
     *  no 1924 film. */
    case Era       extends Signal("era")
    /** The listing's running time is far from every one TMDB states for the film, or a short is taken for a listing
     *  running feature length. */
    case Runtime   extends Signal("runtime")
    /** The listing numbers another instalment than the film ([[IdentityMeasures.numbersAnotherInstalment]]): "Begegnungen
     *  der Seele 2" is not part 1. */
    case Sequel    extends Signal("sequel")
    /** A TMDB-keyed feed linked the listing to another film ([[SourceReliability.tmdbLinked]]). */
    case FeedId    extends Signal("feed")
    /** The take rests on a fill rule alone ([[ResolverDecision.Basis.Filled]]). */
    case WeakRoute extends Signal("route")
  }

  /** One reason a take is suspect. */
  final case class Suspicion(signal: Signal, why: String) {
    def render: String = s"${signal.label}: $why"
  }

  /** One listing's take: the film its cluster took — TMDB's (`tmdb:<id>`) or a fallback source's (`imdb:<tt>`) — its
   *  record (a fallback's: its title and year alone), and the decision that took it. `listing` as its venue's page states
   *  it ([[Listing.withPage]]). */
  final case class Take(listing: Listing, film: String, tmdb: Option[Int], record: IdentityMeasures.Film, decision: ResolverDecision)

  /** Every listing's take among `decisions`: a film whose TMDB record `recordOf` holds, or a fallback film. */
  def takes(decisions: Seq[ResolverDecision], listingOf: ListingKey => Option[Listing], recordOf: Int => Option[IdentityMeasures.Film]): Seq[Take] =
    decisions.flatMap { d =>
      val taken = d.film.fold(d.fallback.map(f => (s"${f.source}:${f.id}", Option.empty[Int], IdentityMeasures.Film(f.title.getOrElse(""), year = f.year))))(
        id => recordOf(id).map(record => (s"tmdb:$id", Some(id), record)))
      taken.toSeq.flatMap { case (film, tmdb, record) => d.members.flatMap(listingOf).map(Take(_, film, tmdb, record, d)) }
    }

  /** A corpus as the audit reads it: every listing's take and what the corpus bills ([[Billings]]). */
  final case class Corpus(takes: Seq[Take], billings: Billings)

  /** `resolution`'s takes over `listings` (every listing it decided), each listing as its venue's page states it
   *  (`venues`, as the agreement stage reads it), each film's record from the resolution, else `recordOf` (an agreed
   *  film's, which the model never decided) — and what `listings` bill ([[Billings]]). */
  def corpus(resolution: Resolution, listings: Seq[Listing], venues: IdentityLookups, recordOf: Int => Option[IdentityMeasures.Film]): Corpus = {
    val byKey  = listings.iterator.map(l => l.key -> l).toMap
    val stated = (key: ListingKey) => byKey.get(key).map(l => if (venues.hasDetail(l)) venues.detail(l).toOption.flatten.fold(l)(l.withPage) else l)
    val taken = takes(resolution.decisions, stated, id => resolution.films.get(id).orElse(recordOf(id)))
    Corpus(taken, Billings.of(listings, taken))
  }

  /** A film older than this many years before its screening is an old film, which a venue bills as one. */
  val EraYears = 30
  /** How many years a venue's stated year may stand from the film's: a festival's or a print's year beside the
   *  release's ("Podglądacz" 1957 for Powell's 1960 film). */
  val StatedYearSlack = 3
  /** A listing running at least this many minutes runs feature length. */
  val FeatureMinutes = 60
  /** The fewest words of the billing, outside the film's title and every decoration, that make it a fragment. */
  val FragmentRest = 2

  /** Why `take` is suspect — none when nothing argues against it. `screeningYear`: the year it screens in, where its
   *  listing states no days; `billings`: what the corpus bills. */
  def of(take: Take, screeningYear: Int, billings: Billings): Seq[Suspicion] = {
    val listing  = take.listing
    val film     = take.record
    val pieces   = Piece.of(listing.rawTitle)
    val screens  = listing.screenings.first.fold(screeningYear)(_.getYear)
    val measured = Evidence.of(listing, None, TitleDecorations.resolver).measured
    fragment(listing, pieces, film, screens, billings).toSeq ++ era(listing, measured, film, screens, billings) ++
      runtime(measured, film, billings, listing) ++ sequel(pieces, measured, film) ++ feed(listing, take.tmdb, take.decision) ++
      route(take.decision)
  }

  /** Every take's suspicions ([[of]]), each billing asked once: the listings a chain bills alike at a hundred venues — the
   *  same title, facts and film — are one question, whatever venue asks it. */
  def suspected(takes: Seq[Take], screeningYear: Int, billings: Billings): Seq[(Take, Seq[Suspicion])] = {
    def asked(t: Take) = (t.listing.rawTitle, t.listing.cleanTitle, t.listing.title, t.listing.year, t.listing.runtime, t.listing.directors,
      t.listing.originalTitle, t.listing.catalogueIds, t.listing.factsFromCatalogue, t.listing.screenings.first.map(_.getYear), t.film,
      t.decision.basis, t.decision.explanation.lastOption)
    val answers = scala.collection.mutable.HashMap.empty[Any, Seq[Suspicion]]
    takes.map(t => t -> answers.getOrElseUpdate(asked(t), of(t, screeningYear, billings)))
  }

  // ── the billing's pieces ─────────────────────────────────────────────────────────────────

  /** One delimited piece of a billing — what a colon, a pipe, a spaced dash or plus, a bullet, a bracket, a quote or a
   *  full stop sets apart — as its words. Not [[IdentityMeasures.pieces]], which splits a TITLE at a colon, a pipe or a
   *  spaced dash or slash alone, its years out: a billing's banner also sits behind a bullet, a plus, a half-spaced dash,
   *  brackets or quotes ("Kino Kobiet - KLAPS! „Jak żyć…”"). Split as a title is (2026-10-13, on the legs' takes), seven
   *  right takes turn fragments: "Parasite (wersja czarno-biała)", "Nosferatu (1922) with Live Score by Hugo Max". */
  private[identity] final case class Piece(words: Seq[String]) {
    lazy val key: String = words.mkString(" ")
  }
  private[identity] object Piece {
    private val Break = """\s*[:|•]\s*|\s+[-–—/+]\s*|\s*[-–—]\s+|[„”"“«»()\[\]]|[.!?]\s+""".r
    def of(billing: String): Seq[Piece] = Break.split(billing).toSeq.map(text => Piece(TitleContainment.tokens(text))).filter(_.words.nonEmpty)
  }

  /** What a corpus bills, by WORK — the film a listing was taken as, else its own cleaned title: the pieces billed beside
   *  at least [[Billings.MinWorks]] films taken — a programme's banner ("Akademia Kina Polskiego", "KLASYCZNE ŚRODY", "Kino bez
   *  barier"), not a work's title — the words at least [[Billings.MinCommonWorks]] works' billings carry ("klub", "filmowy",
   *  "edukacja"), of which no work's own title is made, and how many venues bill each title. By work, not by billing: a
   *  touring show billed at six venues ("Jan Logemann LIVE: Herr Logemann hebt ab!") or one film billed in its variants
   *  ("Herr der Ringe - Rückkehr des Königs (ext.) (OV)") is one work, whose words and pieces are its own. Built for one
   *  audit of a corpus and dropped with it, never held. */
  final class Billings private (recurring: Set[String], common: Set[String], venues: Map[String, Int], newest: Map[String, Int]) {
    def banner(piece: Piece): Boolean = recurring(piece.key) && !piece.words.forall(decoration)
    /** Is `word` one the corpus bills widely — no rare word of a work's own title? */
    def widely(word: String): Boolean = common(word)
    /** How many venues bill `listing`'s title, as its venue's rules clean it. */
    def venuesBilling(listing: Listing): Int = venues.getOrElse(Billings.titleKey(listing), 1)
    /** The year of the newest film a listing billing `listing`'s title was taken as. */
    def newestTaken(listing: Listing): Option[Int] = newest.get(Billings.titleKey(listing))
  }
  object Billings {
    /** In two works: the least that says a piece recurs across films (as `TitleDecorationsLearner.MinFilms`). */
    val MinWorks = 2
    /** The share of the corpus's works whose billings a word must be in to be one the corpus bills widely — three works at
     *  the least: PL's "klub", "filmowy" and "edukacja" are; DE's "Rückkehr" in a corpus five times the size is not. */
    val CommonShare = 0.002
    val MinCommonWorks = 3
    val None: Billings = new Billings(Set.empty, Set.empty, Map.empty, Map.empty)
    private def titleKey(listing: Listing): String = TitleContainment.tokens(listing.cleanTitle).mkString(" ")
    /** What `listings` bill, each the work `takes` took it as ("tmdb:603"), else its own title. */
    def of(listings: Iterable[Listing], takes: Seq[Take] = Nil): Billings = {
      import scala.collection.mutable
      val filmOf = takes.iterator.map(t => t.listing.key -> t.film).toMap
      // each distinct billing once, by work: a chain's hundred venues billing one title read it once
      val works  = mutable.HashMap.empty[String, (Boolean, mutable.HashSet[String], mutable.HashSet[String])]
      val venues = mutable.HashMap.empty[String, mutable.HashSet[String]]
      val read   = mutable.HashSet.empty[(String, String)]
      listings.foreach { l =>
        val title = titleKey(l)
        venues.getOrElseUpdate(title, mutable.HashSet.empty) += l.venue
        val film = filmOf.get(l.key)
        val work = film.getOrElse(title)
        if (read.add((l.rawTitle, work))) {
          val pieces = Piece.of(l.rawTitle)
          val (_, words, keys) = works.getOrElseUpdate(work, (film.isDefined, mutable.HashSet.empty, mutable.HashSet.empty))
          pieces.foreach(p => words ++= p.words)
          // a banner is billed beside FILMS: a show no film was taken for, billed with and without "AUSVERKAUFT", is one work
          if (film.isDefined && pieces.sizeIs >= 2) keys ++= pieces.map(_.key)
        }
      }
      def counted(of: ((Boolean, mutable.HashSet[String], mutable.HashSet[String])) => Iterable[String]) = {
        val n = mutable.HashMap.empty[String, Int]
        works.valuesIterator.foreach(w => of(w).foreach(k => n.update(k, n.getOrElse(k, 0) + 1)))
        n
      }
      val commonWorks = math.max(MinCommonWorks, (works.size * CommonShare).ceil.toInt)
      val newest = takes.flatMap(t => t.record.year.map(titleKey(t.listing) -> _)).groupMapReduce(_._1)(_._2)(math.max)
      new Billings(counted(_._3).collect { case (key, n) if n >= MinWorks => key }.toSet,
        counted(_._2).collect { case (word, n) if n >= commonWorks => word }.toSet, venues.view.mapValues(_.size).toMap, newest)
    }
  }

  // ── (a) fragment ─────────────────────────────────────────────────────────────────────────

  /** Words a venue bills around a film that are no part of another work's title: what it bills the film with ([[DecorationWords.billedAround]]),
   *  numbers, single letters. */
  private def decoration(word: String): Boolean = DecorationWords.billedAround(word) || word.exists(_.isDigit) || word.length == 1

  /** Where the film's longest title runs in `words` — articles aside — as (start, length). */
  private def run(words: IndexedSeq[String], film: IdentityMeasures.Film): Option[(Int, Int)] =
    film.titles.map(TitleContainment.tokens).flatMap(t => Seq(t, IdentityMeasures.withoutLeadingArticle(t)))
      .filter(_.nonEmpty).distinct.sortBy(-_.size).iterator
      .flatMap(t => Option(words.indexOfSlice(t)).filter(_ >= 0).map(_ -> t.size)).nextOption()

  /** Does no title of the film name the billing ([[Agreement.namesIt]], nor run in its words) — where a title could: the
   *  record holds one in the billing's script, and the billing is no stage relay, which a house's record titles in its own
   *  words ("OPERA-MANON" is "The Metropolitan Opera 2026/27: Manon")? "Koncert: Leszek Możdżer - solo" is no "Solo: A
   *  Star Wars Story", "Johnny" no "Johnny English". */
  private def namesNone(listing: Listing, pieces: Seq[Piece], film: IdentityMeasures.Film, billings: Billings): Boolean = {
    // a word of a title, not one the corpus bills widely: "Hayfever" shares none with "Hay Fever", "Johnny" one with
    // "Johnny English" — a respelling or a part is the title relation's to weigh, no word in common is none to weigh
    def rare(words: Iterable[String]) = words.iterator.filter(w => w.length >= 3 && !billings.widely(w)).toSet
    val billed = rare(pieces.flatMap(_.words))
    // cheapest first: most takes share a word with their film's title
    billed.nonEmpty && rare(film.titles.flatMap(TitleContainment.tokens)).intersect(billed).isEmpty && film.titles.exists(tools.LatinScript.hasLetter) &&
      tools.LatinScript.hasLetter(listing.rawTitle) && film.season.isEmpty && !ListingShape.stagesAWork(listing) && !Agreement.namesIt(listing, Seq(film))
  }

  private[identity] def fragment(listing: Listing, pieces: Seq[Piece], film: IdentityMeasures.Film, screens: Int,
                                 billings: Billings): Option[Suspicion] =
    if (ListingShape.billsSeveralBeside(listing, film)) Some(Suspicion(Signal.Fragment, s"the billing bills several works beside '${film.title}'"))
    else if (namesNone(listing, pieces, film, billings)) Some(Suspicion(Signal.Fragment, s"no title of '${film.title}' is in the billing"))
    // a current release billed with words of its own beside its title is its subtitle, its local title, its event: a
    // namesake a fragment names is an older film ("World Premiere" 1941, "Follow the Sun" 1951, "Out of Office" 2022)
    else if (film.year.forall(_ >= screens - 1)) None
    else {
      val words  = pieces.flatMap(_.words).toIndexedSeq
      val people = film.directors.getOrElse(Nil).flatMap(TitleContainment.tokens).toSet
      val titled = film.titles.flatMap(TitleContainment.tokens).toSet
      // a word of another work's title: no decoration, not the film's director nor of any of its titles, not one the
      // corpus bills widely
      def own(word: String) = !decoration(word) && !people(word) && !titled(word) && !billings.widely(word)
      // no title of the film runs in the billing: TMDB's record holds no title in the billing's language — the title
      // relation the take rests on is the resolver's to weigh, not a fragment
      run(words, film).flatMap { case (start, length) =>
        val ends   = pieces.scanLeft(0)(_ + _.words.size)
        val placed = pieces.zip(ends.zip(ends.tail))
        // a piece beside the film's: a work's title unless it is a banner the corpus bills beside other titles, or holds a
        // decoration word; the film's own piece: the words its title leaves
        val (beside, within) = placed.partition { case (_, (from, to)) => to <= start || from >= start + length }
        val rest    = beside.map(_._1).filterNot(p => billings.banner(p) || p.words.exists(DecorationWords.billedAround) || DecorationWords.namesAnEvent(p.words)).flatMap(_.words).filter(own)
        val inPiece = within.flatMap { case (p, (from, _)) =>
          p.words.indices.filter(i => from + i < start || from + i >= start + length).map(p.words) }.filter(own)
        Option.when(rest.sizeIs >= FragmentRest || (inPiece.sizeIs >= FragmentRest && inPiece.size > length))(
          Suspicion(Signal.Fragment, s"'${film.title}' is $length of the billing's words; '${(rest ++ inPiece).mkString(" ")}' is no banner or decoration"))
      }
    }

  // ── (b) era ──────────────────────────────────────────────────────────────────────────────

  /** Words billing an old film as one: a retrospective, a classic, an archive print, a silent film. */
  private val OldFilmWords = TitleWords.tokens(TitleWords.Concept.OldFilm)

  /** Does the listing bill an old film as one: in its words, an edition, a silent film, an anniversary, the film's
   *  director named? */
  private def billsAnOldFilm(listing: Listing, measured: IdentityMeasures.Listing, film: IdentityMeasures.Film): Boolean = {
    val titles = Seq(listing.title, listing.rawTitle, listing.cleanTitle) ++ listing.originalTitle
    val words  = titles.flatMap(TitleContainment.tokens).toSet
    // the film's own title words bill nothing: "The Classic" (2003) is no classic billed, as an edition's words are read
    val own    = film.titles.flatMap(TitleContainment.tokens).toSet
    words.exists(word => OldFilmWords(word) && !own(word)) || DecorationWords.billsAnEdition(titles, film.titles) || ListingShape.billsASilentFilm(listing) ||
      measured.anniversary.isDefined ||
      film.directors.getOrElse(Nil).exists(name => TitleContainment.tokens(name).lastOption.exists(words)) ||
      (listing.directors.nonEmpty && FactRelations.samePerson(listing.directors, film.directors.getOrElse(Nil)))
  }

  private[identity] def era(listing: Listing, measured: IdentityMeasures.Listing, film: IdentityMeasures.Film, screens: Int,
                            billings: Billings): Option[Suspicion] =
    film.year.flatMap { year =>
      lazy val old = billsAnOldFilm(listing, measured, film)
      // a year a feed copied from its catalogue entry is that entry's, the very take in question: no statement of the venue's
      Option.when(SourceReliability.decides(listing, SourceReliability.Fact.Year))(measured.statedYear).flatten match {
        // a year the venue states: the film's, or the year it shows a classic (an old film it bills as one)
        case Some(stated) =>
          Option.when(film.closestYear(stated, None).exists(y => math.abs(y - stated) > StatedYearSlack) && (year > stated || !old))(
            Suspicion(Signal.Era, s"the listing states $stated, the film is from $year"))
        case None =>
          // a release TMDB dates within a year of the screening is a re-release: the venue screens it as current
          val reReleased = film.releases.exists(codes => TmdbFilmRecord.Release.all(codes).exists(_.year >= screens - 1))
          // an old film whose title another listing was taken as a newer film for is a namesake of that release, unless it is
          // billed as old: an old film alone — a classic, a Christmas favourite — is the venue's programme ("Die Hard" at 17)
          val newer = billings.newestTaken(listing).filter(_ >= year + EraYears)
          newer.filter(_ => year < screens - EraYears && !reReleased && !old).map(n =>
            Suspicion(Signal.Era, s"the film is from $year, ${screens - year} years before the screening, nothing bills an old film, " +
              s"and the title's other billings take a film from $n"))
      }
    }

  // ── (c) runtime ──────────────────────────────────────────────────────────────────────────

  private[identity] def runtime(measured: IdentityMeasures.Listing, film: IdentityMeasures.Film, billings: Billings,
                                listing: Listing): Option[Suspicion] = {
    val runs = ShortFilms.runtimesOf(film)
    val stated = measured.statedRuntime
    // a short several venues bill at once: no programme of shorts is a wide release
    lazy val venues = billings.venuesBilling(listing)
    if (stated.isEmpty && ShortFilms.isShort(film) && venues >= Agreement.WidelyBilled)
      Some(Suspicion(Signal.Runtime, s"a ${runs.max}-minute short taken for a title $venues venues bill"))
    else stated.filter(_ => runs.nonEmpty).flatMap { stated =>
      if (stated <= ShortFilms.MaxMinutes) None // a venue billing a short: its programme, not the film's running time
      else if (ShortFilms.isShort(film) && stated >= FeatureMinutes)
        Some(Suspicion(Signal.Runtime, s"a ${runs.max}-minute short taken for a listing running $stated minutes"))
      else Option.when(runs.forall(x => math.abs(stated - x) >= math.max(IdentityMeasures.RuntimeContradiction.toDouble, stated * 0.3)))(
        Suspicion(Signal.Runtime, s"the listing runs $stated minutes, the film ${runs.sorted.mkString("/")}"))
    }
  }

  // ── (d) sequel ───────────────────────────────────────────────────────────────────────────

  private[identity] def sequel(pieces: Seq[Piece], measured: IdentityMeasures.Listing, film: IdentityMeasures.Film): Option[Suspicion] =
    // a billing numbering nothing — no digit, no closing Roman numeral — numbers no other instalment, read without its
    // title shapes (a season's relay, "RBO: Cosi fan Tutte 27", numbers its season: `numeralRelation` compares nothing)
    Option.when(pieces.exists(p => p.words.exists(_.exists(_.isDigit)) || p.words.lastOption.exists(Roman.matches)) &&
      IdentityMeasures.numbersAnotherInstalment(measured, film))(
      Suspicion(Signal.Sequel, s"the billing numbers another instalment than '${film.title}'"))
  private val Roman = "[ivxl]+".r

  // ── (e) the feed's own id ────────────────────────────────────────────────────────────────

  /** A TMDB-keyed feed's link the stage after the model overrode: the model's own takes overrule a link on its evidence
   *  ([[FeedLink]]); a take on the agreement's or a fill's weaker evidence against the feed's is a question. */
  private[identity] def feed(listing: Listing, tmdb: Option[Int], decision: ResolverDecision): Option[Suspicion] =
    if (ModelBases(decision.basis)) None
    else tmdb.flatMap(taken => SourceReliability.tmdbLinked(listing).filter(_._2 != taken)).map { case (id, linked) =>
      Suspicion(Signal.FeedId, s"${id.source} links the listing to tmdb:$linked")
    }

  private val ModelBases: Set[ResolverDecision.Basis] =
    Set(ResolverDecision.Basis.OwnMatch, ResolverDecision.Basis.PooledMatch, ResolverDecision.Basis.Pinned)

  // ── (f) the route ────────────────────────────────────────────────────────────────────────

  private[identity] def route(decision: ResolverDecision): Option[Suspicion] =
    Option.when(decision.basis == ResolverDecision.Basis.Filled)(
      Suspicion(Signal.WeakRoute, decision.explanation.lastOption.fold("a fill rule alone took it")(_.takeWhile(_ != ':'))))
}
