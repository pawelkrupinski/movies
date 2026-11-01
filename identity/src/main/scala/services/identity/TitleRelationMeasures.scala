package services.identity

import services.billing.{BillJoins, BillingParse}
import services.movies.TitleContainment

import IdentityMeasures.*

/** How a listing's titles name a film ([[titleRelation]]), its original title's relation, the pieces naming a film, and
 *  the titles venues publish for a film.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object TitleRelationMeasures {
  /** How the listing's title names the film: its localised title exactly, its original title,
   *  an alternative title, one whole banner segment, the film's title as a token run along one edge
   *  of the listing's (`decorated`: "Ken Russell's The Devils"), the listing's title as a run along
   *  one edge of the film's (`fragment`: "It" beside "It Ends with Us"), some shared words, or
   *  nothing. The two containments are opposite evidence — a decoration names the film, a fragment
   *  names a shorter, often different one — so they are measured apart.
   *
   *  A film record of the listing's season production ([[StageRelays.namesSeasonProduction]]) is a
   *  `segment`: the listing's work segment is the record's, under the same season. Only a FILM's
   *  record says so — two listings' banners do not tell one house from another (the Met's and the
   *  Royal Opera's "Carmen" of one season), so `listingListing` does not read it.
   *
   *  So is a record BILLING the listing's work under the listing's house ([[StageRelays.billing]]): the same
   *  banner once seasons and bracketed years are dropped ("The Metropolitan Opera: Manon (2027)"
   *  and "The Metropolitan Opera 2026/27: Manon"), one abbreviating the other ([[StageRelays.abbreviates]]), or
   *  the house the listing's banner was learned to be (`houses`). The first two are the names
   *  alone, so two listings read them too: UK "NT LIVE: Golden Boy (15)" [180′] and "National
   *  Theatre Live: Golden Boy" [150′] measured an `overlap`, which with the half hour between them
   *  kept them apart by the learned cut, and "NT Live: Golden Boy" ×5 with them. */
  def titleRelation(l: Listing, f: Film, houses: StageRelays.Houses = StageRelays.Houses.Unknown, qualifiers: Qualifiers = Qualifiers.Unknown): Category =
    titleRelation(l, f, Some(houses), qualifiers)

  /** Does none of the listing's own titles or its original share a word with any of the film's titles — not even one a
   *  title runs together ("NTLive: Hayfever" of "National Theatre Live: Hay Fever"), a word of [[GluedWord]] letters at
   *  least found inside the other's key, nor one misspelt by a letter ("Grenzrerfahrungen" of "Grenzerfahrungen", a word
   *  of [[SpeltWord]] letters at least)? A title relation of `none`, read without the relation's pieces. */
  def sharesNoWord(l: Listing, f: Film): Boolean = {
    val own = l.ownForms ++ l.originalTitle.map(TitleForm(_))
    def misspelt(a: TitleForm, b: TitleForm) =
      a.words.exists(word => word.length >= SpeltWord && b.words.exists(other => tools.EditDistance.within(word, other, 1)))
    !own.exists(a => f.forms.exists(b => a.wordSet.exists(b.wordSet) || misspelt(a, b) ||
      a.words.exists(word => word.length >= GluedWord && b.key.contains(word)) || b.words.exists(word => word.length >= GluedWord && a.key.contains(word))))
  }
  private val GluedWord = 4
  private val SpeltWord = 5

  /** Do a venue of country `venue` and a film of country `film` (ISO-3166-1 codes) speak one language: the same country, or
   *  two this deployment serves in one language ([[models.Country.all]]: the United Kingdom and the United States)? */
  def sharesLanguage(venue: String, film: String): Boolean = venue == film || languageOf(venue).exists(languageOf(film).contains)
  /** The language (ISO 639-1) this deployment serves a country (ISO-3166-1) in, if it serves it. */
  def languageOf(country: String): Option[String] = models.Country.all.find(_.language.getCountry == country).map(_.language.getLanguage)

  /** `houses`: `None` for two listings, whose banners name no house on record.
   *
   *  A record title that is only one of the listing's [[Qualifiers]] names nothing: "Director's
   *  Cut" (2016) shares the words of "Dark City: Director's Cut" but is not its film, so it is
   *  measured on the record's other titles, and as an `overlap` when it has none. */
  private[identity] def titleRelation(l: Listing, f: Film, houses: Option[StageRelays.Houses], qualifiers: Qualifiers): Category = {
    val ls  = l.ownForms
    val all = f.forms
    val own = l.ownKeys
    lazy val qualifying = qualifiers.of(l)
    val fs  = if (qualifiers.companions.isEmpty) all else all.filterNot(t => qualifying(t.yearless))
    val (titleForm, rest) = (all.head, all.tail)
    val (originalForms, alternativeForms) = rest.splitAt(f.originalTitle.size)
    // The film's title with the leading article the venue dropped: "Brides of Dracula" is "The Brides of Dracula"
    // (US Metrograph's listing took Fisher's "Dracula" over a `fragment` of its own film). Three words left at least —
    // "Spookies" is no more "The Spookies" than any other — and never the other way: a listing's own article is its title's.
    // A plain title, not a banner's: "Royal Ballet: Swan Lake" is not thereby "The Royal Ballet: Swan Lake" (2024), the
    // house's earlier recording, over the 2026/27 broadcast its banner names.
    val articleless = (t: TitleForm) => !t.text.contains(':') && t.afterArticle.exists(rest => rest.sizeIs >= 3 &&
      ls.exists(a => !a.text.contains(':') && a.words == rest))
    if (own.contains(titleForm.key) || articleless(titleForm) || ls.exists(a => oneTypoApart(a.words, titleForm.words)) ||
        (titleForm.latinKey.nonEmpty && ls.exists(_.latinKey == titleForm.latinKey)) ||
        ls.exists(a => (a.respelt || titleForm.respelt) && a.spellingKey.nonEmpty && a.spellingKey == titleForm.spellingKey)) Category("exact")
    else if (originalForms.map(_.key).exists(own) || originalForms.exists(articleless)) Category("original")
    else if (alternativeForms.map(_.key).exists(own) || alternativeForms.exists(articleless)) Category("alternative")
    else (if (fs.isEmpty) None
          else containment(ls, l.shapeKeys, fs,
            houses.isDefined && StageRelays.namesSeasonProduction(l, f) || StageRelays.billing(l, f).exists(houses.getOrElse(StageRelays.Houses.Unknown).same) ||
              billsWorksOfTwoTitles(l, f), l.shapeWords, l.undecoratedWords)).getOrElse(
      if (ls.exists(a => all.exists(b => a.wordSet.exists(b.wordSet)))) Category("overlap") else Category("none"))
  }

  /** How one side's titles name the other's once no whole title matches: a whole delimited piece
   *  of them (a banner segment, the title without a trailing bracket — `segment`, from `shapes`),
   *  the other's title as a token run along one edge of one of them (`decorated`: "Ken Russell's
   *  The Devils"), or one of them as a run along one edge of the other's (`fragment`: "It" beside
   *  "It Ends with Us"). `alsoSegment` is another reason to read a segment (the title relation's
   *  season production, or a billing naming the works of two of its titles), asked only when no shape matches. ONE definition for the title and the
   *  original-title relations. */
  private def containment(own: Seq[TitleForm], shapeKeys: Seq[String], others: Seq[TitleForm], alsoSegment: => Boolean = false,
                          shapeWords: Seq[Seq[String]] = Nil, undecorated: Seq[Seq[String]]): Option[Category] = {
    val otherKeys = others.map(_.key).filter(_.nonEmpty).toSet
    // a title closing on an author's credit is read without it: the author's name is no edge a film's title decorates
    val ow = own.map(form => creditedWork(form.text).fold(form.words)(words)).filter(_.nonEmpty)
    val fw = others.map(_.words).filter(_.nonEmpty)
    // A shape one venue typo from the film's title is a segment too ("Pradhama Drishtiya Kuttakkar
    // (Malayalam)" of "Pradhama Drishtya Kuttakkar", `oneTypoApart`).
    if (shapeKeys.exists(otherKeys) || shapeWords.exists(sw => fw.exists(oneTypoApart(sw, _))) || alsoSegment) Some(Category("segment"))
    // The title with its learned banner stripped decorates the film too: "Horror Season 2026 Manhunter:
    // The Final Cut" leaves "Manhunter: The Final Cut", "Manhunter" and an edition label. Not any shape:
    // "Fanciulla Encore (2027)" without its year does not decorate a film called "Encore".
    else if ((ow ++ undecorated).exists(a => fw.exists(b => TitleContainment.isTokenRun(b, a)))) Some(Category("decorated"))
    else if (ow.exists(a => fw.exists(b => TitleContainment.isTokenRun(a, b)))) Some(Category("fragment"))
    else None
  }

  /** The pieces of the listing's title that NAME the film, as words: a title shape equal to one of
   *  the film's titles (the whole title, a banner segment), or one of the film's titles as a run
   *  along one edge of the listing's (a decoration). Empty when no piece names it. */
  def namingPieces(l: Listing, f: Film): Set[Seq[String]] = {
    // The record's and the listing's forms, each normalised once per instance: this runs for every
    // node against every candidate (`namesOnlyItsVenue`), a quarter of a resolve when it re-ran the regexes.
    val filmForms = f.forms.filter(_.key.nonEmpty)
    val keys      = filmForms.map(_.key).toSet
    val segments  = l.shapeKeys.zip(l.shapeWords).collect { case (shapeKey, shapeWords) if keys(shapeKey) => shapeWords }
    val edgeRuns  = for {
      own  <- l.ownForms.map(_.words)
      film <- filmForms.map(_.words) if TitleContainment.isTokenRun(film, own)
    } yield film
    (segments ++ edgeRuns).filter(_.nonEmpty).toSet
  }

  /** Does the listing's title name the two films by DISJOINT pieces — "Lalka (Dolly)" names
   *  Kawalski's "Lalka" by one word and Blackhurst's "Dolly" by the other, and neither by the
   *  whole? Then its title names both alike, and only what else it publishes can tell them apart.
   *  Two pieces are disjoint when they share no word, or when they sit at spans of the listing's
   *  title that do not overlap: the double bill "The Gruffalo + The Gruffalo's Child" names each
   *  film by its own part of the title, though both parts spell "The Gruffalo". Nested pieces are
   *  no such tie: "Joker: Folie à deux" names its own film by the whole title and "Joker" only by
   *  a piece of it, and a film whose own title joins two others' ("Romeo + Juliet") is named by
   *  the whole title, which every piece overlaps. */
  def namedApart(l: Listing, a: Film, b: Film): Boolean = {
    val (pa, pb) = (namingPieces(l, a), namingPieces(l, b))
    pa.nonEmpty && pb.nonEmpty && (
      pa.forall(x => pb.forall(y => (x.toSet intersect y.toSet).isEmpty)) ||
        l.ownTitles.map(words).distinct.exists(placedApart(pa.toSeq, pb.toSeq, _)))
  }
  /** Can every piece of `pa` and of `pb` be placed in `title` (one occurrence each) so that no word
   *  of the title is under a piece of both? Every piece must occur in it: "Tokyo Story (Tokyo
   *  Monogatari)" names Ozu's film by both of its titles, which leave "Tokyo" nowhere of its own. */
  private def placedApart(pa: Seq[Seq[String]], pb: Seq[Seq[String]], title: Seq[String]): Boolean = {
    def placements(pieces: Seq[Seq[String]]): Seq[Set[Int]] =
      pieces.foldLeft(Seq(Set.empty[Int])) { (covered, p) =>
        for { c <- covered; i <- title.indices if title.startsWith(p, i) } yield c ++ (i until i + p.size)
      }
    val (ca, cb) = (placements(pa), placements(pb))
    ca.exists(x => cb.exists(y => (x intersect y).isEmpty))
  }

  /** The titles VENUES publish for film records, beside the record's own: a listing whose original
   *  title names one record exactly publishes its own title as that record's title in its language
   *  — an alternative title the record may lack (TMDB titles André Rieu's 2026 Maastricht concert
   *  in English only; Multikino lists it as "Andre Rieu. Niech żyje Maastricht!", originally "Andre
   *  Rieu's 2026 Summer Concert: Viva Maastricht!"). Read so only when
   *  - every listing of the title that publishes a different original title publishes the same one:
   *    venues giving one title two originals ("La invitación" as "The Invitation" and "The Invite")
   *    name two films by it;
   *  - that original is the title or original title of exactly ONE of `films`: "Niebo nad
   *    Normandią", originally "Pressure", names one of nine "Pressure"s, and which one is the
   *    listing's facts' to tell, not a title the other eight gain; and
   *  - the title does not name the record already ([[NamingRelations]]): "Coraline (2009)",
   *    originally "Coraline", is a dated spelling of the record's title, not a translation; and
   *  - it is no stage relay of another stage work than the record's ([[StageRelays.namesAnotherStageWork]]): DE Cinefactory's
   *    "MET Opera: Macbeth 2026", its original title copied from last season's Onegin, is no title of "MET Opera: Eugen
   *    Onegin"; and
   *  - it numbers no other instalment than the record's ([[numeralRelation]] `different`): DE UFA Palast's "League of
   *    Legends Worlds 26 - Finals in Cinema", its original title copied from the 2025 final's "League of Legends
   *    Worlds25 - Finals in Cinema", is no title of that record.
   *  Keyed by film id, each title once (its smallest spelling), sorted: a function of the two sets. */
  def venueTitles(listings: Iterable[Listing], films: Iterable[(Int, Film)]): Map[Int, Seq[String]] = {
    val translated = listings.iterator.flatMap(l => l.originalTitle.map(o => (key(l.title), key(o), l.title)))
      .filter { case (t, o, _) => t.nonEmpty && o.nonEmpty && t != o }.toSeq
    val unanimous = translated.groupMap(_._1)(_._2).collect { case (t, os) if os.distinct.sizeIs == 1 => t -> os.head }
    val spelling  = translated.groupMapReduce(_._1)(_._3)((a, b) => if (a <= b) a else b)
    val filmsByKey = films.toSeq.flatMap { case (id, f) => f.ownTitleKeys.map(_ -> (id, f)) }
      .groupMap(_._1)(_._2)
    val byOriginal = unanimous.toSeq.flatMap { case (t, o) =>
      filmsByKey.getOrElse(o, Nil).distinctBy(_._1) match {
        case Seq((id, f)) if !NamingRelations(titleRelation(Listing(spelling(t)), f).value) &&
          !StageRelays.namesAnotherStageWork(Listing(spelling(t)), f) &&
          numeralRelation(Listing(spelling(t)), f) != Category("different") => Some(id -> spelling(t))
        case _ => None
      }
    }
    byOriginal.groupMap(_._1)(_._2).map { case (id, ts) => id -> ts.distinct.sorted }
  }

  /** The second way a venue publishes a record's title in its language — asked per title key, of that key's
   *  listings and the records their own searches and walks reached (so it is local to the key, as the live
   *  corpus keeps it): listings of one title whose own
   *  director and year single out ONE record of `films` (the director the same person, the year within
   *  one) — "Vincent. Legenda oceanu" [2025] {Reza Memari} is TMDB's English-only "The Last Whale Singer",
   *  so a sibling listing billing that title bare reads it as the record's alternative title, not an
   *  overlap. Read so only when every listing of the title whose facts single out a record single out the
   *  same one, no record of `films` their years do not rule out carries the title whole already (as its title, original
   *  or alternative title — this one or another), the title is no piece of the record's own (a tour's name is
   *  not its São Paulo film's), contains no OTHER record its years do not rule out, is no double bill's, and is no stage
   *  relay of another stage work than the record's ([[StageRelays.namesAnotherStageWork]]). */
  def titlesByFacts(listings: Iterable[Listing], films: Iterable[(Int, Film)]): Seq[(Int, String)] = {
    val pool = films.toSeq
    // A bill's title names two works: its facts may single out one, but the title is no record's own.
    val singled = listings.iterator.filter(l => l.directors.nonEmpty && l.statedYear.isDefined && !BillJoins.joinsWorks(l.title)).map { l =>
      val year = l.statedYear.get
      val ids  = pool.collect { case (id, f) if f.year.exists(FactRelations.yearsNear(_, year)) &&
        creditsItsDirector(l, f) => id }.distinct
      (key(l.title), l.title, ids, l.statedYear)
    }.filter(_._1.nonEmpty).toSeq
    singled.groupBy(_._1).toSeq.flatMap { case (_, ls) =>
      // A listing whose facts single out no record here, or several, says nothing; one singling out another denies.
      ls.map(_._3).filter(_.sizeIs == 1).distinct match {
        // A title some record of the pool carries already is that record's to answer for ("Obcy" is Ozon's
        // 2025 film however a 2026 listing's facts lean): only a title no record its years do not rule out names is
        // learned. PL "Ktoś całkiem obcy" [2024, 2026] {Brandt Andersen} is his "I Was a Stranger", titled in Arabic and
        // English only, not Foley's 2007 "Perfect Stranger", whose Polish title it is.
        // Nor a title pointing at ANOTHER record its facts do not rule out: "BTS World Tour 'ARIRANG' In Buenos
        // Aires: Live" credits the tour's director, whom only the São Paulo record carries, while its title
        // names the Buenos Aires one ("…: Live Viewing").
        case Seq(Seq(id)) =>
          val spelling = ls.map(_._2).min
          val years    = ls.flatMap(_._4).distinct
          def relation(f: Film) = titleRelation(Listing(spelling), f).value
          def ruledOut(f: Film) = f.year.exists(y => years.nonEmpty && years.forall(l => !FactRelations.yearsNear(l, y)))
          // A title that is a piece of the record's own ("BTS World Tour 'ARIRANG'" of its São Paulo concert
          // film) names the tour, not the record: only a title sharing no part of it, a translation, is learned.
          Option.when(pool.collectFirst { case (`id`, f) => !ContainingRelations(relation(f)) && !StageRelays.namesAnotherStageWork(Listing(spelling), f) }
              .getOrElse(false) &&
            !pool.exists { case (_, f) => Rivalling(relation(f)) && !ruledOut(f) } &&
            !pool.exists { case (other, f) => other != id && ContainingRelations(relation(f)) && !ruledOut(f) })(id -> spelling)
        case _ => None
      }
    }
  }

  /** `f` with the titles venues publish for it ([[venueTitles]]) among its alternative titles. */
  def withVenueTitles(f: Film, titles: Seq[String]): Film =
    if (titles.isEmpty) f else f.copy(alternativeTitles = f.alternativeTitles ++ titles)

  /** Does a year the original title writes ("… (2024)") agree with the film's — within one — when the
   *  yearless comparison drops it? A title that writes no year drops nothing; one that does, against a
   *  film whose year is unknown, is not read as agreeing: nothing else measures that year. */
  private val WrittenYear = """(?<!\d)(?:18|19|20)\d{2}(?!\d)""".r
  private def yearAgrees(title: String, filmYear: Option[Int]): Boolean = {
    val written = WrittenYear.findAllIn(title).map(_.toInt).toSeq
    written.isEmpty || filmYear.exists(y => written.exists(FactRelations.yearsNear(_, y)))
  }

  /** The listing's own ORIGINAL title against every title of the other side: the same title (or the same once its closing
   *  version tag is dropped, [[BillingParse.versionless]]: "Your Name (re-release)" is "Your Name."), a decorated or delimited
   *  spelling of one ([[containment]], as the title relation reads it), a shared long word, or nothing. */
  def originalTitleRelation(original: Option[String], otherTitles: Seq[String], filmYear: Option[Int] = None): Measure =
    originalFormRelation(original.map(_.trim).filter(_.nonEmpty).map(OriginalForm(_)),
      otherTitles.map(_.trim).filter(_.nonEmpty).map(TitleForm(_)), filmYear)

  /** An original title's comparison forms: its own, and its shapes' keys. */
  private[identity] final case class OriginalForm(text: String) {
    val form: TitleForm = TitleForm(text)
    private lazy val ownShapes: Seq[String] = shapes(Seq(text))
    lazy val shapeKeys: Seq[String] = ownShapes.map(key)
    /** Its delimited segments as words, read for a decoration: "Michael Mann's Manhunter: The Final
     *  Cut"'s "Michael Mann's Manhunter" ends in the film's title. */
    lazy val segmentWords: Seq[Seq[String]] =
      (ownShapes ++ ColonBreak.split(text).headOption.filter(_ != text)).filterNot(_ == text).map(words).filter(_.nonEmpty).distinct
    lazy val longWords: Set[String] = form.words.filter(_.length >= 4).toSet
    /** Its key without the closing version tag it bills ([[BillingParse.versionless]]): "The Lord of the Rings: The Two
     *  Towers – Extended" is the film's own original title, as a cut of it. */
    lazy val versionlessKey: Option[String] = BillingParse(text).versionless.map(key).filter(_.nonEmpty)
  }

  /** [[originalTitleRelation]] over forms normalised once: a node's original title meets every
   *  film of its family's pool, and re-normalising both sides per pair was a sixth of a PL re-resolve. */
  private[identity] def originalFormRelation(original: Option[OriginalForm], others: Seq[TitleForm], filmYear: Option[Int]): Measure =
    original match {
      case None => MissingListing
      case Some(o) =>
        if (others.isEmpty) MissingFilm
        // The same title — or the film's without its trailing gloss ("TKT (T'inquiète)") — or once years
        // and seasons are dropped: "The Metropolitan Opera: Così fan tutte
        // (2026)" is "The Metropolitan Opera 2026/27: Così fan tutte" (the year is measured apart).
        else if (others.exists(t => t.key == o.form.key || t.unglossedKey.contains(o.form.key) || o.versionlessKey.contains(t.key)) ||
                 others.exists(t => oneTypoApart(o.form.words, t.words)) ||
                 others.exists(_.latinKey == o.form.latinKey) ||
                 (yearAgrees(o.text, filmYear) && others.exists(t => t.yearlessWords.nonEmpty && t.yearlessWords == o.form.yearlessWords)))
          Category("match")
        else containment(Seq(o.form), o.shapeKeys, others, undecorated = o.segmentWords).getOrElse {
          if (others.exists(t => (t.words.filter(_.length >= 4).toSet intersect o.longWords).nonEmpty)) Category("overlap")
          else Category("disjoint")
        }
    }
}
