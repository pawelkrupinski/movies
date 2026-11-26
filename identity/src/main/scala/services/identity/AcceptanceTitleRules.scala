package services.identity

import services.identity.AcceptanceRule.{Weighing, isEligible}
import services.identity.measures.Scored.Accepted
import services.identity.measures.{Film, IdentityMeasures, ListingShape, Measure, MeasuredListing, MultiFilmBill, Scored}
import services.titles.NonMovieEventClassifier
import services.titles.billing.{BillingPieces, Credits}

/** What the acceptance rules that take a film by how its title is searched and billed ask: the exact and segment top
 *  hits, IMDb's suggestion, and the sole result ([[AcceptanceRuleTable]]); and what makes a listing a BILL of several
 *  works, which no rule takes a film of; and what the vetoes of [[AcceptanceRuleTable.Vetoes]] ask of any rule's take:
 *  whether the billing is something else than the film it found — a performance beside it, an author's namesake, a
 *  staged show's own record, an event no film, a page reused for another evening — each answering with what it held
 *  against the film (the refusal's detail), if anything.
 *  Functions of the candidates and the listing they score; [[Acceptance]] makes the one its rules ask. */
private[identity] final class AcceptanceTitleRules {
  import Acceptance.Refused

  /** The one film the listing's whole title names exactly that its own title search returned first, in TMDB's order
   *  ([[IdentityMeasures.exactTopHits]]): [[AcceptanceRuleTable.ExactTopHit]]'s pick, refused when it is denied. */
  private[identity] def theExactTopHit(weighing: Weighing): Either[Refused, Scored] = {
    val ranked = weighing.ranked
    (IdentityMeasures.exactTopHits(weighing.listing, ranked.map(scored => (scored.candidate.tmdbId, scored.candidate.film, scored.rank))) match {
      case Seq(only) => Right(only)
      case Seq()     => Left(Refused("no film its whole title names is its search's first hit"))
      case ids       => Left(Refused("two exact top hits", None, ids.mkString("; ")))
    }).flatMap(id => weighing.eligible.find(_.candidate.tmdbId == id).toRight(Refused("its exact top hit is denied", Some(id),
      ranked.find(_.candidate.tmdbId == id).flatMap(_.denial).fold("")(_.why))))
  }

  /** The candidates the title names that may stand for it, [[AcceptanceRuleTable.SegmentTopHit]]'s to pick one of — a
   *  one-word piece names no film here, rival or taken: "Inna Mamusia - maraton" is no "Maraton". */
  private[identity] def filmsTheTitleNames(weighing: Weighing): Seq[Scored] =
    weighing.ranked.filter(scored => scored.titleNamesIt && (scored.category("title").exists(IdentityMeasures.Rivalling) ||
      IdentityMeasures.standsForTheWhole(weighing.listing, scored.candidate.film)))
  /** The candidate read as the bare title its piece is: the banner beside it is no evidence against the film. */
  private[identity] def asItsBareTitle(scored: Scored): Scored = scored.copy(measures = scored.measures + ("title" -> Measure.Category("exact")))

  /** Is `scored` one IMDb's suggestions take, on the old pipeline's rungs: IMDb's FIRST suggestion with the year the
   *  listing states; the ONE suggested film the listing's credited director directed; or the ONLY film IMDb suggests,
   *  when the title names it and TMDB ranks no other film of that very title above it ("Ziemia obiecana" is Wajda's, not
   *  the 1927 film IMDb alone spells so); or the ONE film IMDb lists under one of the listing's search titles in some
   *  language — an AKA TMDB does not carry ("Camino dla opornych" is IMDb's Polish title of "Compostelle"). */
  private[identity] def imdbRung(weighing: Weighing, scored: Scored): Boolean =
    titledRung(weighing, scored) || scored.imdb.exists(place => datedOrCredited(weighing, scored, place) || onlySuggestion(weighing, scored, place))

  /** The film's own title or its original — not an alternative TMDB files it under ("Lumière" is not "Café Lumière"). */
  private def exactly(scored: Scored) = scored.category("title").exists(Set("exact", "original"))
  private def outranked(weighing: Weighing, scored: Scored) = weighing.undenied.exists(rival => (rival ne scored) &&
    rival.category("title").contains("exact") && rival.rank.exists(r => scored.rank.forall(r < _)))

  /** IMDb's FIRST suggestion with the year the listing states, or the ONE suggested film the listing's credited
   *  director directed. */
  private def datedOrCredited(weighing: Weighing, scored: Scored, place: Scored.ImdbPlace): Boolean =
    (place.place == 1 && scored.number("year.delta").contains(0.0)) ||
      (scored.ownCredit && weighing.suggested.count(_.ownCredit) == 1)

  private def onlySuggestion(weighing: Weighing, scored: Scored, place: Scored.ImdbPlace): Boolean =
    (place.of == 1 && scored.titleNamesIt && !outranked(weighing, scored)) ||
      // the only film IMDb suggests, its title the billing's but for a venue's slip of letters in a word or two (UK
      // "The Hunger Games Sunrise of the Reaping" ×25, DE "LA VIE DE CHÂTEAU, MON ENFACE À VERSEILLES"): TMDB's
      // search finds nothing under the slip, and only IMDb's forgiving one reaches the film
      (place.of == 1 && slippedTitle(scored.listing, scored.candidate.film) && !outranked(weighing, scored)) ||
      // the old pipeline's yearless rung: IMDb's FIRST suggestion, and the only suggested film the title is ("Kroll")
      (place.place == 1 && exactly(scored) && weighing.suggested.count(exactly) == 1 && !outranked(weighing, scored))

  // the ONE suggestion IMDb lists under the listing's own title in some language (an AKA TMDB does not carry):
  // "Camino dla opornych" is IMDb's Polish title of TMDB's French "Compostelle". Only while nothing else the
  // listing names stands beside it: no other record carrying its title, though denied (Renoir 2025 beside the 2012
  // film IMDb also lists as "Renoir"; the 1951 "Streetcar" beside the 1989 TV film), no year the title dates against
  // it ("Miłość 2024" is not Haneke's 2012 film), no numbered set ("Bolek i Lolek – zestaw IV"), a title of words,
  // not a number ("2026"), and no stage work broadcast as a non-season film (the Met's "Così fan tutte").
  private def titledRung(weighing: Weighing, scored: Scored): Boolean = {
    val ranked = weighing.ranked
    IdentityMeasures.soleImdbTitled(ranked.filter(_.imdbTitled.nonEmpty).map(s => s.candidate.tmdbId -> s.imdbTitled).toMap)
      .exists(_._1 == scored.candidate.tmdbId) &&
      !ranked.exists(other => (other ne scored) && other.category("title").exists(IdentityMeasures.TitlesItsOwn)) &&
      IdentityMeasures.takesImdbTitle(scored.listing, scored.candidate.film) && titledByMoreThanAWord(scored)
  }

  // the billing's whole title, a piece of two words at least, the word the billing opens with, or a DATED record: a
  // one-word piece after another names an undated record only as many films' title does — DE "Friday the 13th:
  // Legacy", a fan film, is not David Slade's undated "Legacy" IMDb lists under its last word, while "Spooked: Ringu"
  // is the 1998 "Ring" and "Gina (Kurzfilm)" is "Gina"
  private def titledByMoreThanAWord(scored: Scored): Boolean = scored.candidate.film.year.isDefined || scored.imdbTitled.exists { title =>
    val words = IdentityMeasures.words(title)
    scored.listing.ownKeys(IdentityMeasures.key(title)) || words.sizeIs >= 2 ||
      scored.listing.ownForms.exists(_.words.startsWith(words))
  }

  /** Does TMDB's own search rank `rival`, another film the title names, above the IMDb-suggested `scored` ("BTS
   *  'ARIRANG' IN SÃO PAULO" is TMDB's São Paulo record, not IMDb's first 2026 suggestion, Busan; "Kroll" is TMDB's
   *  first, the 1991 film IMDb suggests, above 1972's "Krõll")? */
  private[identity] def searchNamesAnother(scored: Scored, rival: Scored): Boolean = (rival ne scored) && rival.titleNamesIt &&
    rival.rank.exists(r => scored.rank.forall(r < _))
  /** Does the title number another instalment than the film's own ([[IdentityMeasures.OtherInstalment]])? */
  private[identity] def otherInstalment(scored: Scored): Boolean = scored.category("numeral").exists(IdentityMeasures.OtherInstalment)
  /** Does the title number the film's own instalment — or is it the film's own title but for a slip ([[slippedTitle]]),
   *  which never numbers another? */
  private[identity] def itsOwnInstalmentOrASlip(scored: Scored): Boolean =
    slippedTitle(scored.listing, scored.candidate.film) || !otherInstalment(scored)

  /** Is one of the listing's titles the film's own or original title, word for word, but for a SLIP in at most two
   *  words — never more than half of them, of four words or more: each slipped pair words of letters, no numeral
   *  ("Part II", "2"), at most one letter edit apart per four letters (one in a short word: "of" for "on", "Enface" for
   *  "Enfance", "Caspar" for "Casper")? A slip a typist makes, not another film's title: "A Boy and His Dog" is no slip
   *  of "A Boy and Her Dog" (two edits in a three-letter word), "Part One" none of "Part Two". */
  private[identity] def slippedTitle(listing: MeasuredListing, film: Film): Boolean = {
    def letters(word: String) = word.forall(_.isLetter) && !IdentityMeasures.numbersAnInstalment(word)
    def slip(a: String, b: String) = letters(a) && letters(b) &&
      tools.EditDistance.within(a, b, math.max(1, math.min(a.length, b.length) / 4))
    val billed = (listing.ownTitles ++ listing.searchTitles).map(services.titles.TitleContainment.tokens).distinct
    val titles = (film.title +: film.originalTitle.toSeq).map(services.titles.TitleContainment.tokens).distinct
    billed.exists(own => own.sizeIs >= 4 && titles.exists { title =>
      title.sizeIs == own.size && {
        val slipped = own.zip(title).filter { case (a, b) => a != b }
        slipped.nonEmpty && slipped.sizeIs <= 2 && slipped.size * 2 <= own.size && slipped.forall(slip.tupled)
      }
    })
  }

  /** Does a BARE billing — no year, director, running time or original title of its own — reach the IMDb-suggested film
   *  only by a title TMDB files it under beside its own (`title=alternative`), while TMDB's search of the billing finds
   *  other films and not this one? Then IMDb's suggestion is all that speaks for it, against TMDB's own answer: DE
   *  Kinopolis Freiberg's "Patagonien", a travel lecture, is not Bozzelli's 2023 "Patagonia", one of whose alternative
   *  titles it is, where TMDB's search of "Patagonien" ranks "Abenteuer Patagonien" and a dozen others. A search that
   *  finds nothing says nothing against it: an arthouse's German title of a foreign film ("Eine Seite des Wahnsinns",
   *  "Tempel zur goldenen Halle") is the film. The same refusal without that condition lost 16 such right takes
   *  (docs/identity-experiments.md). */
  private[identity] def alternativeAgainstTmdbsSearch(scored: Scored, ranked: Seq[Scored]): Boolean = {
    val l = scored.listing
    l.year.isEmpty && l.directors.isEmpty && l.runtime.isEmpty && l.originalTitle.isEmpty &&
      scored.category("title").contains("alternative") && scored.rank.isEmpty && ranked.exists(_.rank.isDefined)
  }

  /** Is every word of the listing's whole title one of the film's title's — two words at least, or the film's title
   *  before its dash or colon — as the old pipeline took a whole title's only search result: "Dzień Dziecka księdza
   *  Kaczkowskiego" is "Dzień Dziecka księdza Jana Kaczkowskiego", "TAFITI" is "Tafiti – Ab durch die Wüste". Two words
   *  or more must cover the film's title ([[coversItsTitle]]): an event billed in a few words is not every longer title
   *  that happens to hold them. */
  private[identity] def wordsOfIts(scored: Scored): Boolean = {
    val own = IdentityMeasures.yearlessTokens(scored.listing.title)
    // the billing's own pieces, when it has two or more ("About Time: Once Upon a Time")
    lazy val pieces = TitlePartBreak.split(scored.listing.title).toSeq.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty)
    own.nonEmpty && (Seq(scored.candidate.film.title) ++ scored.candidate.film.originalTitle).exists { title =>
      own.forall(IdentityMeasures.yearlessTokens(title).contains) && touchesPartsWhole(own, title, scored.listing.title) &&
        ((own.sizeIs >= 2 && coversItsTitle(own, pieces, title)) || IdentityMeasures.yearlessTokens(MainTitleBreak.split(title).head) == own)
    }
  }
  /** Does the billing touch every part of `title` ([[TitlePartBreak]]) it touches at all by half that part's words or more?
   *  Or by its leading words alone, a title the venue cut short ("Shaun The Sheep : The Beast"). A part touched otherwise
   *  holds the billing's words only by chance — a banner's that recur in it: UK Watershed's "About
   *  Time: Once Upon a Time", an episode of Berger's series, touches "All About My Mother" by one word in four, so it is not
   *  "Once Upon a Time… All About My Mother". A part the billing bills in its own order is still touched whole
   *  ("GÖTTERDÄMMERUNG Royal Opera" of "Royal Ballet & Opera 2026/27: Götterdämmerung"). A part touched by a whole part of
   *  the BILLING is no chance either: Leiston's "Ballet - Notre-Dame de Paris" [2013] bills "Ballet" as a part of its own,
   *  which "La Scala Ballet: Notre-Dame de Paris" holds in its first part — where "About Time" touches "All About My
   *  Mother" by half a part of the billing. */
  private[identity] def touchesPartsWhole(own: Seq[String], title: String, billing: String): Boolean = {
    val words  = own.toSet
    val billed = TitlePartBreak.split(billing).toSeq.map(IdentityMeasures.yearlessTokens(_).toSet).filter(_.nonEmpty)
    TitlePartBreak.split(title).toSeq.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty).forall { part =>
      val touched = part.filter(words)
      // its leading words alone: the venue cut the title short ("Shaun The Sheep : The Beast")
      touched.isEmpty || touched.size * 2 >= part.size || part.takeWhile(words).size == touched.size ||
        billed.exists(_.subsetOf(touched.toSet))
    }
  }
  /** Do the listing's words `own`, all of them the film's `title`'s, cover that title: more than half its words — each of
   *  the billing's own `pieces`, when it bills two or more, read by the title ([[readIn]]) — or every part of it
   *  ([[TitlePartBreak]]) they touch whole — "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE" for "Manon (BALLET LIVE)". Fewer are
   *  words scattered over a longer title (the wrong-match hunt, 10-14, 9 wrong to 1 right): DE "Simply Christmas", a
   *  band's Advent concert, is no "Steve's Simply Happy Christmas Cook-A-Long", "DAS GEHEIME FILMARCHIV", a secret-classic
   *  series, no "Das geheime Filmarchiv der Eva Braun"; nor is a piece whose words the title reads the other way round
   *  another's phrase: UK Watershed's "About Time: Once Upon a Time", an episode of Berger's series, is no "Once Upon a
   *  Time… All About My Mother" (prod 2026-10-14). */
  private[identity] def coversItsTitle(own: Seq[String], pieces: => Seq[Seq[String]], title: String): Boolean = {
    val words = own.toSet
    val ordered = IdentityMeasures.yearlessTokens(title)
    val theirs = ordered.toSet
    (words.size * 2 > theirs.size && (pieces.sizeIs < 2 || pieces.forall(readIn(_, title, ordered)))) || {
      val parts = TitlePartBreak.split(title).toSeq.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty)
      parts.filter(_.exists(words)).forall(_.forall(words))
    }
  }
  /** Does the film's `title` (its words `ordered`) read the billing's `piece`: its words in the piece's order ([[inOrder]]),
   *  or all within one part of the title ([[TitlePartBreak]]), where a translation may order them otherwise — DE Cinema
   *  Wolfhagen's "National Opera Paris: Schwanensee" is "Opéra national de Paris 2024: Schwanensee" (10-15). A piece whose
   *  words two of the title's parts hold the other way round is another's phrase: "About Time" in "Once Upon a Time… All
   *  About My Mother". */
  private[identity] def readIn(piece: Seq[String], title: String, ordered: Seq[String]): Boolean =
    inOrder(piece, ordered) || TitlePartBreak.split(title).iterator.map(IdentityMeasures.yearlessTokens).exists(part => piece.forall(part.contains))
  /** Are `piece`'s words `title`'s in its order, others between them at most? */
  private[identity] def inOrder(piece: Seq[String], title: Seq[String]): Boolean =
    piece.foldLeft(Option(title)) { (rest, word) => rest.map(_.dropWhile(_ != word)).filter(_.nonEmpty).map(_.tail) }.isDefined

  /** Denied only by the probability cut — no learned rule, no pin — while every word of the listing's title is the
   *  film's: the cut is then the title's reading of a reordered banner, not a fact against it. ES Ocine's "Manon
   *  (BALLET LIVE)" ×7 is TMDB's "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE", the one film its search returns, denied
   *  at 2.4% for its original title "Manon" reading as a fragment of that. */
  private[identity] def cutOnly(scored: Scored): Boolean =
    !scored.suggestedOnly && scored.deniedByCutOnly && wordsOfIts(scored)
  private[identity] val MainTitleBreak = java.util.regex.Pattern.compile("""\s+[-–—]\s+|:\s""")
  /** What parts a title: a spaced dash, a colon, a full stop, an ellipsis or a middle dot before a space. */
  private[identity] val TitlePartBreak = java.util.regex.Pattern.compile("""\s+[-–—·]\s+|[:.…]\s""")
  /** The only film one of the listing's own title searches returned, eligible — or denied by the cut alone
   *  ([[cutOnly]]): [[AcceptanceRuleTable.SoleResult]]'s to pick one of. */
  private[identity] def soleResults(weighing: Weighing): Seq[Scored] =
    weighing.ranked.filter(scored => scored.soleResult && (isEligible(scored) || cutOnly(scored)))
  private[identity] def resultsFound(weighing: Weighing): String = s"${weighing.ranked.count(_.rank.isDefined)} film(s) found by its searches"
  private[identity] def namesItsOnlyResult(sole: Scored): Boolean = sole.titleNamesIt || wordsOfIts(sole)
  /** The candidate read as its title's exact top hit, unrivalled — how a rule taking a film by another title (an IMDb
   *  suggestion, a search's only result) has its own facts weighed, the title's evidence granted. */
  private[identity] def asItsTitlesTopHit(scored: Scored): Scored = scored.copy(measures = scored.measures ++ TopHitMeasures)
  private[identity] val TopHitMeasures = Map("title" -> Measure.Category("exact"), "search.rank" -> Measure.Number(1),
    "rivals" -> Measure.Number(0))
  /** The sole result's facts as its title's exact top hit's, its year and running time aside: a search's only film is
   *  taken unless a fact CONTRADICTS it, which the rule asked already. */
  private[identity] def itsTitlesFactsBeside(sole: Scored): Map[String, Measure] =
    asItsTitlesTopHit(sole).measures.filterNot { case (name, _) => name == "runtime.delta" || name == "year.delta" }
  private[identity] def creditsAnotherDirector(sole: Scored): Boolean =
    sole.category("director").contains("different") && !sole.category("title").exists(IdentityMeasures.Rivalling)
  private[identity] def standsForItsTitle(listing: MeasuredListing, sole: Scored): Boolean =
    sole.category("title").exists(IdentityMeasures.Rivalling) || wordsOfIts(sole) || IdentityMeasures.standsForTheWhole(listing, sole.candidate.film)
  private[identity] def namesBeside(sole: Scored, other: Scored): Boolean =
    (other ne sole) && other.titleNamesIt && !(wordsOfIts(sole) && !wordsOfIts(other))

  // ── bills of several works ──────────────────────────────────────────────────────────────────
  /** A DOUBLE PROGRAMME: two quoted works a conjunction joins ([[services.titles.billing.BillingParse.quotesTwoJoined]], PL "„Mieszkanie” i „Owoce
   *  rajskich drzew spożywamy”"), or a "+" joining two whole works ([[MultiFilmBill.billsTwoWholeWorks]]), whatever is held
   *  of them, is neither film (user rule) — UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna
   *  Harrison, Robin Shaw} ×133, its two films scored 91–93% and popularity's coin picking one; PL Kino Pałacowe's
   *  "Historia kina w Popielawach + Pruska kultura", a 1908 short TMDB does not hold. A talk joined to one film
   *  ("+ spotkanie z reżyserem") is no second work, and a film whose own whole title it is ("Romeo + Juliet") is no
   *  bill. So is a programme a word bills as several films ([[MultiFilmBill]]: "Triple Feature: Lord of the Rings",
   *  "The Dark Knight Trilogy", "Maraton Horrorów") — unless an eligible film's own title carries that word ("Marathon
   *  Man"), and then only that film may be taken ([[takesNoBill]]). */
  def billsSeveralWorks(ranked: Seq[Scored]): Boolean = ranked.headOption.exists { any =>
    any.listing.rawTitle.exists(BillingPieces.quotesSeveralDated) || (any.listing.title +: any.listing.rawTitle.toSeq).exists(BillingPieces.quotesTwoJoined) ||
    slashesTwoWorks(any.listing, ranked) || andJoinsTwoWorks(any.listing, ranked) ||
    (MultiFilmBill.billsTwoWorks(any.listing) && !ranked.filter(isEligible).exists(_.category("title").contains("exact")) &&
      MultiFilmBill.billsTwoWholeWorks(any.listing)) ||
      markerOf(ranked).exists(marker => !ranked.filter(isEligible).exists(scored => MultiFilmBill.namedBy(marker, scored.candidate.film)) &&
        billedAlone(ranked).isEmpty)
  }

  /** Does a SLASH join two works — every piece of the title a slash parts the whole title of one candidate, a leading
   *  article aside, and no candidate titled as two pieces? UK Savoy Cinema Corby's "We're Going on a Bear Hunt / Tiger
   *  Who Came to Tea" bills the two shorts as one programme, as the "+" bills do; one film's titles either side of the
   *  slash ("Le Fabuleux Destin d'Amélie Poulain / Die fabelhafte Welt der Amélie") are its own, and a piece naming no
   *  candidate ("/ Baby Friendly") no second work. */
  private[identity] def slashesTwoWorks(listing: MeasuredListing, ranked: Seq[Scored]): Boolean = {
    def slashed(title: String) = {
      val parts  = SlashBreak.split(title).toSeq
      val pieces = parts.map(pieceKey).filter(_.nonEmpty)
      // a format, a screening note or an event after the slash is no second work, whatever film carries its name
      // ("Baranek Shaun i kudłata bestia / dubbing", '"LALKA" / Premiera')
      pieces.sizeIs >= 2 && parts.forall(MultiFilmBill.namesAWork) && {
        val named = pieces.map(piece => ranked.filter(isEligible).filter(_.candidate.film.titles.exists(pieceKey(_) == piece)).map(_.candidate.tmdbId).toSet)
        named.forall(_.nonEmpty) && named.combinations(2).forall { case Seq(a, b) => (a intersect b).isEmpty }
      }
    }
    // read for every rule's every ranking: a title with no slash is split for nothing
    def read(title: String) = title.indexOf('/') >= 0 && slashed(title)
    read(listing.title) || listing.rawTitle.exists(raw => raw != listing.title && read(raw))
  }
  private[identity] val SlashBreak = """\s+/\s+""".r
  /** A title's words, a leading article aside: what a piece of a bill is matched to a film's whole title by. */
  private def pieceKey(text: String): Seq[String] = {
    val words = services.titles.TitleContainment.tokens(text)
    if (words.sizeIs >= 2 && IdentityMeasures.LeadingArticles(words.head)) words.tail else words
  }

  /** Does an "AND" join two works — the title's one "and" parting two pieces of two words or more, each the title of
   *  another candidate (denied or not: a denial says the film is not the BILLING's, not that no film bears the piece's
   *  title), a word of four or more apart at most, and no candidate titled as the whole billing? UK Hyde Park Picture
   *  House's "The Tiger Who Came for Tea and We're Going on a Bear Hunt" bills the two shorts as one programme, as the
   *  "+" and the slash bills do: it took The Tiger Who Came to Tea at 49.6%. A film's own title joining two phrases
   *  ("Willy Wonka and the Chocolate Factory") is its candidate's whole title; a one-word piece ("The Boy and the Heron"
   *  beside a film titled "The Boy") is too short to name a work of its own. */
  private[identity] def andJoinsTwoWorks(listing: MeasuredListing, ranked: Seq[Scored]): Boolean = {
    // read for every rule's every ranking: a title with no "and" is split for nothing
    def read(title: String) = AndBreak.findFirstIn(title).isDefined && joinsTwoWorks(title, ranked)
    read(listing.title) || listing.rawTitle.exists(raw => raw != listing.title && read(raw))
  }
  /** Does `title`'s "and" join two pieces naming a work each, no record titled the whole, each named by records the
   *  other is not? */
  private def joinsTwoWorks(title: String, ranked: Seq[Scored]): Boolean = AndBreak.split(title).toSeq match {
    case Seq(first, second) if Seq(first, second).forall(piece => pieceKey(piece).sizeIs >= 2 && MultiFilmBill.namesAWork(piece)) =>
      val whole = unjoinedKey(title)
      !ranked.exists(_.candidate.film.titles.exists(unjoinedKey(_) == whole)) && {
        val named = Seq(first, second).map(piece => ranked.filter(_.candidate.film.titles.exists(t => nearlyKey(pieceKey(t), pieceKey(piece)))).map(_.candidate.tmdbId).toSet)
        named.forall(_.nonEmpty) && (named.head intersect named.last).isEmpty
      }
    case _ => false
  }
  private def unjoinedKey(text: String): Seq[String] = pieceKey(text).filterNot(_ == "and")
  /** The same words, or as many, four at least, all but one alike. */
  private def nearlyKey(title: Seq[String], piece: Seq[String]): Boolean =
    title == piece || (title.sizeIs == piece.size && piece.sizeIs >= 4 && title.zip(piece).count((a, b) => a != b) == 1)
  private val AndBreak = """(?i)\s+and\s+""".r

  /** The one film a marathon billing one title names ([[MultiFilmBill.billedOne]]): the one eligible candidate titled
   *  exactly so, when no eligible candidate's title extends it — PL "Inna Mamusia - maraton" is Inna mamusia (2026);
   *  "Władca Pierścieni - maraton" bills Jackson's films, which extend Bakshi's "Władca Pierścieni". */
  private[identity] def billedAlone(ranked: Seq[Scored]): Option[Int] =
    ranked.headOption.flatMap(_.listing.billedOne).flatMap { title =>
      val named    = IdentityMeasures.key(title)
      val eligible = ranked.filter(isEligible)
      def keys(scored: Scored) = scored.candidate.film.titles.map(IdentityMeasures.key)
      val extended = eligible.exists(scored => keys(scored).exists(key => key != named && key.startsWith(named)))
      eligible.filter(scored => keys(scored).contains(named)).map(_.candidate.tmdbId).distinct match {
        case Seq(one) if !extended && named.nonEmpty => Some(one)
        case _                                       => None
      }
    }

  /** The word the ranked listing bills several films by, if any ([[MultiFilmBill.marker]]). */
  private[identity] def markerOf(ranked: Seq[Scored]): Option[String] =
    ranked.headOption.flatMap(_.listing.billMarker)

  /** Is `taken` no film of a programme the listing bills by a word — the word its own title's, if the listing has one? */
  private[identity] def takesNoBill(ranked: Seq[Scored])(taken: (Accepted, String)): Boolean =
    markerOf(ranked).forall(marker => MultiFilmBill.namedBy(marker, taken._1._1.candidate.film) ||
      billedAlone(ranked).contains(taken._1._1.candidate.tmdbId))

  // ── what the vetoes ask ─────────────────────────────────────────────────────────────────────

  /** A film the billing names by one piece while ANOTHER piece, sharing no word with the record's titles, bills a
   *  performance ([[NonMovieEventClassifier.isPerformance]]: a tribute, a concert, a stage show): the evening is the show,
   *  named after the film's title, and whatever record the venue's system attached to it is the namesake its search
   *  found. UK Electric Palace Hastings' "Straight Talk: A Tribute to Dolly Parton" [1992] {Barnet Kellman} is a tribute
   *  show named after her song, not the 1992 film she starred in. Only the event the billing bills is read
   *  ([[FilmNess.eventBilled]]): a billing attaching a screening ("+ film", "pokaz", a festival's "Closing
   *  Film + Awards") keeps its film, and a performance "joined with" the film is its companion ("DYRYGENT - POŁĄCZONY Z
   *  KONCERTEM MUZYKI NA ŻYWO", Wajda's film). A record titled by the performance itself ("Kabaret", "Die Tribute von
   *  Panem") is the film its event words name. A film of the silent era is screened to a performance — live music is
   *  its accompaniment (PL Kino Wisła's "PANTEON - Gabinet Doktora Caligari | muzyka na żywo", Wiene's 1920 film). */
  private[identity] def performedBeside(scored: Scored): Option[String] = {
    val film      = scored.candidate.film
    val pieces    = if (film.year.exists(_ <= ListingShape.SilentEraEnd)) Nil else scored.listing.performancePieces
    lazy val filmWords = film.titles.flatMap(IdentityMeasures.countedWords).toSet
    val performed =
      if (pieces.isEmpty || !pieces.exists(_._2.forall(filmWords))) Nil
      else pieces.collect { case (piece, words, true) if !words.exists(filmWords) => piece }
    Option.when(performed.nonEmpty && !film.titles.exists(NonMovieEventClassifier.isPerformance(_)))(
      s"${Acceptance.named(scored)} beside: ${performed.mkString(", ")}")
  }

  /** The staged show's OWN record for a billing naming a staged show ([[ListingShape.showsOwnRecord]]): a pantomime is an
   *  event, whatever facts its billing shares with the record TMDB files of it. Held here, where a listing takes a film
   *  ALONE, and not only after the assignment ([[BilledPiece]]): a listing holding the record would join no other spelling
   *  of the event (`ConstraintEdges`' same-event must-link joins two holding no film). UK, prod 2026-11-13: 15 "Cbeebies
   *  Panto: Treasure Island" billings (Lonsdale Alhambra Penrith, BEAM Hertford, Showcase's sensory-friendly ones) took
   *  1785679 by favoured-calibrated, then lost it to the billed-as-one-piece veto alone. */
  private[identity] def stagedShowsOwn(scored: Scored): Boolean =
    ListingShape.showsOwnRecord(scored.listing.rawTitle.getOrElse(scored.listing.title), scored.candidate.film)

  /** The older namesake of a work a billing credits to its author ([[ListingShape.creditDenies]]): DE Cineplex's "Fallen
   *  Angels by Noël Coward", the 2026 capture of Coward's play, is not Wong Kar-wai's 1995 "Fallen Angels". A refusal, not
   *  a denial: the billing's siblings stay free to join the namesake's listings (a denial's cannot-link split UK's
   *  92-venue cluster of the capture, losing 16 of its takes). Asked of every accepted take, so a billing with no
   *  authorship word allocates nothing ([[ResolverAllocationSpec]]'s dated-title budget). */
  private[identity] def authorsNamesake(scored: Scored): Option[String] = {
    val listing = scored.listing
    Credits.authorCredited(listing.rawTitle.getOrElse(listing.title))
      .filter(credit => ListingShape.creditDenies(credit, scored.candidate.film, listing.dating.screeningsSeason))
      .map(credit => s"${Acceptance.named(scored)}: ${credit.work} by ${credit.person}")
  }

  /** A billing that reads as no film at all ([[FilmNess]]: event words — a gig, a tour, a festival, a show — and no relay
   *  or screening attached) whose record shares none of its other words: the namesake a search for the event's words
   *  found, not what the venue screens. The agreement stage already asks no family about such a billing
   *  ([[NonFilmEvents]]); this holds the model's own takes to the same reading. Asked of every accepted take,
   *  so a billing with no event word answers from the listing's one-off [[MeasuredListing.eventWords]] and
   *  allocates nothing ([[ResolverAllocationSpec]]'s dated-title budget). */
  private[identity] def readsAsNoFilm(scored: Scored): Boolean = {
    val listing = scored.listing
    val film    = scored.candidate.film
    // V2: an event-worded billing whose record shares none of its other words — found by the event's words alone
    // V3: V2 alone (V1's NonFilmEvents reading dropped), and only where the billing has words beside the event's
    listing.eventWords.nonEmpty && listing.nonEventWords.nonEmpty &&
      // V3d: the record's own and original title only — an alternative title TMDB files under the gig's own billing
      // (1419814 carries "Tanzwut - Live Tour 2026 | Special Guest: Sagenbringer") is the reused record's echo, not its name
      !(film.title +: film.originalTitle.toSeq).exists(t => IdentityMeasures.countedWords(t).exists(listing.nonEventWords)) &&
      // V3e: a record in another script is named by its alternatives (Cinema City's "ENHYPEN WORLD TOUR 'BLOOD SAGA'"), and a
      // year the billing states that the record shares is a film's fact ("Silent Night, Bloody Night (1972)")
      !(film.title.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c) != Character.UnicodeScript.LATIN) &&
        film.titles.exists(t => IdentityMeasures.countedWords(t).exists(listing.nonEventWords))) &&
      !listing.statedYear.exists(year => film.year.exists(filmYear => math.abs(filmYear - year) <= 1))
  }
  private[identity] def onlyTheEventsWords(scored: Scored): String =
    s"${Acceptance.named(scored)}: its record shares only the event's words (${scored.listing.eventWords.mkString(", ")})"

  /** Any film while the listing's page reads as REUSED: a record its ORIGINAL title and the director it credits name
   *  stands in its pool ([[copiedBeyondItsBilling]]) though the billing says more than that record's titles. The copied
   *  title is what a search for the billing hit, copied onto a ticket page with the record's director, running time and
   *  year: they agree with the record by construction and confirm nothing, and the billing's other words name something
   *  else — a party, a band, a show the page was reused for. DE kinoheld: Kaminwerk's "Teenie Fasching" on Schweikart's
   *  "Fasching" (1939), "Dark New Wave Night" on Ai's "New Wave", Lagerhalle's "Three for Silver" (a folk band) on Kamal's
   *  "Tea For Three", KAP-Torgau's "VOODOO LOUNGE" (a Rolling Stones tribute) on Costabile's "VooDoo". Nor does such a
   *  page lend another film its take: only one the billing names whole (an exact, original or alternative title) may
   *  still be taken — Landsberg's "Helmut Schleich" (a Kabarett evening, on Kuzmic's "Helmut") is no more the 2018
   *  recording of his "Kauf, du Sau" the bare name leads. */
  private[identity] def reusedPage(weighing: Weighing, scored: Scored): Option[String] = {
    def namedWhole = scored.category("title").exists(Set("exact", "original", "alternative"))
    weighing.eligible.find(copiedBeyondItsBilling).filter(copied => (copied eq scored) || !namedWhole)
      .map(copied => s"${Acceptance.named(copied)} billed beyond: ${billedBeyond(copied).mkString(", ")}")
  }

  /** Is `scored` named only by the listing's ORIGINAL title and the director it credits, that title a mere part of the
   *  billing ([[MeasuredListing.originalStandsApart]] false), with the billing saying more than any of the
   *  record's titles ([[billedBeyond]]) and stating no year of it? A billing the record's titles hold word for word
   *  ("Matrix" of "The Matrix", or a translation TMDB holds as an alternative title) is its own, and a year the listing
   *  states that the record shares makes the billing another record's spelling of it, as the copied facts' other reader
   *  holds (`CopiedTitleDenial.namedOnlyByCopiedTitle`: DE "Tabaluga und Leo - Die Weihnachtsgeschichte" [2003]). The title
   *  relation cannot tell: it reads the original title as one of the listing's own shapes, a `segment` of every record
   *  so titled. */
  private[identity] def copiedBeyondItsBilling(scored: Scored): Boolean = {
    val listing = scored.listing
    scored.category("originalTitle").contains("match") && IdentityMeasures.sameDirector(scored.measures) && !listing.originalStandsApart &&
      !listing.year.exists(year => scored.candidate.film.year.exists(film => math.abs(film - year) <= 1)) && billedBeyond(scored).nonEmpty
  }
  /** The billing's words none of the record's titles carries — none when a whole piece of the billing, or the search its
   *  title rules leave of it, is the record's word for word ("Twisters" of "(4DX Rewind) Twisters", "Endgame" of
   *  "Vengadores: Endgame (Reestreno)", "Moje własne Idaho" of "… + DEBATA [POKAZ BEZPŁATNY]"): the rest decorates it. */
  private[identity] def billedBeyond(scored: Scored): Seq[String] = {
    val listing   = scored.listing
    val filmWords = scored.candidate.film.titles.flatMap(IdentityMeasures.countedWords).toSet
    val pieces    = IdentityMeasures.shapes(listing.ownTitles ++ listing.searchTitles, listing.decorations).map(IdentityMeasures.countedWords).filter(_.nonEmpty)
    if (pieces.exists(_.forall(filmWords))) Nil else IdentityMeasures.countedWords(listing.title).filterNot(filmWords)
  }
}
