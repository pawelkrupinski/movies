package services.identity

import services.identity.AcceptanceRule.{Weighing, isEligible}
import services.identity.measures.Scored.Accepted
import services.identity.measures.{Film, IdentityMeasures, Measure, MeasuredListing, Scored}

/** What the acceptance rules that take a film by how its title is searched and billed ask: the exact and segment top
 *  hits, IMDb's suggestion, and the sole result ([[AcceptanceTitleRows]]); and the EDITION of a taken film the listing's
 *  whole title names ([[editionNamed]]). What makes a listing a bill of several works is [[AcceptanceBillRules]]', what
 *  the vetoes ask [[AcceptanceVetoRules]]'.
 *  Functions of the candidates and the listing they score; [[AcceptanceWiring]] makes the one the rules ask. */
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

  // ── the edition a take names ─────────────────────────────────────────────────────────────

  /** The EDITION of the accepted film that the listing's whole title names, when there is exactly
   *  one: a later record carrying the film's title under a qualifier (`IdentityMeasures.editionOf`
   *  — "Radiohead X Nosferatu: A Symphony of Horror" of Murnau's "Nosferatu"), which the listing
   *  names by its whole title MORE as its own than it names the film ([[namedAs]]). The
   *  listing's facts chose the work, and an edition carries its work's facts — the venue credits
   *  Murnau, TMDB the edition's maker — so they do not deny the edition; a pin still does. With the
   *  confidence of the work. The film itself when the listing names it as its own — by its title
   *  or its own original title — or no edition, or two.
   *
   *  A work the title names only by one of its ALTERNATIVE titles, while another record carries
   *  that title as its own, is named as closely by the title as that record: TMDB files "Caligula:
   *  The Ultimate Cut" among the 1979 "Caligula"'s alternatives beside the re-cut's own record, and
   *  "Nosferatu: A Symphony of Horror" among Murnau's beside David Lee Fisher's 2023 remake. Then
   *  the record is an edition by the work's OWN titles, and the listing's credit decides: a record
   *  crediting another person than the listing's is another film, one crediting nobody else is the
   *  work's edition — its running time is the cut's own. */
  private[identity] def editionNamed(ranked: Seq[Scored])(accepted: Accepted): Accepted = {
    val (work, confidence) = accepted
    val named = namedAs(work.category("title"))
    // The listing's own original title naming the work whole names it too ("Die Puppe", "Lalka").
    if (named == NamedAsItsOwn || work.category("originalTitle").contains("match")) accepted
    else ranked.filter(edition => (edition ne work) && !edition.deniedByPin && namesTheEdition(work, named)(edition)) match {
      case Seq(edition) => edition -> confidence
      case _            => accepted
    }
  }
  /** Does the listing's title, naming `work` as `named` ([[namedAs]]), name `edition` more as its own, the
   *  record an edition of the work ([[editionNamed]]): by any of the work's titles where the title names it by none of
   *  them, else by the work's own titles alone and crediting no other person than the listing's? */
  private def namesTheEdition(work: Scored, named: Int)(edition: Scored): Boolean =
    namedAs(edition.category("title")) > named && (
      if (named == 0) IdentityMeasures.editionOf(edition.candidate.film, work.candidate.film)
      else IdentityMeasures.editionOf(edition.candidate.film, work.candidate.film.copy(alternativeTitles = Nil)) &&
        !edition.category("director").contains("different"))

  /** How much as ITS OWN a title relation names a record: by the record's own title or original
   *  title ([[NamedAsItsOwn]]), by one of the alternative titles the database files beside them,
   *  or not whole (0). */
  private[identity] val NamedAsItsOwn = 2
  private[identity] def namedAs(titleRelation: Option[String]): Int = titleRelation match {
    case Some("exact") | Some("original") => NamedAsItsOwn
    case Some("alternative")              => 1
    case _                                => 0
  }
}
