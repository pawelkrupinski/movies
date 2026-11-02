package services.identity

import services.identity.Scored.Accepted

/** The acceptance rules that take a film by how its title is searched and billed: the exact and segment top hits,
 *  IMDb's suggestion, and the sole result.
 *  Part of [[Acceptance]], which mixes it in. */
private[identity] trait AcceptanceTitleRules { this: Acceptance =>
  import Acceptance.Refused

  /** The listing's EXACT TOP HIT, accepted on what the calibration measured for its evidence as a
   *  CLASS (`IdentityCalibration.classProbability`): the one film its whole title names exactly
   *  that its own title search returned first, in TMDB's order ([[IdentityMeasures.exactTopHits]]),
   *  when the listing's own evidence rules it out on nothing, published nothing against it, and
   *  gives no rival a better fit ([[EvidenceWeights.fitsBetter]]). A bare title credits it with the
   *  naive-Bayes sum of missing facts, a low popularity and its same-titled rivals, which undersells
   *  what the class measured on the labels; here the search standing LENDS confidence and never
   *  withdraws it. */
  private[identity] def topHit(ranked: Seq[Scored]): Option[Accepted] = ownInstalment(topHitWhy(ranked)).toOption
  private[identity] def topHitWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any  <- firstCandidate(ranked)
      id   <- IdentityMeasures.exactTopHits(any.listing, ranked.map(scored => (scored.candidate.tmdbId, scored.candidate.film, scored.rank))) match {
                case Seq(only) => Right(only)
                case Seq()     => Left(Refused("no film its whole title names is its search's first hit"))
                case ids       => Left(Refused("two exact top hits", None, ids.mkString("; ")))
              }
      best <- eligible.find(_.candidate.tmdbId == id).toRight(Refused("its exact top hit is denied", Some(id),
                ranked.find(_.candidate.tmdbId == id).flatMap(_.denial).fold("")(_.why)))
      taken <- unopposed(best, best, eligible)
    } yield taken
  }

  /** A programme listing's EXACT TOP HIT once its banner is off: the one candidate whose title is a whole
   *  delimited piece of the listing's (`segment`) and which the listing's own search returned FIRST, when
   *  the title names no other candidate at all (the rest is a banner, not a second film), it bills no two
   *  works, and nothing it publishes speaks against it or fits a rival better. The piece IS a bare title,
   *  so it is credited with what the exact-top-hit class measured for bare titles. PL's "DZIEŃ KINA POLSKIEGO:
   *  Przepraszam, czy tu biją" and ~30 programme listings like it sat at 28.9% with no rule to take them. */
  private[identity] def segmentTopHitWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any    <- firstCandidate(ranked)
      // a one-word piece names no film here, rival or taken: "Inna Mamusia - maraton" is no "Maraton"
      scored <- one(ranked.filter(scored => scored.titleNamesIt && (scored.category("title").exists(IdentityMeasures.Rivalling) ||
                  IdentityMeasures.standsForTheWhole(any.listing, scored.candidate.film))), "the title names no film", "the title names two films")
      _      <- needOf(!scored.denied, "the film it names is denied", scored, scored.denial.fold("")(_.why))
      _      <- needOf(scored.rank.contains(1), "not its search's first hit", scored, scored.rank.fold("not ranked")(rank => s"ranked $rank"))
      _      <- needOf(scored.category("title").contains("segment"), "not named by a piece of the title", scored,
                  s"title=${scored.category("title").getOrElse("absent")}")
      _      <- notADoubleBill(any.listing)
      _      <- needOf(IdentityMeasures.standsForTheWhole(any.listing, scored.candidate.film), "its piece cannot stand for the whole title", scored)
      _      <- notJoinedToAnotherWork(any.listing, scored)
      // a year the title states is the record's: "Disney Junior Cinema Club 2026" is not the 2024 edition
      _      <- needOf(scored.number("titleYear.delta").forall(FactRelations.nearDelta), "the title dates another year",
                  scored, scored.number("titleYear.delta").fold("")(delta => f"titleYear.delta=$delta%.0f"))
      // Read as the bare title its piece is: the banner beside it is no evidence against the film.
      bare    = scored.copy(measures = scored.measures + ("title" -> IdentityMeasures.Category("exact")))
      taken  <- unopposed(scored, bare, eligible)
    } yield taken
  }

  /** The film IMDb's suggestions for the listing's own title name, on the old pipeline's three rungs — the route
   *  that reaches a film whose local title TMDB lacks, since IMDb matches a query against a film's
   *  other-language titles ("Superfutrzak i złośliwa wiewiórka" suggests only the Finnish "Supermarsu ja suuri
   *  huijaus"): IMDb's FIRST suggestion with the year the listing states; the ONE suggested film the listing's
   *  credited director directed; or the ONLY film IMDb suggests, when the title names it and TMDB ranks no
   *  other film of that very title above it ("Ziemia obiecana" is Wajda's, not the 1927 film IMDb alone spells
   *  so); or the ONE film IMDb lists under one of the listing's search titles in some language — an AKA TMDB does
   *  not carry ("Camino dla opornych" is IMDb's Polish title of "Compostelle"). Read as a title the film answers to,
   *  nothing the listing publishes may speak against it, no rival may fit better, and a double bill is neither film. */
  private[identity] def imdbSuggestedWhy(ranked: Seq[Scored]): Verdict = firstCandidate(ranked).flatMap { any =>
    val eligible  = ranked.filterNot(_.denied)
    val suggested = eligible.filter(scored => scored.imdb.isDefined || scored.imdbTitled.nonEmpty)
    // the film's own title or its original — not an alternative TMDB files it under ("Lumière" is not "Café Lumière")
    def exact(scored: Scored)        = scored.category("title").exists(Set("exact", "original"))
    def outranked(scored: Scored) = eligible.exists(rival => (rival ne scored) && rival.category("title").contains("exact") &&
      rival.rank.exists(r => scored.rank.forall(r < _)))
    // the ONE suggestion IMDb lists under the listing's own title in some language (an AKA TMDB does not carry):
    // "Camino dla opornych" is IMDb's Polish title of TMDB's French "Compostelle". Only while nothing else the
    // listing names stands beside it: no other record carrying its title, though denied (Renoir 2025 beside the 2012
    // film IMDb also lists as "Renoir"; the 1951 "Streetcar" beside the 1989 TV film), no year the title dates against
    // it ("Miłość 2024" is not Haneke's 2012 film), no numbered set ("Bolek i Lolek – zestaw IV"), a title of words,
    // not a number ("2026"), and no stage work broadcast as a non-season film (the Met's "Così fan tutte").
    def titledRung(scored: Scored): Boolean =
      IdentityMeasures.soleImdbTitled(ranked.filter(_.imdbTitled.nonEmpty).map(s => s.candidate.tmdbId -> s.imdbTitled).toMap)
        .exists(_._1 == scored.candidate.tmdbId) &&
        !ranked.exists(other => (other ne scored) && other.category("title").exists(IdentityMeasures.TitlesItsOwn)) &&
        IdentityMeasures.takesImdbTitle(scored.listing, scored.candidate.film) && titledByMoreThanAWord(scored)
    // the billing's whole title, a piece of two words at least, the word the billing opens with, or a DATED record: a
    // one-word piece after another names an undated record only as many films' title does — DE "Friday the 13th:
    // Legacy", a fan film, is not David Slade's undated "Legacy" IMDb lists under its last word, while "Spooked: Ringu"
    // is the 1998 "Ring" and "Gina (Kurzfilm)" is "Gina"
    def titledByMoreThanAWord(scored: Scored): Boolean = scored.candidate.film.year.isDefined || scored.imdbTitled.exists { title =>
      val words = IdentityMeasures.words(title)
      scored.listing.ownKeys(IdentityMeasures.key(title)) || words.sizeIs >= 2 ||
        scored.listing.ownForms.exists(_.words.startsWith(words))
    }
    def rung(scored: Scored): Boolean = titledRung(scored) || scored.imdb.exists { place =>
      (place.place == 1 && scored.number("year.delta").contains(0.0)) ||
        (scored.ownCredit && suggested.count(_.ownCredit) == 1) ||
        (place.of == 1 && scored.titleNamesIt && !outranked(scored)) ||
        // the only film IMDb suggests, its title the billing's but for a venue's slip of letters in a word or two (UK
        // "The Hunger Games Sunrise of the Reaping" ×25, DE "LA VIE DE CHÂTEAU, MON ENFACE À VERSEILLES"): TMDB's
        // search finds nothing under the slip, and only IMDb's forgiving one reaches the film
        (place.of == 1 && slippedTitle(scored.listing, scored.candidate.film) && !outranked(scored)) ||
        // the old pipeline's yearless rung: IMDb's FIRST suggestion, and the only suggested film the title is ("Kroll")
        (place.place == 1 && exact(scored) && suggested.count(exact) == 1 && !outranked(scored))
    }
    // A fallback, as the old pipeline's was: IMDb answers only when TMDB's own search ranks no OTHER film the title
    // names above it ("BTS 'ARIRANG' IN SÃO PAULO" is TMDB's São Paulo record, not IMDb's first 2026 suggestion, Busan;
    // "Kroll" is TMDB's first, the 1991 film IMDb suggests, above 1972's "Krõll"), and
    // never for an instalment the title numbers otherwise ("Recepta na szczęście 2" is not the first film).
    def searchNamesAnother(scored: Scored, rival: Scored) = (rival ne scored) && rival.titleNamesIt &&
      rival.rank.exists(r => scored.rank.forall(r < _))
    for {
      _      <- need(suggested.nonEmpty, "IMDb suggests none of its candidates")
      _      <- notADoubleBill(any.listing)
      scored <- one(suggested.filter(rung), "no IMDb suggestion is taken by its year, its director or as the only one", "two IMDb suggestions qualify")
      _      <- notJoinedToAnotherWork(any.listing, scored)
      _      <- needOf(!alternativeAgainstTmdbsSearch(scored, ranked), "a bare billing names the film only by an alternative title, " +
                  "and TMDB's search of it finds other films, not this one", scored, s"title=${scored.category("title").getOrElse("absent")}")
      _      <- noneOf(eligible.find(searchNamesAnother(scored, _)), "TMDB ranks another film the title names above it", scored)
      _      <- notAnotherInstalment(scored)
      taken  <- unopposed(scored, asItsTitlesTopHit(scored), eligible)
    } yield taken
  }

  /** Is one of the listing's titles the film's own or original title, word for word, but for a SLIP in at most two
   *  words — never more than half of them, of four words or more: each slipped pair words of letters, no numeral
   *  ("Part II", "2"), at most one letter edit apart per four letters (one in a short word: "of" for "on", "Enface" for
   *  "Enfance", "Caspar" for "Casper")? A slip a typist makes, not another film's title: "A Boy and His Dog" is no slip
   *  of "A Boy and Her Dog" (two edits in a three-letter word), "Part One" none of "Part Two". */
  private[identity] def slippedTitle(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film): Boolean = {
    def letters(word: String) = word.forall(_.isLetter) && !IdentityMeasures.numbersAnInstalment(word)
    def slip(a: String, b: String) = letters(a) && letters(b) &&
      tools.EditDistance.within(a, b, math.max(1, math.min(a.length, b.length) / 4))
    val billed = (listing.ownTitles ++ listing.searchTitles).map(services.movies.TitleContainment.tokens).distinct
    val titles = (film.title +: film.originalTitle.toSeq).map(services.movies.TitleContainment.tokens).distinct
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
  /** The ONLY film one of the listing's own title searches returned — the old pipeline's `searchUnique`, which
   *  took it unless a fact CONTRADICTED it: "Loving Karma" [78′] is TMDB's 85-minute record, the one "Loving
   *  Karma" finds, though seven minutes weigh against it in the probability. The title must name it — a piece
   *  standing for the whole ([[IdentityMeasures.standsForTheWhole]]: not "Bhutan" of "Bhutan – Trails of
   *  Happiness") — no other candidate the title names may stand beside it, and a double bill is neither film. */
  private[identity] def soleResultWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any   <- firstCandidate(ranked)
      sole  <- one(ranked.filter(scored => scored.soleResult && (isEligible(scored) || cutOnly(scored))),
                 "no search of its returned a single film", "its searches returned different single films",
                 s"${ranked.count(_.rank.isDefined)} film(s) found by its searches")
      _     <- needOf(sole.titleNamesIt || wordsOfIts(sole), "the title does not name its search's only film", sole,
                 s"title=${sole.category("title").getOrElse("absent")}")
      _     <- needOf(!contradicted(sole), "a published fact contradicts it", sole, contradiction(sole))
      // a director the venue credits is the listing's own fact: a search's only film directed by another, that the title
      // names only in part, is no answer (PL "Carmen" [2026] {Richard Eyre} and "Carmen: Salzburger Festspiele 2026") —
      // a film carrying the very title still is (Kino Parczew's "Tedi i magiczna lampa" credits its co-directors)
      _     <- needOf(!sole.category("director").contains("different") || sole.category("title").exists(IdentityMeasures.Rivalling),
                 "it credits another director", sole)
      _     <- notADoubleBill(any.listing)
      _     <- notJoinedToAnotherWork(any.listing, sole)
      _     <- needOf(sole.category("title").exists(IdentityMeasures.Rivalling) || wordsOfIts(sole) ||
                   IdentityMeasures.standsForTheWhole(any.listing, sole.candidate.film),
                 "its piece cannot stand for the whole title", sole)
      // a film the title names by fewer of its words is no rival to one carrying them all ("Manon" beside "BALLET LIVE.
      // MANON. ROYAL ÓPERA HOUSE" for "Manon (BALLET LIVE)")
      _     <- noneOf(eligible.find(other => (other ne sole) && other.titleNamesIt && !(wordsOfIts(sole) && !wordsOfIts(other))),
                 "the title names another film", sole)
      _     <- notAnotherInstalment(sole)
      titled = asItsTitlesTopHit(sole)
      taken <- classAccepted(sole, titled.measures.filterNot { case (name, _) => name == "runtime.delta" || name == "year.delta" })
    } yield taken
  }
}
