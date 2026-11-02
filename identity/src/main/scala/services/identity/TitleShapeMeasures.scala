package services.identity

import services.billing.{BillingParse, DecorationWords, NumberedTitles}
import services.movies.TitleContainment
import services.resolution.SearchTitles

import IdentityMeasures.*

/** The parts a title numbers, its capitalised tags, and the SHAPES it is billed in — the titles a listing's billing
 *  can stand for, delimited, quoted, or with its screening year dropped.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object TitleShapeMeasures {
  /** A part a title numbers by name — "Dune: Part Three", "Teil 2", "Episode VII" — not a bare number beside it. */
  private val NamedPart = s"""(?iU)\\b(?:${NumberedTitles.PartWord}|${NumberedTitles.EpisodeWord})\\s*${NumberedTitles.PartNumber}""".r
  /** The parts the listing's own titles number and subtitle ([[BillingParse.titledParts]]): each one's number, if it has
   *  one, and its subtitle's words of four letters or more. */
  private[identity] def titledParts(titles: Seq[String]): Seq[(Option[String], Seq[String])] =
    titles.flatMap(BillingParse(_).titledParts).filter(_._2.nonEmpty).distinct

  /** Does the listing's title give the part it numbers a subtitle ([[titledParts]]) not one word of which is a word
   *  of the film's titles? Then it is another film's part: DE Union Filmtheater Friedrichshagen's "DAS INDISCHE GRABMAL
   *  (TEIL 1, DIE SENDUNG DES YOGHI)" is Joe May's 1921 first part, not Lang's 1959 "Das indische Grabmal", and DE Acud
   *  Kino's "LOVE ISLAND - EPISODE NATURE" an episode of a series, not the 1952 "Love Island". A part
   *  numbered with no subtitle ("SHOAH (TEIL I)"), titled as the film is ("De Gaulle Part Two: Liberté") or numbered
   *  as the film's titles name a part ("Dune: Part Three - ERLY FAN PREVIEW") is not — a bare number among them is no
   *  part named: Lang's 1959 film also goes by "2 - Das indische Grabmal", and is no "TEIL 2, DER TIGER VON ESCHNAPUR". */
  def titlesAnotherPart(l: Listing, f: Film): Boolean = {
    val parts = titledParts(l.ownTitles)
    // a film its titles name as that part is that part, whatever note follows it: "Dune: Part Three - ERLY FAN PREVIEW"
    lazy val numbered = f.titles.flatMap(t => NamedPart.findAllMatchIn(t).flatMap(m => TitleContainment.tokens(m.group(1)).headOption.map(NumberedTitles.partNumber))).toSet
    parts.nonEmpty && parts.forall { case (number, words) => !number.exists(numbered) && !words.exists(f.titleWords) }
  }

  /** Does the listing number an instalment ([[BillingParse.instalment]]: "Der Herr der Ringe 2", "Terminator 2 (4K)") whose
   *  subtitle its original title gives ("The Lord of the Rings: The Two Towers – Extended"), while not one word of that
   *  subtitle is a word of the film's titles,
   *  alternative titles included, and none of the film's own titles numbers that instalment? Then it is another
   *  instalment of the series: DE Kinopolis' "Der Herr der Ringe 2 (Extended Version)" is Jackson's "The Two Towers", not
   *  Bakshi's 1978 "Der Herr der Ringe". A series whose name spells its number ("Ocean's Eleven", "The Fantastic Four:
   *  First Steps") is no instalment; a version tag ("– Extended", ": Director's Cut") or a part number ("…: Part 1") is
   *  no subtitle; an original title that only repeats the billing gives none of its own. */
  def numbersAnotherSubtitle(l: Listing, f: Film): Boolean = {
    val numbers = l.ownTitles.flatMap(BillingParse(_).instalment).toSet
    // the ORIGINAL title's subtitle alone: a billing's own ("HOBBIT 2: SMAUGS EINÖDE (EXTENDED)") is the venue's language,
    // which a record of the film in another ("The Hobbit: The Desolation of Smaug", IMDb's) need not hold
    lazy val original = l.originalTitle.filterNot(o => l.ownTitles.exists(t => key(t) == key(o)))
    lazy val subtitles = original.flatMap(BillingParse(_).subtitle).filterNot(_.seriesSpellsNumber).map(_.words)
    // the film's OWN titles: an alternative one numbers whatever a catalogue did — Bakshi's 1978 film is also "El Señor De
    // Los Anillos (Animación) 2 - El Señor De Los Anillos"
    numbers.nonEmpty && subtitles.nonEmpty && !f.ownTitles.exists(t => numbered(t).numbers.exists(numbers)) &&
      subtitles.forall(words => !words.exists(f.titleWords))
  }

  /** Is a listing's credited "director" the film's HOUSE — a name two words or more long that runs inside the film's
   *  own title, and none of the people the film credits? UK venues credit "The Metropolitan Opera" for its 2026/27
   *  "The Metropolitan Opera: Così fan tutte" ×97, which read as a different director than Phelim McDermott and
   *  vetoed the film. "Guillermo del Toro" of "Guillermo del Toro's Pinocchio" directed it, and stays a person. */
  private[identity] def namesItsHouse(name: String, words: Seq[String], f: Film): Boolean =
    words.sizeIs >= 2 && f.ownTitleTokens.exists(_.containsSlice(words)) &&
      !f.directorCredits.exists(CreditRelation.onePerson(new CreditRelation.Credits(Seq(name)), _))

  /** Does the listing credit one of the film's directors ([[CreditRelation.onePerson]])? */
  private[identity] def creditsItsDirector(l: Listing, f: Film): Boolean = f.directorCredits.exists(CreditRelation.onePerson(l.directorCredits, _))

  /** The pieces of a listing's title AS BILLED ([[shapes]]) that are written in capitals where the title is not — the
   *  case change a venue's programme tag makes beside a work's own spelling ("Alim | UFF", "DKF: Lalka"). None in a title
   *  billed all in capitals ("BEZ KOŃCA 2D PL LOLO"): its case says nothing. */
  def capitalisedTags(l: Listing): Seq[String] = {
    val billed = l.rawTitle.getOrElse(l.title)
    if (!billed.exists(_.isLower)) Nil
    else l.shapesOfOwn(billed).filter(piece => piece != billed.trim && piece.exists(_.isLetter) && !piece.exists(_.isLower))
  }

  private[identity] def shapesOf(l: Listing): Seq[String] = {
    shapes(l.ownTitles ++ l.searchTitles ++ SearchTitles.candidates(l.title, l.originalTitle) ++
      l.rawTitle.toSeq.flatMap(SearchTitles.candidates(_, None)) ++ l.ownTitles.flatMap(creditedWork), l.decorations)
  }

  /** The pieces a billing JOINS to its film with a "+" that name only an event ([[namesOnlyAnEvent]]): "Q&A" of "The
   *  Assassin + Q&A", "Interval" of "The Odyssey + Interval". Only a joined piece: a banner's own words ("HALLOWEEN -
   *  SPECIAL - …") are its programme's. Never one of the listing's own titles. A film such a piece alone names is no
   *  rule's take ([[eventNamed]]). */
  private[identity] def joinedEvents(l: Listing): Set[String] =
    if (!l.ownTitles.exists(_.indexOf('+') >= 0)) Set.empty
    else {
      val own = l.ownTitles.map(_.trim).toSet
      l.ownTitles.flatMap(t => services.billing.BillJoins.billPieces(t).drop(1).map(_.trim)).filter(p => !own(p) && namesOnlyAnEvent(p)).toSet
    }

  /** Does the listing name `f` ONLY by a piece its billing joins as an event ([[joinedEvents]]) — "Q & A" (1990) of UK
   *  "The Assassin + Q&A", which its "Q&A" searched? Such a film stays in the pool as a rival (the namesakes a bare
   *  billing cannot tell apart are still left to no pooled vote: "Amazing Grace + Q&A" is Ruth Carney's, not Apted's),
   *  but no rule takes it ("Inside Gaza + Q&A" ×1, "Le Moulin + Q&A": prod 2026-11-01) nor does it stand as the runner-up
   *  a film's own facts must outweigh ("The Assassin + Q&A", "My Daughter Is a Zombie + Q&A"). */
  private[identity] def eventNamed(l: Listing, f: Film): Boolean = {
    val joined = joinedEvents(l)
    joined.nonEmpty && {
      val pieces = namingPieces(l, f)
      val events = joined.map(TitleContainment.tokens)
      pieces.nonEmpty && pieces.forall(piece => events.exists(event => event == piece || TitleContainment.isTokenRun(piece, event)))
    }
  }

  /** Does a piece of a billing name nothing but an event around a screening — a talk, a panel, an interval, an
   *  introduction ([[DecorationWords.namesAnEvent]]), every other word of it one a venue bills around a film
   *  ([[DecorationWords.billedAround]]): "Q&A", "Q+A", "Panel", "Interval", "Live Q&A"? No film's title, whatever film a
   *  database files under the word. */
  private[identity] def namesOnlyAnEvent(piece: String): Boolean = {
    val all   = TitleContainment.tokens(piece)
    val words = all.filter(_.length > 1)
    namesAnEvent(all) && words.forall(word => DecorationWords.billedAround(word) || SideEventWords(word))
  }
  /** Do the folded `words` of a piece name an event around a screening — one the billing grammar knows
   *  ([[DecorationWords.namesAnEvent]]: a talk, a premiere, a festival) or one a bill joins after a film (a panel, an
   *  interval, a reception: the word table's side events)? */
  private[identity] def namesAnEvent(words: Seq[String]): Boolean =
    DecorationWords.namesAnEvent(words) || words.exists(SideEventWords)
  private val SideEventWords: Set[String] = services.movies.TitleWords.tokens(services.movies.TitleWords.Concept.SideEvent)

  /** `titles` and every part a split leaves, each de-decorated in turn ("Throwback: Donnie Darko
   *  (25th Anniversary)" → "Donnie Darko (25th Anniversary)" → "Donnie Darko"; "(4DX Rewind) Shrek"
   *  → "Shrek" by a learned `decorations` run) and cut before a bracketed year ("Człowiek z żelaza
   *  (1981) 4K" → "Człowiek z żelaza"), to a fixpoint: every shape still a whole delimited or
   *  decorated piece of one of the titles. */
  private[identity] def shapes(titles: Seq[String], decorations: TitleDecorations = TitleDecorations.None): Seq[String] = {
    // A worklist, each shape split once: what one round adds is what its NEW shapes split into — the older shapes'
    // pieces are all held already — in the order a round over every shape would add them (by splitter, then shape).
    val held     = scala.collection.mutable.LinkedHashSet.empty[String]
    // a box office's certificate is no part of any shape: searched whole, "Obsession (18)" finds "80 Day Obsession: Day 18"
    var frontier = titles.map(services.billing.BillingParse(_).uncertified).filter(_.nonEmpty).distinct
    // a title quoting two works bills both: none of its spellings is cut to one quoted title
    val quotesOne = !frontier.exists(title => Quoted.findAllMatchIn(title).size >= 2)
    held ++= frontier
    while (frontier.nonEmpty) {
      // a learned decoration never cuts into an author's credit: its name is no film's title ("… by Noël Coward")
      frontier = (frontier.flatMap(SearchTitles.candidates(_, None)) ++ frontier.flatMap(t => decorations.strip(creditedWork(t).getOrElse(t))) ++ frontier.flatMap(beforeItsYear) ++
        frontier.flatMap(delimitedPieces(_, quotesOne))).map(_.trim).filter(shape => shape.nonEmpty && !held(shape)).distinct
      held ++= frontier
    }
    held.toSeq
  }

  /** The pieces more banner separators leave, beside the ones `SearchTitles` splits: a slash with a
   *  space on either side ("MISTYCZKA /film polski/", "Róża / Spotkanie Filozoficzne"), a dash with a
   *  space on one side ("Fregata dla seniorów- 500 Mil"), a code of up to three letters before a colon
   *  with no space ("MS:HOT SPOT"), a square-bracketed format and a title
   *  quoted at the head of its billing (`quotesOne`: none where a title quotes two), or at its tail after a banner's
   *  stop ('Kino Kobiet - KLAPS! "Jak żyć żeby nie zwariować"'); what an upper-case banner of two words or more runs
   *  into with an unspaced colon ("KINO NA NIEDZIELE:CAMINO DLA OPORNYCH"). A slash or colon inside a word is the
   *  title's own ("Face/Off", "AC/DC", "Star Wars:Episode I"); so is a sentence's stop: "Szkoła magicznych zwierząt.
   *  Tajemnica szkolnego podwórka" is the sequel, and its head the first film (whole-corpus dump, 2026-10-05: the
   *  head cut switched it and lost "Avengers: Koniec Gry. Dogrywka"). Here, not in `SearchTitles`, which the old
   *  pipeline also reads. */
  private def delimitedPieces(title: String, quotesOne: Boolean): Seq[String] = {
    val slashed = Seq(SpacedSlash, HalfSpacedDash).flatMap(separator =>
      if (separator.findFirstIn(title).isDefined) separator.split(title).toSeq.map(_.trim) else Nil)
    val coded   = CodeBeforeColon.findFirstMatchIn(title).map(m => title.substring(m.end)).toSeq
    val undated = (if (endsInScreeningYear(title)) ScreeningYearSuffix.findFirstMatchIn(title).map(_.group(1)) else None).toSeq
    val format  = (if (endsIn(title, _ == ']')) TrailingSquareBracket.findFirstMatchIn(title).map(_.group(1)) else None).toSeq
    val quoted  = (if (quotesOne && Quoted.findAllMatchIn(title).size == 1)
      LeadingQuoted.findFirstMatchIn(title).orElse(TrailingQuoted.findFirstMatchIn(title)).map(_.group(1)) else None).toSeq
    val banner  = BannerBeforeColon.findFirstMatchIn(title).map(m => title.substring(m.end)).toSeq
    (slashed ++ coded ++ undated ++ format ++ quoted ++ banner ++ editionHead(title) ++ caseSplit(title) ++ beforeCertificate(title) ++
      quotedWithin(title, quotesOne))
      .map(_.trim).filter(_.exists(_.isLetter)).filter(_ != title.trim)
  }

  /** The title before an EDITION its colon or spaced dash closes on — "TOUCH OF EVIL: THE RESTORED CUT" ×5 (US), "Blade
   *  Runner - The Final Cut", "Dune: Part One Reissue" is not one, as "Part One" is the film's: every word after the
   *  stop an edition's ([[EditionTail]]). `SearchTitles` reads what follows a colon as the title and the head as its
   *  banner, and searched only "THE RESTORED CUT" (prod 2026-11-01). */
  private def editionHead(title: String): Seq[String] = {
    // the stops, read by index: a pattern anchored at both ends backtracks over a long billing (the scan spec below)
    val stops = Iterator.iterate(title.indexOf(": "))(i => if (i < 0) i else title.indexOf(": ", i + 1)).takeWhile(_ >= 0).map(i => (i, i + 2)) ++
      Seq(" - ", " – ", " — ").iterator.flatMap(stop =>
        Iterator.iterate(title.indexOf(stop))(i => if (i < 0) i else title.indexOf(stop, i + 1)).takeWhile(_ >= 0).map(i => (i, i + stop.length)))
    stops.collectFirst { case (at, after) if {
      val tail = TitleContainment.tokens(title.substring(after))
      tail.exists(EditionTail) && tail.forall(word => EditionTail(word) || LeadingArticles(word)) &&
        title.substring(0, at).exists(_.isLetter)
    } => title.substring(0, at) }.toSeq
  }
  /** The words of an edition a billing names after its film: the word table's editions and anniversaries
   *  ([[services.billing.DecorationWords.EditionWords]]: cut, restored, remastered, 4K, re-release, anniversary…) and the
   *  words they come with ("Director's Cut", "The Final Cut", "Reissue", "Extended Edition"). */
  private val EditionTail: Set[String] = services.billing.DecorationWords.EditionWords ++
    Set("director", "directors", "final", "reissue", "extended", "uncut", "theatrical", "ultimate", "special", "edition")

  /** The two halves of a mixed-case billing at a CASE CHANGE: a run of two or more words in capitals heading or closing a
   *  billing whose other words are not, set off by a sentence's stop, an unspaced dash or a bare space — never by a colon
   *  or a spaced dash, which `SearchTitles` already splits, and where a film's own subtitle is billed in capitals
   *  ("Mission: Impossible – THE FINAL RECKONING"). The case change is the venue's: a strand closing the film's title
   *  (PL "Lalka.  DZIEŃ KINA POLSKIEGO", "Chłopiec w różowych spodniach-DZIECIŃSTWO BEZ PRZEMOCY"), a programme heading
   *  it ("UK PREMIERE For The Plasma"), or the film billed in capitals after the programme ("Godard est mort, vive
   *  Godard! DIE PASSION DER JUNGFRAU VON ORLÉANS"). Both halves are shapes: which one is the film, the candidates tell. */
  private[identity] def caseSplit(title: String): Seq[String] =
    if (!title.exists(_.isLower) || !title.exists(_.isUpper)) Nil
    else {
      val words = CaseWord.findAllMatchIn(title).toVector
      def capitals(w: String) = w.count(_.isLetter) >= 2 && !w.exists(_.isLower)
      def lettered(w: String) = w.exists(_.isLetter)
      def split(at: Int): Option[Seq[String]] = {
        val (head, tail) = (title.substring(0, words(at).start), title.substring(words(at).start))
        val stop = head.reverse.takeWhile(c => !c.isLetterOrDigit)
        val spaced = stop.exists(_.isWhitespace)
        val allowed = stop.trim.isEmpty || stop.trim.forall(".!?…".contains(_)) || (stop.trim.forall("-–—".contains(_)) && !spaced)
        Option.when(allowed && head.exists(_.isLetter) && tail.exists(_.isLetter) && bracketsClosed(head))(
          Seq(head.trim.stripSuffix(stop.trim).trim, tail.trim))
      }
      // a closing run of capitals
      val closing = words.reverseIterator.takeWhile(m => capitals(m.matched) || !lettered(m.matched)).size
      val opening = words.iterator.takeWhile(m => capitals(m.matched) || !lettered(m.matched)).size
      val tailRun = Option.when(closing >= 2 && closing < words.size && words.takeRight(closing).count(m => capitals(m.matched)) >= 2 &&
        words.dropRight(closing).exists(_.matched.exists(_.isLower)))(words.size - closing).flatMap(split)
      val headRun = Option.when(opening >= 2 && opening < words.size && words.take(opening).count(m => capitals(m.matched)) >= 2 &&
        words.drop(opening).exists(_.matched.exists(_.isLower)))(opening).flatMap(split)
      (tailRun.toSeq ++ headRun.toSeq).flatten.filter(_.nonEmpty)
    }
  private val CaseWord = """[\p{L}\p{N}][\p{L}\p{N}'’]*""".r
  /** Does `head` close every bracket it opens? A case change inside a bracket is the note's, never a strand's stop: PL
   *  "KADR JUNIOR (6-12 lat): …" split at "lat" searched "KADR JUNIOR (6-12". */
  private def bracketsClosed(head: String): Boolean = head.count(c => c == '(' || c == '[') == head.count(c => c == ')' || c == ']')

  /** The title before a box office's certificate note the billing runs on past — "Ken Russell's The Devils (18)
   *  Directors Cut", "Wicker (15) 105 mins 05.01.27" (UK, prod 2026-11-01): the ingest's peel takes a closing note only
   *  (FormatTags.uncertified). */
  private def beforeCertificate(title: String): Seq[String] =
    if (title.indexOf('(') < 0 && !title.toLowerCase(java.util.Locale.ROOT).contains("cert")) Nil
    else CertificateWithin.findFirstMatchIn(title).filter(m => m.end < title.length && title.substring(m.end).trim.nonEmpty &&
      title.substring(0, m.start).exists(_.isLetter)).map(m => title.substring(0, m.start)).toSeq
  private val CertificateWithin = (s"""(?i)(?<=\\S)\\s*${services.titlerules.TitleRulePlaceholders.all("CERT_NOTE")}(?=\\s)""").r

  /** The one title a long billing quotes within its prose — PL DKF's "DKF ZAPRASZA 13 LISTOPADA O GODZ. 18:00 NA FILM
   *  „NASZA REWOLUCJA” – HISTORIĘ MIŁOŚCI …" — of six words or more around the quote: a short billing quoting a work is
   *  a talk's or a show's about it ([[LeadingQuoted]]'s note), a long one an announcement of the film it quotes. */
  private def quotedWithin(title: String, quotesOne: Boolean): Seq[String] =
    if (!quotesOne || Quoted.findAllMatchIn(title).size != 1) Nil
    else QuotedTitle.findFirstMatchIn(title).filter(m =>
      TitleContainment.tokens(title.substring(0, m.start) + " " + title.substring(m.end)).count(_.length > 1) >= 6).map(_.group(1)).toSeq
  /** Whether a title can match a pattern anchored at its end, read off its last characters before the pattern scans it
   *  from the start: anchored at the end, a scan backtracks over every title that does not end so (11.8% of a
   *  convergence leg's single-threaded take-up, JFR run 38553692147). A `$` also matches before a final line break, so a
   *  title ending in one is always scanned. */
  private def endsIn(title: String, last: Char => Boolean): Boolean =
    title.nonEmpty && (last(title.last) || LineBreaks(title.last))
  private def endsInScreeningYear(title: String): Boolean = {
    def digit(c: Char) = c >= '0' && c <= '9'
    val n = title.length
    (n >= 5 && title(n - 4) == '2' && title(n - 3) == '0' && (title(n - 2) == '2' || title(n - 2) == '3') &&
      digit(title(n - 1))) || endsIn(title, _ => false)
  }
  private val LineBreaks = Set('\n', '\r', '\u0085', '\u2028', '\u2029')
  /** A format or version tag in square brackets after the title ("Vivaldi i ja [2D LEKTOR]"); a round bracket is
   *  `SearchTitles`'s. */
  private val TrailingSquareBracket = """^(.*\p{L}.*?)\s*\[[^\]\[]*\]$""".r
  /** A title quoted at the head of a programme's billing ("„Baranek Shaun i kudłata bestia” Rodzinne Poranki Filmowe"),
   *  the title's only quotes: two quoted titles bill two works, and a quote further in names what a talk, a concert or a
   *  show is about ('Wojciech Cejrowski i „Prawo Dżungli”', 'ZADUSZKI JAZZOWE GABA JANUSZ "NIEOBECNI"'). */
  private val Quoted        = """[„"“«][^"”„“«»]*["”“»]""".r
  /** A quoted title of words, wherever it stands: its words the first group. */
  private[identity] val QuotedTitle = """[„"“«]([^"”„“«»]*\p{L}[^"”„“«»]*)["”“»]""".r
  private val LeadingQuoted = ("""^\s*""" + QuotedTitle.regex).r
  private val SpacedSlash     = """\s+/\s*|\s*/\s+""".r
  /** A dash with a space on ONE side ("Fregata dla seniorów- 500 Mil"); both sides is `SearchTitles`'s. */
  private val HalfSpacedDash  = """(?<=\S)[-–—]\s+|\s+[-–—](?=\S)""".r
  private val CodeBeforeColon = """^\p{L}{1,3}:(?=\S)""".r
  private val TrailingQuoted  = """[:!\-–—]\s*[„"“«]([^"”„“«»]*\p{L}[^"”„“«»]*)["”“»]\s*$""".r
  private val BannerBeforeColon = """^[\p{Lu}\d]+(?:\s+[\p{Lu}\d]+)+:(?=\p{L})""".r
  /** A screening year after a title, unbracketed ("Ma to sens 2026"): 2020–2039 only, so a title that IS a
   *  number ("2046", "Blade Runner 2049") keeps it. A shape, never the old pipeline's lookup query. */
  private val ScreeningYearSuffix = """^(.*\p{L}.*?)\s+20[23]\d$""".r

  /** Two titles that are the same words but for ONE, a letter apart — a venue's typo ("The Beast of
   *  Mossy Botton", "Pradhama Drishtiya Kuttakkar") — where both spellings of that word run to five
   *  letters and neither is a number, in a title of two words or more: a sequel's numeral ("Scary Movie 3", "Mission: Impossible II")
   *  or a short word ("Hunt"/"Hurt") is a different title, not a typo. */
  private[identity] def oneTypoApart(a: Seq[String], b: Seq[String]): Boolean =
    // Two words at least: a one-word title a letter from another is another film ("Lalka"/"Lalkar").
    a.size >= 2 && a.size == b.size && a != b && {
      // The one index the two differ at, or -1 for more than one: counted in place, not as a vector of them per pair.
      val differing = { var at = -1; var i = 0; var n = 0
        while (i < a.size && n < 2) { if (a(i) != b(i)) { at = i; n += 1 }; i += 1 }
        if (n == 1) at else -1 }
      differing >= 0 && {
        val (x, y) = (a(differing), b(differing))
        x.length >= 5 && y.length >= 5 && !Seq(x, y).exists(w => w.exists(_.isDigit) || RomanNumeral.pattern.matcher(w).matches()) &&
          tools.EditDistance.within(x, y, 1)
      }
    }
}
