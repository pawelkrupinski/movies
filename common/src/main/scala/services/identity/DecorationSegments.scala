package services.identity

import services.movies.{FormatTags, TitleContainment}

/**
 * Decoration stripping learned SUPERVISED by STRUCTURE: a venue's title split at its delimiters (":", " - ", "|", "/",
 * ". ", "+", brackets, quotes, ", ") into segments, and each segment scored as the venue's (a programme banner, a format,
 * a date) or the film's by a logistic regression over every signal held about it ([[DecorationSegments.Groups]]: its
 * structure — the delimiter on each side one-hot, so each delimiter's weight is LEARNED —, its words, how the corpus
 * bills it, the venue's own habits, the listing's other spellings, the venue's facts, and what a recorded search for it
 * found). The counterpart of [[DecorationTokens]] (word by word); like it, trained on the listings the model matched,
 * aligned with their film's titles, and only ever a search title to be measured — never a rule by itself.
 */
object DecorationSegments {

  /** The delimiters a segment can sit between; "edge" is the title's own start or end. */
  val Delimiters: Seq[String] = Seq("edge", "colon", "dash", "pipe", "slash", "dot", "plus", "bracket", "quote", "comma")
  private val Split = """\s*(:|\s[-–—]\s|[–—]|\||/|\.\s|\+|[()\[\]]|[„”“"«»]|,\s)\s*""".r

  private def kind(delimiter: String): String = delimiter.trim match {
    case ":"                   => "colon"
    case "-" | "–" | "—"       => "dash"
    case "|"                   => "pipe"
    case "/"                   => "slash"
    case "."                   => "dot"
    case "+"                   => "plus"
    case "(" | ")" | "[" | "]" => "bracket"
    case ","                   => "comma"
    case _                     => "quote"
  }

  /** One segment: its text as billed, its words, and the delimiter kinds before and after it. */
  final case class Segment(text: String, tokens: Seq[String], before: String, after: String) {
    def key: String = tokens.mkString(" ")
  }

  /** `title`'s segments, empty ones folded into their neighbours — `None` when their words are not the title's. */
  def segments(title: String): Option[IndexedSeq[Segment]] = {
    val cuts   = Split.findAllMatchIn(title).toIndexedSeq
    val pieces = (Seq(0) ++ cuts.map(_.end)).zip(cuts.map(_.start) ++ Seq(title.length)).zipWithIndex.map { case ((from, until), i) =>
      (title.substring(from, math.max(from, until)), if (i == 0) "edge" else kind(cuts(i - 1).group(1)), if (i == cuts.size) "edge" else kind(cuts(i).group(1)))
    }
    val kept = pieces.foldLeft(Vector.empty[Segment]) { case (acc, (text, before, after)) =>
      val tokens = TitleContainment.tokens(text)
      if (tokens.isEmpty) acc.lastOption.fold(acc)(last => acc.init :+ last.copy(after = if (after == "edge") last.after else after))
      else acc :+ Segment(text.trim, tokens, if (acc.isEmpty) "edge" else before, after)
    }
    Option.when(kept.nonEmpty && kept.flatMap(_.tokens) == TitleContainment.tokens(title))(kept)
  }

  /** Does the title bill several works — a "+", or two quoted titles? Never stripped to one film. */
  def billsSeveral(segs: IndexedSeq[Segment]): Boolean =
    segs.exists(s => s.before == "plus" || s.after == "plus") || segs.count(s => s.before == "quote" && s.after == "quote") >= 2

  /** Each segment decoration (true) or the film's (false), by the words an [[DecorationTokens.Aligned]] title covers —
   *  `None` when the film's title starts or ends inside a segment. */
  def labels(segs: IndexedSeq[Segment], aligned: DecorationTokens.Aligned): Option[IndexedSeq[Boolean]] = {
    val offsets  = segs.scanLeft(0)(_ + _.tokens.size)
    val labelled = segs.indices.map { i =>
      val flags = (offsets(i) until offsets(i + 1)).map(aligned.decoration)
      if (flags.forall(identity)) Some(true) else if (flags.forall(!_)) Some(false) else None
    }
    Option.when(labelled.forall(_.isDefined))(labelled.flatten)
  }

  /** Everything beyond the title's own text a segment's features read: the corpus, the venue, the listing, the searches. */
  trait Context {
    /** Venues and distinct films billing this segment text as a segment. */
    def venues(segment: String): Int
    def films(segment: String): Int
    /** Venues of the same chain (first word of the venue name) billing it. */
    def chainVenues(segment: String): Int
    /** Is it a learned decoration (`TitleDecorations`), a film record's whole title, another listing's whole title? */
    def knownDecoration(segment: String): Boolean
    def recordTitle(segment: String): Boolean
    def billedPlain(segment: String): Boolean
    /** Is it the whole title of another listing in this listing's cluster? */
    def sibling(segment: String): Boolean
    /** The share of its words any film record's title uses. */
    def recordWordShare(tokens: Seq[String]): Double
    /** The venue's other matched titles: the share whose segment in the same place was decoration, smoothed. */
    def venuePrior(place: String): Double
    /** The listing's own facts: its credited directors' name words, its year, its original title. */
    def directorWords: Set[String]
    def listingYear: Option[Int]
    def originalTitle: Option[String]
    /** A recorded TMDB search for the segment: None when not recorded, else its hits' titles (as words). */
    def search(segment: String): Option[Seq[Seq[String]]]
  }

  final case class Feature(group: String, name: String, value: Double)

  private val Year        = """(19|20)\d\d""".r
  private val Roman       = Set("ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x", "vol", "cz", "czesc", "part", "teil", "parte", "set")
  private val EventWords  = Set("spotkanie", "prelekcja", "prelekcja", "pokaz", "specjalny", "q", "a", "qa", "premiera", "przedpremiera", "konkurs",
    "dyskusja", "rozmowa", "seans", "screening", "event", "special", "preview", "live", "talk", "intro", "introduced", "gesprach", "vorpremiere",
    "festiwal", "festival", "retransmisja", "koncert", "karnet", "maraton", "marathon")
  private val EditionWords = Set("cut", "remaster", "remastered", "restored", "restoration", "4k", "rerelease", "re", "release", "anniversary",
    "aniversario", "rocznica", "edition", "extended", "director", "directors", "wersja", "rozszerzona", "jubileusz")
  private val PlaceWords  = Set("sala", "zl", "pln", "eur", "gbp", "usd", "bilet", "bilety", "dzieci", "kids", "seniora", "senior", "familie", "family")
  private val Stopwords   = Set("i", "w", "z", "na", "do", "the", "a", "of", "and", "in", "der", "die", "das", "und", "el", "la", "los", "las", "de", "y")

  private def flag(b: Boolean) = if (b) 1.0 else 0.0
  private def caps(text: String) = text.exists(_.isLetter) && text.filter(_.isLetter).forall(_.isUpper)

  /** Segment `i` of `segs` as every signal group sees it. */
  def features(segs: IndexedSeq[Segment], i: Int, ctx: Context): Seq[Feature] = {
    val s = segs(i); val ws = s.tokens; val n = ws.size.toDouble
    def share(p: String => Boolean) = ws.count(p) / n
    val years   = ws.filter(t => Year.matches(t)).map(_.toInt)
    val hits    = ctx.search(s.key)
    val structure = Seq("first" -> flag(i == 0), "last" -> flag(i == segs.size - 1), "segments" -> math.min(segs.size, 6) / 6.0) ++
      Delimiters.drop(1).map(d => s"before:$d" -> flag(s.before == d)) ++ Delimiters.drop(1).map(d => s"after:$d" -> flag(s.after == d)) ++
      Seq("quoted" -> flag(s.before == "quote" && s.after == "quote"), "words" -> math.min(n, 8) / 8.0,
        "punctuation" -> s.text.count(c => !c.isLetterOrDigit && !c.isWhitespace).toDouble / math.max(1, s.text.length),
        "allCaps" -> flag(caps(s.text)), "caseChange" -> flag(segs.indices.exists(j => j != i && caps(segs(j).text) != caps(s.text))))
    val lexicon = Seq("formatWords" -> share(FormatTags.FormatToken.contains), "eventWords" -> share(EventWords), "editionWords" -> share(EditionWords),
      "placeWords" -> share(t => PlaceWords(t) || t.endsWith("zl")), "numbered" -> share(t => Roman(t) || t.forall(_.isDigit) && t.length <= 2),
      "year" -> flag(years.nonEmpty), "yearInBrackets" -> flag(years.nonEmpty && s.before == "bracket" && ws.size == 1),
      "stopwords" -> share(Stopwords), "digits" -> flag(ws.exists(_.exists(_.isDigit))), "nonAscii" -> flag(s.text.exists(c => c.isLetter && c > 127)))
    val corpus = Seq("log1p(venues)" -> math.log1p(ctx.venues(s.key).toDouble), "log1p(films)" -> math.log1p(ctx.films(s.key).toDouble),
      "knownDecoration" -> flag(ctx.knownDecoration(s.key)), "recordTitle" -> flag(ctx.recordTitle(s.key)), "recordWords" -> ctx.recordWordShare(ws),
      "billedPlain" -> flag(ctx.billedPlain(s.key)))
    val venue = Seq("venuePrior" -> ctx.venuePrior(place(segs, i)), "log1p(chainVenues)" -> math.log1p(ctx.chainVenues(s.key).toDouble))
    val cross = Seq("siblingTitle" -> flag(ctx.sibling(s.key)))
    val facts = Seq("directorWords" -> flag(ws.exists(ctx.directorWords)), "possessive" -> flag(s.text.matches("""(?s).*['’]s$""")),
      "listingYear" -> flag(years.exists(y => ctx.listingYear.contains(y))),
      "originalTitle" -> flag(ctx.originalTitle.exists(t => TitleContainment.tokens(t) == ws)))
    val search = Seq("searched" -> flag(hits.isDefined), "searchHits" -> math.log1p(hits.fold(0)(_.size).toDouble),
      "searchExact" -> flag(hits.exists(_.contains(ws))), "searchFirstExact" -> flag(hits.exists(_.headOption.contains(ws))))
    Seq("structure" -> structure, "lexicon" -> lexicon, "corpus" -> corpus, "venue" -> venue, "cross" -> cross, "facts" -> facts, "search" -> search)
      .flatMap { case (group, fs) => fs.map { case (name, value) => Feature(group, name, value) } }
  }

  /** The groups of [[features]], in order. */
  val Groups: Seq[String] = Seq("structure", "lexicon", "corpus", "venue", "cross", "facts", "search")

  /** The place a venue prior is kept by: first, last or middle, and the delimiters either side. */
  def place(segs: IndexedSeq[Segment], i: Int): String =
    s"${if (i == 0) "first" else if (i == segs.size - 1) "last" else "middle"}|${segs(i).before}|${segs(i).after}"

  /** A fitted model over the named features it was fitted on (an ablation leaves a group out). */
  final case class Model(names: Seq[String], weights: Seq[Double], cut: Double = 0.5) {
    private val w = weights.toIndexedSeq
    def vector(fs: Seq[Feature]): IndexedSeq[Double] = { val byName = fs.map(f => f.name -> f.value).toMap; (1.0 +: names.drop(1).map(byName.getOrElse(_, 0.0))).toIndexedSeq }
    def probability(fs: Seq[Feature]): Double = LogisticFit.sigmoid(LogisticFit.dot(w, vector(fs)))
    /** The words left once the edge segments the model reads as decoration are cut, from either end, one segment kept;
     *  a title billing several works is left whole. */
    def inner(segs: IndexedSeq[Segment], featuresOf: Int => Seq[Feature]): Seq[String] =
      if (billsSeveral(segs)) segs.flatMap(_.tokens)
      else {
        val p     = segs.indices.map(i => probability(featuresOf(i)) >= cut)
        val from  = p.indexWhere(!_) match { case -1 => segs.size; case k => k }
        val until = p.lastIndexWhere(!_) + 1
        (if (from >= until) segs else segs.slice(from, until)).flatMap(_.tokens)
      }
  }

  val L2 = 1.0
  val Iterations = 25

  /** The model `rows` fit over every feature but those of `without`'s groups. */
  def fit(rows: Seq[(Seq[Feature], Boolean)], without: Set[String] = Set.empty): Model = {
    val names = "intercept" +: rows.headOption.toSeq.flatMap(_._1).filterNot(f => without(f.group)).map(_.name)
    val shell = Model(names, Seq.fill(names.size)(0.0))
    Model(names, LogisticFit.fit(rows.map(r => shell.vector(r._1).toArray).toArray, rows.map(r => if (r._2) 1.0 else 0.0).toArray, L2, Iterations))
  }
}
