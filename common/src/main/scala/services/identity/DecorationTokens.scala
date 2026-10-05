package services.identity

import services.movies.{FormatTags, TitleContainment}

/**
 * Decoration stripping learned SUPERVISED, token by token: which words of a venue's title are the venue's (a programme
 * banner, a format, a date) and which are the film's. Trained on listings the model already matched — each listing's
 * title aligned with its film's titles ([[DecorationTokens.align]]): the edge words no film title covers are decoration,
 * the covered ones title — and scored by a logistic regression over what the title's text and the corpus say of each
 * word ([[DecorationTokens.Features]]). What a fitted model strips ([[DecorationTokens.Model.inner]]) is a search title
 * the resolver may try, never a rule by itself: it is measured (`integration.IdentityDecorationTokens`).
 */
object DecorationTokens {

  /** One word of a title, with the text around it: its token, and the raw text between it and its neighbours. */
  final case class Word(token: String, before: String, after: String, allCaps: Boolean)

  /** A title's words as the token rule reads them, with the text between them — `None` when the two disagree. */
  def words(title: String): Option[IndexedSeq[Word]] = TitleDecorations.words(title).map { ws =>
    ws.indices.map { i =>
      val (token, start, end) = ws(i)
      val before = title.substring(if (i == 0) 0 else ws(i - 1)._3, start)
      val after  = title.substring(end, if (i == ws.size - 1) title.length else ws(i + 1)._2)
      val text   = title.substring(start, end)
      Word(token, before, after, text.exists(_.isLetter) && text.filter(_.isLetter).forall(_.isUpper))
    }
  }

  /** A matched listing's title as the training reads it: each word decoration (true) or the film's (false). */
  final case class Aligned(tokens: IndexedSeq[String], decoration: IndexedSeq[Boolean]) {
    def inner: Seq[String] = tokens.indices.filterNot(decoration).map(tokens)
    def clean: Boolean = !decoration.contains(true)
  }

  /** `title` aligned with `filmTitles`: the longest film title that is a contiguous run of its words covers them, the
   *  edge words outside it are decoration. `None` when no film title is such a run, or two longest ones sit in
   *  different places (ambiguous). */
  def align(title: String, filmTitles: Seq[String]): Option[Aligned] = {
    val tokens = TitleContainment.tokens(title).toIndexedSeq
    val spans  = filmTitles.map(TitleContainment.tokens).filter(_.nonEmpty).distinct.flatMap { t =>
      tokens.indices.filter(i => tokens.startsWith(t, i)).map(i => (i, t.size))
    }.distinct
    val longest = spans.map(_._2).maxOption
    longest.flatMap { size =>
      spans.filter(_._2 == size) match {
        case Seq((start, n)) => Some(Aligned(tokens, tokens.indices.map(i => i < start || i >= start + n)))
        case _               => None
      }
    }
  }

  /** How widely the corpus uses a word: venues and films billing it among listings, and record titles carrying it. */
  final case class Spread(venues: Int, films: Int, records: Int)

  /** What the text and the corpus say of word `i` of `ws`. */
  final case class Features(edgeDistance: Int, relative: Double, separatorBefore: Boolean, separatorAfter: Boolean, digits: Boolean,
                            year: Boolean, format: Boolean, allCaps: Boolean, spread: Spread, length: Int) {
    def vector: Array[Double] = Array(1.0, math.min(edgeDistance, 5) / 5.0, if (edgeDistance == 0) 1.0 else 0.0, relative,
      flag(separatorBefore), flag(separatorAfter), flag(digits), flag(year), flag(format), flag(allCaps),
      math.log1p(spread.venues.toDouble), math.log1p(spread.films.toDouble), math.log1p(spread.records.toDouble), math.min(length, 12) / 12.0)
    private def flag(b: Boolean) = if (b) 1.0 else 0.0
  }
  val Names: Seq[String] = Seq("intercept", "edgeDistance", "atEdge", "relativePosition", "separatorBefore", "separatorAfter", "digits", "year",
    "formatWord", "allCaps", "log1p(venues)", "log1p(films)", "log1p(records)", "titleLength")

  private val Separator = """[:|–—\-./"„”“«»()\[\]+,;!?]""".r
  private val Year      = """(19|20)\d\d""".r

  def features(ws: IndexedSeq[Word], i: Int, spread: String => Spread): Features = {
    val w = ws(i)
    Features(math.min(i, ws.size - 1 - i), if (ws.size == 1) 0.0 else i.toDouble / (ws.size - 1),
      i > 0 && Separator.findFirstIn(w.before).isDefined, i < ws.size - 1 && Separator.findFirstIn(w.after).isDefined,
      w.token.exists(_.isDigit), Year.matches(w.token), FormatTags.FormatToken.contains(w.token), w.allCaps, spread(w.token), ws.size)
  }

  /** A fitted model: its weights over [[Names]], and the probability at which a word is decoration. */
  final case class Model(weights: Seq[Double], cut: Double = 0.5) {
    private val w = weights.toIndexedSeq
    def probability(f: Features): Double = LogisticFit.sigmoid(LogisticFit.dot(w, f.vector.toIndexedSeq))
    /** The words of `ws` left once the edge runs the model reads as decoration are cut, from either end, one word kept. */
    def inner(ws: IndexedSeq[Word], spread: String => Spread): Seq[String] = {
      val p = ws.indices.map(i => probability(features(ws, i, spread)) >= cut)
      val from  = p.indexWhere(!_) match { case -1 => ws.size; case k => k }
      val until = p.lastIndexWhere(!_) + 1
      if (from >= until) ws.map(_.token) else ws.slice(from, until).map(_.token)
    }
  }

  val L2 = 1.0
  val Iterations = 25

  /** The model `rows` (each word's features and whether it is decoration) fit. */
  def fit(rows: Seq[(Features, Boolean)]): Model =
    Model(LogisticFit.fit(rows.map(_._1.vector).toArray, rows.map(r => if (r._2) 1.0 else 0.0).toArray, L2, Iterations))
}
