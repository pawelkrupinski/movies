package services.identity

import play.api.libs.json.{Json, OFormat}
import services.movies.TitleContainment

/**
 * The VENUE DECORATIONS the identity resolver reads around a film's title — "Unlimited Screening",
 * "(4DX Rewind)", "2D PL", "w Helios na Scenie" — LEARNED from the listings and the film records
 * (`scripts.IdentityDecorationsLearn`, run by `scripts/identity-calibrate.sh`), never listed by
 * hand. An undelimited decoration keeps a title's own search empty ("Girls Like Girls Unlimited
 * Screening" names no film) and its title relation at `none`, so the film its plain siblings found
 * is never offered to it; a delimited one ("… | FKS", "Kino: …") is a banner segment already
 * (`SearchTitles.candidates`).
 *
 * A token run is a decoration when
 *  - it is an EDGE of listing titles whose remainder is itself a listing's whole title, for at
 *    least [[MinFilms]] different remainders (a remainder that is only a decorated spelling of
 *    another, "Lalka 2D" beside "Lalka", counts once) — it recurs across films, it is not part of
 *    one — or, around ONE remainder, when it recurs across venues instead: at least [[MinVenues]]
 *    carry it, the remainder is a film record's title exactly, the decorated title's own searches
 *    found no film at all (what it names, if anything, is the remainder's), and no record title
 *    crosses from the remainder into the run or starts the run where the remainder ends ("Girls
 *    Like Girls Unlimited Screening" beside the record "Girls Like Girls"; never "Friday the 13th
 *    (1980)" beside "Friday", whose title names the record "Friday the 13th", nor "Michael Mann's
 *    Manhunter: The Final Cut" beside "Michael", whose search finds "Manhunter", nor the double
 *    bill "Basia. Humor w paski mam + Kocia Szajka"); and
 *  - no film record's title, original title or alternative title carries it anywhere: "OPERA",
 *    "Exhibition on Screen", "Throwback" and "The" are words TMDB titles use, so they are the
 *    film's, not the venue's.
 *
 * A decoration is stripped ADDITIVELY — the undecorated spelling becomes one more title shape
 * (`IdentityMeasures.titleShapes`), so it is searched and read by the title relation; the listing's
 * own title, its card and its programme are untouched.
 */
final case class TitleDecorations(prefixes: Set[Seq[String]], suffixes: Set[Seq[String]]) {

  /** Every spelling of `title` with ONE learned decoration taken off either edge, the title's own
   *  text kept (its casing and punctuation, which the search reads): "(4DX Rewind) Shrek" →
   *  "Shrek". `IdentityMeasures.titleShapes` repeats it to a fixpoint. */
  def strip(title: String): Seq[String] =
    if (prefixes.isEmpty && suffixes.isEmpty) Nil
    else TitleDecorations.words(title).toSeq.flatMap { ws =>
      val tokens = ws.map(_._1)
      val n = tokens.size
      val fromFront = (1 until n).filter(k => prefixes(tokens.take(k))).map(k => title.substring(ws(k)._2))
      val fromBack  = (1 until n).filter(k => suffixes(tokens.takeRight(k))).map(k => title.substring(0, ws(n - k)._2))
      (fromFront ++ fromBack).map(TitleDecorations.trimmed).filter(_.nonEmpty)
    }.distinct
}

object TitleDecorations {

  val None: TitleDecorations = TitleDecorations(Set.empty, Set.empty)

  /** "Recurs across films": two different films at the least. The smallest count that says it,
   *  not a tuned number — a run with one remainder is indistinguishable from a film's own title. */
  val MinFilms = 2

  /** "Recurs" for a run seen around ONE film: two venues carrying it. */
  val MinVenues = 2

  /** One learned decoration and where it was learned: the remainders it was seen around (up to
   *  [[ProvenanceExamples]], sorted), how many there are, and how many venues and listing titles
   *  carry it. */
  final case class Learned(side: String, decoration: String, films: Int, venues: Int, titles: Int, examples: Seq[String])
  val ProvenanceExamples = 5

  /** The artefact `scripts.IdentityDecorationsLearn` writes: the decorations with provenance. */
  final case class Artefact(version: String, basis: String, inputs: Map[String, Int], decorations: Seq[Learned]) {
    def decorationsOf: TitleDecorations = TitleDecorations(
      decorations.filter(_.side == "prefix").map(d => d.decoration.split(" ").toSeq).toSet,
      decorations.filter(_.side == "suffix").map(d => d.decoration.split(" ").toSeq).toSet)
  }
  implicit val learnedFormat: OFormat[Learned]   = Json.format[Learned]
  implicit val artefactFormat: OFormat[Artefact] = Json.format[Artefact]

  val ResourcePath = "identity-decorations.json"

  def fromResource(path: String): Option[Artefact] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).map { in =>
      try Json.parse(in).as[Artefact] finally in.close()
    }

  /** The resolver's decorations: the artefact on the classpath. */
  lazy val resolver: TitleDecorations =
    fromResource(ResourcePath).map(_.decorationsOf).getOrElse(throw new IllegalStateException(s"$ResourcePath is not on the classpath"))

  /** The decorations `listings` (each a venue and one of its titles) carry that no title of
   *  `recordTitles` does, strongest first (most films, then the text). A function of the two SETS:
   *  any order of either learns the same list. */
  def learn(listings: Iterable[(String, String)], recordTitles: Iterable[String],
            searches: Iterable[(String, Boolean)] = Nil): Seq[Learned] = {
    val byTokens: Map[Seq[String], Set[String]] = listings.iterator
      .map { case (venue, title) => TitleContainment.tokens(title) -> venue }.filter(_._1.nonEmpty).toSeq
      .groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    // (side, run) → remainder → venues carrying the decorated title.
    val seen = scala.collection.mutable.HashMap.empty[(String, Seq[String]), Map[Seq[String], Set[String]]]
    def note(side: String, run: Seq[String], rest: Seq[String], venues: Set[String]): Unit =
      if (byTokens.contains(rest))
        seen.updateWith((side, run))(m => Some(m.getOrElse(Map.empty).updatedWith(rest)(v => Some(v.getOrElse(Set.empty) ++ venues))))
    byTokens.foreach { case (tokens, venues) =>
      (1 until tokens.size).foreach { k =>
        note("prefix", tokens.take(k), tokens.drop(k), venues)
        note("suffix", tokens.takeRight(k), tokens.dropRight(k), venues)
      }
    }
    val recordKeys = recordTitles.iterator.map(TitleContainment.tokens).filter(_.nonEmpty).toSet
    // A title's own searches answered empty, by its words: every spelling of them.
    val searchedEmpty: Set[Seq[String]] = searches.iterator.map { case (t, empty) => TitleContainment.tokens(t) -> empty }.toSeq
      .groupMapReduce(_._1)(_._2)(_ && _).collect { case (ws, true) if ws.nonEmpty => ws }.toSet
    /** One film only: its rest is a record's title, two venues carry it, the decorated title's own
     *  searches found nothing, and no record's title crosses from the rest into the run or starts
     *  the run where the rest ends — the title goes on with another film's ("Basia. Humor w paski
     *  mam + Kocia Szajka", "Toddler Club: Tabby McTat + Room on the Broom"): a programme. */
    def aroundOneFilm(side: String, run: Seq[String], byRest: Map[Seq[String], Set[String]], rest: Seq[String]): Boolean = {
      val decorated = if (side == "prefix") run ++ rest else rest ++ run
      val runsOn = (rest.size + 1 to decorated.size).exists(n => recordKeys(if (side == "prefix") decorated.takeRight(n) else decorated.take(n)))
      val nextFilm = (1 to run.size).exists(n => recordKeys(if (side == "prefix") run.takeRight(n) else run.take(n)))
      recordKeys(rest) && searchedEmpty(decorated) && byRest.values.flatten.toSet.size >= MinVenues && !runsOn && !nextFilm
    }
    val recurring = seen.toSeq.map { case (key, byRest) => key -> (byRest, distinctFilms(byRest.keySet)) }
      .filter { case ((side, run), (byRest, films)) =>
        films.size >= MinFilms || (films.size == 1 && aroundOneFilm(side, run, byRest, films.head)) }
    val inRecords = carried(recordTitles, recurring.map(_._1._2).toSet)
    recurring.collect { case ((side, run), (byRest, films)) if !inRecords(run) =>
      Learned(side, run.mkString(" "), films.size, byRest.values.flatten.toSet.size, byRest.size,
        films.toSeq.map(_.mkString(" ")).sorted.take(ProvenanceExamples))
    }.sortBy(d => (-d.films, d.side, d.decoration))
  }

  /** `learned` — this recording's decorations — with every `earlier` one it did not learn again, unless a title of
   *  `recordTitles` now carries it. A recording sees only the programmes billed that week: a banner learned around two
   *  films stays a banner after its season ends ("WAJDA: re-wizje"), and comes back with the next one. What both learned
   *  is this recording's; the list is ordered as [[learn]] orders it. */
  def accumulate(earlier: Seq[Learned], learned: Seq[Learned], recordTitles: Iterable[String]): Seq[Learned] = {
    val relearned = learned.map(d => (d.side, d.decoration)).toSet
    val kept      = earlier.filterNot(d => relearned((d.side, d.decoration)))
    val inRecords = carried(recordTitles, kept.map(d => d.decoration.split(" ").toSeq).toSet)
    (learned ++ kept.filterNot(d => inRecords(d.decoration.split(" ").toSeq))).sortBy(d => (-d.films, d.side, d.decoration))
  }

  /** The remainders that are not a decorated spelling of another remainder: "lalka" and "lalka 2d"
   *  are one film. */
  private def distinctFilms(rests: Set[Seq[String]]): Set[Seq[String]] =
    rests.filterNot(r => rests.exists(o => TitleContainment.isTokenRun(o, r)))

  /** The runs of `wanted` some title of `titles` carries anywhere, as a contiguous token run. */
  private def carried(titles: Iterable[String], wanted: Set[Seq[String]]): Set[Seq[String]] = {
    val longest = wanted.map(_.size).maxOption.getOrElse(0)
    titles.iterator.map(TitleContainment.tokens).flatMap(ts =>
      ts.indices.iterator.flatMap(i => (1 to math.min(longest, ts.size - i)).iterator.map(n => ts.slice(i, i + n)).filter(wanted))).toSet
  }

  /** A title's words as `TitleContainment.tokens` reads them, each with its start and end in the
   *  title's own text — `None` when the two disagree (a character the token rule splits inside a
   *  word), so such a title is never cut. */
  private[identity] def words(title: String): Option[IndexedSeq[(String, Int, Int)]] = {
    val spans = WordRun.findAllMatchIn(title).map(m => (m.start, m.end)).toIndexedSeq
    val ws = spans.flatMap { case (s, e) => TitleContainment.tokens(title.substring(s, e)) match {
      case Seq(t) => Some((t, s, e))
      case _      => scala.None
    } }
    Option.when(ws.size == spans.size && ws.map(_._1) == TitleContainment.tokens(title))(ws)
  }
  private val WordRun = """[\p{L}\p{N}\p{M}]+""".r
  private val LeadingSeparators  = java.util.regex.Pattern.compile("""^[\s\-–—:|/)\]},;.]+""")
  private val TrailingSeparators = java.util.regex.Pattern.compile("""[\s\-–—:|/(\[{,;]+$""")

  /** A cut spelling without the separators and brackets the cut left at its edges. */
  private def trimmed(s: String): String =
    TrailingSeparators.matcher(LeadingSeparators.matcher(s).replaceAll("")).replaceAll("").trim
}
