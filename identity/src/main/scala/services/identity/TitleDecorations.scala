package services.identity

import play.api.libs.json.{Json, OFormat}
import services.movies.TitleContainment

/**
 * The VENUE DECORATIONS the identity resolver reads around a film's title — "Unlimited Screening",
 * "(4DX Rewind)", "2D PL", "w Helios na Scenie" — LEARNED from the listings and the film records
 * (`TitleDecorationsLearner` in `scripts.IdentityDecorationsLearn`, run by `scripts/identity-calibrate.sh`), never listed by
 * hand. An undelimited decoration keeps a title's own search empty ("Girls Like Girls Unlimited
 * Screening" names no film) and its title relation at `none`, so the film its plain siblings found
 * is never offered to it; a delimited one ("… | FKS", "Kino: …") is a banner segment already
 * (`SearchTitles.candidates`).
 *
 * A token run is a decoration when
 *  - it is an EDGE of listing titles whose remainder is itself a listing's whole title, for at
 *    least `TitleDecorationsLearner.MinFilms` different remainders (a remainder that is only a decorated spelling of
 *    another, "Lalka 2D" beside "Lalka", counts once) — it recurs across films, it is not part of
 *    one — or, around ONE remainder, when it recurs across venues instead: at least `TitleDecorationsLearner.MinVenues`
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
final case class TitleDecorations(prefixes: Set[Seq[String]], suffixes: Set[Seq[String]], tails: Set[Seq[String]] = Set.empty) {

  /** Every spelling of `title` with ONE learned decoration taken off either edge, the title's own
   *  text kept (its casing and punctuation, which the search reads): "(4DX Rewind) Shrek" →
   *  "Shrek" — or with a learned event TAIL it bills last cut off: "Punku + spotkanie z reżyserem" → "Punku".
   *  `IdentityMeasures.titleShapes` repeats it to a fixpoint. */
  def strip(title: String): Seq[String] =
    if (prefixes.isEmpty && suffixes.isEmpty && tails.isEmpty) Nil
    else (TitleDecorations.words(title).toSeq.flatMap { ws =>
      val tokens = ws.map(_._1)
      val n = tokens.size
      val fromFront = (1 until n).filter(k => prefixes(tokens.take(k))).map(k => title.substring(ws(k)._2))
      val fromBack  = (1 until n).filter(k => suffixes(tokens.takeRight(k))).map(k => title.substring(0, ws(n - k)._2))
      (fromFront ++ fromBack).map(TitleDecorations.trimmed).filter(_.nonEmpty)
    } ++ Option(withoutTail(title)).filter(_ != title)).distinct

  /** `title` with every learned event tail it bills last cut off, with its join: "Punku + spotkanie z reżyserem +
   *  PJM" → "Punku". What a talk or a signed screening follows is one work, not a double bill. */
  def withoutTail(title: String): String =
    if (tails.isEmpty) title
    else TitleDecorations.LastBillJoin.findFirstMatchIn(title).filter { m =>
      val piece = TitleContainment.tokens(title.substring(m.end))
      (1 to math.min(piece.size, TitleDecorations.TailRun)).exists(n => tails(piece.take(n)))
    }.fold(title)(m => withoutTail(title.substring(0, m.start).trim))
}

object TitleDecorations {

  val None: TitleDecorations = TitleDecorations(Set.empty, Set.empty)

  /** One learned decoration and where it was learned: the remainders it was seen around (up to
   *  `TitleDecorationsLearner.ProvenanceExamples`, sorted), how many there are, and how many venues and listing titles
   *  carry it — and, for one the detector kept (`integration.IdentityDecorationCandidates`), what its measure found. */
  final case class Learned(side: String, decoration: String, films: Int, venues: Int, titles: Int, examples: Seq[String],
                           measured: Option[String] = scala.None)

  /** The artefact `scripts.IdentityDecorationsLearn` writes: the decorations with provenance. */
  final case class Artefact(version: String, basis: String, inputs: Map[String, Int], decorations: Seq[Learned]) {
    def decorationsOf: TitleDecorations = {
      def of(side: String) = decorations.filter(_.side == side).map(d => d.decoration.split(" ").toSeq).toSet
      TitleDecorations(of("prefix"), of("suffix"), of(Tail))
    }
  }
  implicit val learnedFormat: OFormat[Learned]   = Json.format[Learned]
  implicit val artefactFormat: OFormat[Artefact] = Json.format[Artefact]

  val ResourcePath = "identity-decorations.json"

  /** The side a learned event tail is filed under: what a title bills LAST, after a spaced "+". */
  val Tail = "tail"
  /** The longest tail run learned: "spotkanie z" leads "… z reżyserem", "… z reżyserką" and "… z Janem Englertem" alike. */
  private[identity] val TailRun = 3
  private[identity] val LastBillJoin = """\s\+\s(?!.*\s\+\s)""".r

  def fromResource(path: String): Option[Artefact] =
    ClasspathResource.json[Artefact](path)

  /** The resolver's decorations: the artefact on the classpath. */
  lazy val resolver: TitleDecorations =
    fromResource(ResourcePath).map(_.decorationsOf).getOrElse(throw new IllegalStateException(s"$ResourcePath is not on the classpath"))

  /** "Recurs around many inner titles": three different remainders — what the offline candidate learner
   *  (`TitleDecorationsLearner.candidates`) asks of a run, and what `CorpusContext.TagSpread` reads. */
  val MinCandidateRemainders = 3

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
