package services.identity

import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.util.Using

/**
 * THE HAND JUDGEMENTS OF TAKES as the live worker reads them: `test/resources/fixtures/identity-judgements.tsv` (its
 * columns and states: `tools.IdentityJudgements`, the gates' reader), bundled into the worker as [[Resource]] so its
 * suspect-take count (`services.identity.adapters.SuspectTakeCount`) counts only the suspects no one has judged, and
 * the takes a judgement calls wrong apart — and the resolver denies a listing every TMDB film a judgement calls wrong for
 * its billing ([[Index.denied]], through [[PinConstraints]]), as an operator's never-this-film pin does: a wrong
 * judgement corrects the take, not only labels it. An input like the pins, read whole on every resolve.
 *
 * [[decide]] is THE rule a set of judgements gives a take — the gates' (`tools.UnmatchedClusters.verdict`) and the
 * worker's alike.
 */
object TakeJudgements {

  /** One row: `film` ("tmdb:913760", "imdb:tt16315948") is `right` or wrong for `rawTitle` at `venue` ("*": every venue
   *  billing it). An undecided row judges nothing. */
  final case class Judgement(country: String, venue: String, rawTitle: String, film: String, right: Boolean)

  /** A billing as every judgement keys it: runs of whitespace one space, none at the ends. */
  def normalise(raw: String): String =
    if (raw.isEmpty || (!space(raw.head) && !space(raw.last) && !hasOtherSpace(raw))) raw
    else raw.replaceAll("(?U)\\s+", " ").stripPrefix(" ").stripSuffix(" ")

  /** What `(?U)\s` matches: a no-break space too. */
  private def space(c: Char): Boolean = Character.isWhitespace(c) || Character.isSpaceChar(c)

  /** Whether `raw` holds whitespace other than single plain spaces: a resolve asks this of every listing it denies. */
  private def hasOtherSpace(raw: String): Boolean = {
    var i = 0
    var other = false
    while (!other && i < raw.length) {
      val c = raw.charAt(i)
      other = space(c) && (c != ' ' || (i + 1 < raw.length && space(raw.charAt(i + 1))))
      i += 1
    }
    other
  }

  /** A take's verdict by the judgements covering its listing (`covering`: its country and billing, its venue or "*"):
   *  `Some(false)` when one denies a film it is known by (`ids`), or names its right film in a database the take has an id
   *  in and this is not it; `Some(true)` when one calls a film it is known by right; else `None`. The take's own venue's
   *  judgement calling a film right overrules every venue's calling it wrong: PL Kino Iluzjon's "Lalka" is Has' 1968 film,
   *  the bare "Lalka" everywhere else Kawalski's 2026 one. */
  def decide(ids: Set[String], covering: Seq[Judgement]): Option[Boolean] = {
    val rightHere = covering.filter(j => j.venue != "*" && j.right).map(_.film).toSet
    val about     = covering.filterNot(j => j.venue == "*" && !j.right && rightHere(j.film))
    val databases = ids.map(_.takeWhile(_ != ':'))
    if (about.exists(j => !j.right && ids(j.film))) Some(false)
    else if (about.exists(j => j.right && ids(j.film))) Some(true)
    else if (about.exists(j => j.right && databases(j.film.takeWhile(_ != ':')))) Some(false)
    else None
  }

  /** The bundled judgements' classpath resource. */
  val Resource = "/identity-judgements.tsv"

  /** The rows of the judgements file's lines that judge a film right or wrong. */
  def parse(lines: Iterator[String]): Seq[Judgement] =
    lines.filterNot(line => line.trim.isEmpty || line.startsWith("#") || line.startsWith("country\t")).map(_.split("\t", -1)).collect {
      case Array(cc, venue, raw, film, verdict, _*) if verdict == "right" || verdict == "wrong" => Judgement(cc, venue, raw, film, verdict == "right")
    }.toSeq

  /** One country's judgements, by billing. Equal by its rows, so a resolve's rules version
   *  ([[IncrementalResolver.rulesVersion]], through [[PinConstraints]]) moves exactly when they do. */
  final case class Index(judgements: Seq[Judgement]) {
    private lazy val byBilling = judgements.groupBy(j => normalise(j.rawTitle))
    def size: Int = judgements.size
    private def covering(venue: String, rawTitle: String): Seq[Judgement] =
      if (judgements.isEmpty) Nil
      else byBilling.get(normalise(rawTitle)).fold(Seq.empty[Judgement])(_.filter(j => j.venue == "*" || j.venue == venue))
    /** The verdict on `rawTitle` at `venue` taken as a film known by `ids`. */
    def verdict(venue: String, rawTitle: String, ids: Set[String]): Option[Boolean] =
      Option(covering(venue, rawTitle)).filter(_.nonEmpty).flatMap(decide(ids, _))
    /** The TMDB films a judgement calls wrong for `rawTitle` at `venue`: its venue's or every venue's, unless its venue's
     *  calls the film right ([[decide]]'s precedence). What the resolver denies the listing, as a never-this-film pin. */
    def denied(venue: String, rawTitle: String): Set[Int] = {
      val rows = covering(venue, rawTitle)
      if (rows.isEmpty) Set.empty
      else {
        val rightHere = rows.filter(j => j.venue != "*" && j.right).map(_.film).toSet
        rows.iterator.filter(j => !j.right && !rightHere(j.film) && j.film.startsWith("tmdb:")).flatMap(_.film.stripPrefix("tmdb:").toIntOption).toSet
      }
    }
  }

  object Index {
    val Empty: Index = Index(Nil)
    /** `country`'s bundled judgements: none when the worker was built without them. */
    def bundled(country: String): Index =
      Option(getClass.getResourceAsStream(Resource)).fold(Empty)(stream =>
        Index(Using.resource(Source.fromInputStream(stream, StandardCharsets.UTF_8.name()))(source => parse(source.getLines())).filter(_.country == country)))
  }
}
