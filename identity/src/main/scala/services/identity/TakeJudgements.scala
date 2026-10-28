package services.identity

/**
 * THE HAND JUDGEMENTS OF TAKES as the live worker reads them: `test/resources/fixtures/identity-judgements.tsv` (its
 * columns and states: `tools.IdentityJudgements`, the gates' reader), bundled into the worker and handed in by its
 * composition root (`modules.wiring.BundledJudgements`) so its
 * suspect-take count (`services.identity.adapters.SuspectTakeCount`) counts only the suspects no one has judged, and
 * the takes a judgement calls wrong apart. Reporting only: no take reads it.
 *
 * [[decide]] is THE rule a set of judgements gives a take — the gates' (`tools.UnmatchedClusters.verdict`) and the
 * worker's alike.
 */
object TakeJudgements {

  /** One row: `film` ("tmdb:913760", "imdb:tt16315948") is `right` or wrong for `rawTitle` at `venue` ("*": every venue
   *  billing it). An undecided row judges nothing. */
  final case class Judgement(country: String, venue: String, rawTitle: String, film: String, right: Boolean)

  /** A billing as every judgement keys it: runs of whitespace one space, none at the ends. */
  def normalise(raw: String): String = raw.replaceAll("(?U)\\s+", " ").stripPrefix(" ").stripSuffix(" ")

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

  /** The rows of the judgements file's lines that judge a film right or wrong. */
  def parse(lines: Iterator[String]): Seq[Judgement] =
    lines.filterNot(line => line.trim.isEmpty || line.startsWith("#") || line.startsWith("country\t")).map(_.split("\t", -1)).collect {
      case Array(cc, venue, raw, film, verdict, _*) if verdict == "right" || verdict == "wrong" => Judgement(cc, venue, raw, film, verdict == "right")
    }.toSeq

  /** One country's judgements, by billing. */
  final class Index(judgements: Seq[Judgement]) {
    private val byBilling = judgements.groupBy(j => normalise(j.rawTitle))
    def size: Int = judgements.size
    /** The verdict on `rawTitle` at `venue` taken as a film known by `ids`. */
    def verdict(venue: String, rawTitle: String, ids: Set[String]): Option[Boolean] =
      byBilling.get(normalise(rawTitle)).flatMap(rows => decide(ids, rows.filter(j => j.venue == "*" || j.venue == venue)))
  }

  object Index {
    val Empty: Index = new Index(Nil)
  }
}
