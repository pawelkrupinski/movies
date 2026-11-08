package services.identity

/**
 * THE HAND JUDGEMENTS OF TAKES as the live worker reads them: `test/resources/fixtures/identity-judgements.tsv` (its
 * columns and states: `tools.IdentityJudgements`, the gates' reader), bundled into the worker and handed in by its
 * composition root (`modules.wiring.BundledJudgements`) so its
 * suspect-take count (`services.identity.adapters.SuspectTakeCount`) counts only the suspects no one has judged, and
 * the takes a judgement calls wrong apart; and the projection withdraws each listing's take a judgement calls wrong
 * ([[withdraw]]), after the model and the agreement stage decided: a wrong judgement corrects the take.
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

  /** Whether `raw` holds whitespace other than single plain spaces: every listing a projection takes is asked. */
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
    val rightHere = rightAtTheVenue(covering)
    val about     = covering.filterNot(j => j.venue == "*" && !j.right && rightHere(j.film))
    val databases = ids.map(_.takeWhile(_ != ':'))
    if (about.exists(j => !j.right && ids(j.film))) Some(false)
    else if (about.exists(j => j.right && ids(j.film))) Some(true)
    else if (about.exists(j => j.right && databases(j.film.takeWhile(_ != ':')))) Some(false)
    else None
  }

  /** The films a venue's own judgement among `covering` calls right: they overrule every venue's calling them wrong. */
  private def rightAtTheVenue(covering: Seq[Judgement]): Set[String] = covering.filter(j => j.venue != "*" && j.right).map(_.film).toSet

  /** The rows of the judgements file's lines that judge a film right or wrong. */
  def parse(lines: Iterator[String]): Seq[Judgement] =
    lines.filterNot(line => line.trim.isEmpty || line.startsWith("#") || line.startsWith("country\t")).map(_.split("\t", -1)).collect {
      case Array(cc, venue, raw, film, verdict, _*) if verdict == "right" || verdict == "wrong" => Judgement(cc, venue, raw, film, verdict == "right")
    }.toSeq

  /** One country's judgements, by billing. */
  final class Index(judgements: Seq[Judgement]) {
    private val byBilling = judgements.groupBy(j => normalise(j.rawTitle))
    def size: Int = judgements.size
    /** The judgements covering `rawTitle` at `venue`: its venue's and every venue's ("*"); `None` for a billing none judges. */
    private def covering(venue: String, rawTitle: String): Option[Seq[Judgement]] =
      byBilling.get(normalise(rawTitle)).map(_.filter(j => j.venue == "*" || j.venue == venue))
    /** The verdict on `rawTitle` at `venue` taken as a film known by `ids`. */
    def verdict(venue: String, rawTitle: String, ids: Set[String]): Option[Boolean] =
      covering(venue, rawTitle).flatMap(decide(ids, _))
    /** The TMDB films a judgement calls wrong for `rawTitle` at `venue`: its venue's or every venue's, unless its venue's
     *  calls the film right ([[decide]]'s precedence). */
    def denied(venue: String, rawTitle: String): Set[Int] =
      if (judgements.isEmpty) Set.empty
      else covering(venue, rawTitle).fold(Set.empty[Int]) { rows =>
        val rightHere = rightAtTheVenue(rows)
        rows.iterator.filter(j => !j.right && !rightHere(j.film) && j.film.startsWith("tmdb:")).flatMap(_.film.stripPrefix("tmdb:").toIntOption).toSet
      }
  }

  /** `resolution` with every listing taken as a TMDB film a judgement calls wrong for it WITHDRAWN: the judged listings of
   *  a decision taken out of it TOGETHER into one of their own, no film, [[ResolverDecision.Basis.Withdrawn]] (one decision
   *  a listing split one Paris Opera ballet into sixteen films at sixteen Multikinos, Verify scrape recording #17: a
   *  regrouping no judgement says) — the judged listings' take and nothing else, as a pin acts on its own listings: no
   *  other listing of its decision, family or title moves, and nothing is
   *  decided again (2026-10-28: read as the resolver's evidence instead, a denial moved the film other venues of the
   *  title pooled to — Münster's "Transit", Ilkley's "Fallen Angels"). A pure function of the resolution and the
   *  judgements, applied whole to every projection: no stickiness. */
  def withdraw(resolution: Resolution, index: Index): Resolution =
    if (index.size == 0) resolution
    else {
      var moved = false
      val decisions = resolution.decisions.flatMap { d =>
        d.film.fold(Seq(d)) { film =>
          val (judged, kept) = d.members.partition(k => index.denied(k.venue, k.rawTitle)(film))
          if (judged.isEmpty) Seq(d)
          else {
            moved = true
            val line = s"withdrawn: a hand judgement calls tmdb:$film wrong for this billing (identity-judgements.tsv)"
            Option.when(kept.nonEmpty)(d.copy(members = kept)(d.trace)).toSeq :+
              d.copy(members = judged, film = None, basis = ResolverDecision.Basis.Withdrawn, explanation = d.explanation :+ line,
                fallback = None, agreed = Map.empty)(d.trace.copy(stage = Some(WithdrawnRule)))
          }
        }
      }
      if (moved) resolution.copy(decisions = decisions) else resolution
    }

  /** The rule a judged listing's withdrawal is traced under. */
  val WithdrawnRule = "withdrawal:judged-wrong"

  object Index {
    val Empty: Index = new Index(Nil)
  }
}
