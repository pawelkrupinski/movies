package services.review

import play.api.libs.json.{JsObject, Json}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * One take awaiting the user's word on the Agent-judged tab (`/debug/review/agent-judged`): an agent's judgement in
 * `identity-judgements.tsv` (its evidence marked [[ReviewDigest.agentJudged]]), or a take an agent flagged without
 * judging it (`identity-review-candidates.tsv`). The answer is a review answer like any other ([[payload]]): its export
 * writes the `review page:` row that takes the item off the digest.
 *
 * @param agentVerdict what the agent judged (`right` / `wrong`), none for a candidate
 * @param flagged      the date the agent judged or flagged it
 */
final case class DigestItem(country: String, venue: String, rawTitle: String, film: FilmRef, filmTitle: Option[String],
                            agentVerdict: Option[String], flagged: String, evidence: String) {
  def members: Seq[ReviewMember] = Seq(ReviewMember(venue, rawTitle, None))

  /** One id per listing AND film: two films judged for one billing are two items, each answered on its own. */
  def id: String = ReviewClusterId.of(members :+ ReviewMember(ReviewDigest.FilmMember, film.render, None))

  /** The film as the answer remembers it: `filmTitle` read as "Title (year)". */
  def shown: FilmFacts = filmTitle match {
    case Some(ReviewDigest.TitleYear(title, year)) => FilmFacts(film, Some(title), Some(year.toInt))
    case other                                     => FilmFacts(film, other)
  }

  /** As a judgement row keys it: venue (or `*`), billing and film. */
  def asLabel: LabelRow = LabelRow(country, venue, LabelsTsv.normalise(rawTitle), film.render, "", "")

  /** What the page posts to `/debug/review/answer`: the card [[AnswerRequest]] reads. */
  def payload: JsObject = Json.obj(
    "clusterId" -> id, "country" -> country, "page" -> ReviewPage.Digest.code, "title" -> rawTitle,
    "shown"     -> (Json.obj("ref" -> film.render) ++ shown.title.fold(Json.obj())(t => Json.obj("title" -> t)) ++
      shown.year.fold(Json.obj())(y => Json.obj("year" -> y))),
    "listings"  -> Json.arr(Json.obj("venues" -> Json.arr(venue), "rawTitle" -> rawTitle)))
}

object ReviewDigest {
  /** Beside `identity-judgements.tsv`: the takes agents flagged for the user without judging them. */
  val CandidatesFile = "identity-review-candidates.tsv"
  val CandidatesHeader = "country\tvenue\trawTitle\tfilm\tfilmTitle\tflagged\tevidence"

  def candidatesPath(labels: Path): Path = labels.resolveSibling(CandidatesFile)

  private[review] val FilmMember = "\u0002film"
  private[review] val TitleYear  = """^(.+) \((\d{4})\)$""".r

  /** The marker an agent's judgement carries in its evidence: `AGENT-JUDGED` (the skill's spelling), and the spellings
   *  agents used before it — "agent judged 2026-10-22 (…)", "judged 10-22 (agent, …)". */
  private val AgentMarker = """(?i)\bagent[- ]judged\b|\(agent[,)]""".r

  /** The user's own word on a row: the review pages' export, or a hand label. */
  def userJudged(evidence: String): Boolean = evidence.startsWith("review page:") || evidence.startsWith("hand label:")

  /** An agent's judgement the user has not confirmed or overruled yet. */
  def agentJudged(evidence: String): Boolean = AgentMarker.findFirstIn(evidence).isDefined && !userJudged(evidence)

  def readCandidates(path: Path): Seq[DigestItem] =
    if (!Files.exists(path)) Nil
    else Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq
      .filter(_.trim.nonEmpty).filterNot(_.startsWith("#")).filterNot(_ == CandidatesHeader).map { line =>
        line.split("\t", -1) match {
          case Array(country, venue, raw, film, filmTitle, flagged, evidence) =>
            val ref = FilmRef.formed(film).getOrElse(throw new IllegalArgumentException(s"$CandidatesFile: not a film ref: $film"))
            DigestItem(country, venue, raw, ref, Option(filmTitle).filter(_.nonEmpty), None, flagged, evidence)
          case other => throw new IllegalArgumentException(s"$CandidatesFile: ${other.length} fields, want 7: $line")
        }
      }

  /** Every item awaiting the user: each agent-judged row, and each candidate no row judges yet (an agent's row for the
   *  same take stands for it, any other row has judged it), less those `answered` on the digest since the last export —
   *  by country, then billing. */
  def pending(labels: Seq[LabelRow], candidates: Seq[DigestItem], answered: Set[String]): Seq[DigestItem] = {
    val agents = labels.filter(r => agentJudged(r.note)).flatMap(r => FilmRef.formed(r.film).map(f =>
      DigestItem(r.country, r.venue, r.rawTitle, f, Option(r.filmTitle).filter(_.nonEmpty), Some(r.verdict), r.judged, r.note)))
    val open   = candidates.filterNot(c => labels.exists(_.sameJudgement(c.asLabel)))
    (agents ++ open).filterNot(i => answered(i.id)).sortBy(i => (i.country, i.rawTitle.toLowerCase(java.util.Locale.ROOT), i.venue, i.film.render))
  }

  /** The items of one kind: `agent` (an agent's judgement) or `candidate` (flagged, not judged); any other value (or none) is all. */
  def ofKind(items: Seq[DigestItem], kind: Option[String]): Seq[DigestItem] = kind match {
    case Some(Kinds.Agent)     => items.filter(_.agentVerdict.isDefined)
    case Some(Kinds.Candidate) => items.filter(_.agentVerdict.isEmpty)
    case _                     => items
  }

  object Kinds {
    val Agent = "agent"
    val Candidate = "candidate"
  }

  /** The other judgements of the item's billing: other venues' and other films' rows, for its card. */
  def otherJudgements(labels: Seq[LabelRow], item: DigestItem): Seq[LabelRow] = {
    val title = LabelsTsv.normalise(item.rawTitle)
    labels.filter(r => r.country == item.country && LabelsTsv.normalise(r.rawTitle) == title && !r.sameJudgement(item.asLabel))
  }

  /** Where `scripts/identity-judge-cards` writes its cards, in the checkout the labels file belongs to. */
  def judgeCardsPath(labels: Path): Path = {
    // the checkout is four levels up (test/resources/fixtures/<file>); a file nearer the root stands in the root's own
    val checkout = (1 to 4).foldLeft(labels.toAbsolutePath)((path, _) => Option(path.getParent).getOrElse(path))
    checkout.resolve("target/identity-judge/cards.md")
  }

  private val CardLimit = 3000

  /** The cards (`## ` blocks) of `cards` that name the item's billing and film, joined, as far as a page should show. */
  def judgeCard(cards: String, item: DigestItem): String = {
    val title = LabelsTsv.normalise(item.rawTitle)
    val filmId = item.film.render.split(':').last
    cards.split("\n## ").map(_.trim).filter(b => b.contains(title) && b.contains(filmId)).mkString("\n\n").take(CardLimit)
  }

  /** The cluster the item's card shows: the item's listing (its venue and billing alone, so an answer labels that listing and
   *  no other), with the film judged put forward and the film the model took, if another, among the candidates it lost to.
   *  The model's decision of the listing gives the explanation and the other candidates; a listing the model holds no
   *  decision for gets an empty one. Answered under the item's own id: one listing and film each. */
  def clusterOf(country: models.Country, item: DigestItem, decisions: Seq[services.identity.ResolverDecision]): ReviewCluster = {
    val key      = decisions.iterator.flatMap(_.members).find(k => k.rawTitle == item.rawTitle && (item.venue == "*" || k.venue == item.venue))
    val member   = key.getOrElse(services.movies.ListingKey.Published(if (item.venue == "*") "" else item.venue, item.rawTitle, None, Nil))
    val seed     = key.flatMap(k => decisions.find(_.members.contains(k))).map(ReviewCluster.of(country, _))
      .getOrElse(ReviewCluster(country, Seq(member), None, 0.0, services.identity.ResolverDecision.Basis.NoCandidate, Nil, false, Nil))
    val judged   = item.film.tmdb
    val agrees   = judged.isDefined && seed.film == judged
    val tookOther = seed.film.filterNot(judged.contains).map(ReviewCandidate(_, Some(seed.confidence)))
    val candidates =
      if (agrees) seed.candidates
      else judged.fold(Seq.empty[ReviewCandidate])(film => ReviewCandidate(film, seed.candidates.find(_.film == film).flatMap(_.probability)) +:
        (tookOther.toSeq ++ seed.candidates.filterNot(_.film == film)).distinctBy(_.film))
    seed.copy(members = Seq(member), film = if (agrees) seed.film else None, candidates = candidates, answerKey = Some(item.id))
  }

  /** The digest's answers standing: their items' ids. */
  def answered(current: Seq[ReviewAnswer]): Set[String] = current.filter(_.page == ReviewPage.Digest).map(_.clusterId).toSet
}
