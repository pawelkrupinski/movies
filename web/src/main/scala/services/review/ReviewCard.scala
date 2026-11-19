package services.review

import play.api.libs.json.JsObject
import java.time.Instant

/** A cluster as a review card renders it. `shown` is the film put forward (the match or best surviving candidate);
 *  `updatedAt` the recently-matched page's stand-in for when it was decided. */
final case class ReviewCard(cluster: ReviewCluster, members: Seq[MemberView], films: Map[Int, FilmCard],
                            labels: Seq[LabelRow], answer: Option[ReviewAnswer], updatedAt: Option[Instant],
                            // the queue's: what an answer on the card settles, and why the resolver is unsure of it
                            priority: Option[ReviewPriority] = None,
                            // the Agent-judged tab's: the take awaiting the user this card is of, the other judgements of its
                            // billing, and the identity-judge-cards evidence for it
                            item: Option[DigestItem] = None, others: Seq[LabelRow] = Nil, judgeCard: Option[String] = None) {
  def shown: Option[Int] = cluster.shown

  /** The card ranked as the queue ranks it: what an answer on it settles, and why the resolver is unsure of it. */
  def withPriority(ranked: Option[ReviewPriority]): ReviewCard = copy(priority = ranked)

  /** The card as the Agent-judged tab shows `judged`: the take awaiting the user, the billing's `others` judgements,
   *  its judge-cards `evidence`, and the film judged as the row names it where the mirror holds no record of it. */
  def awaiting(judged: DigestItem, others: Seq[LabelRow], evidence: Option[String]): ReviewCard =
    copy(item = Some(judged), others = others, judgeCard = evidence, films = judged.film.tmdb.filterNot(films.contains).fold(films) { id =>
      films + (id -> FilmCard(id, None, judged.shown.title, None, judged.shown.year, Nil, None, None, None))
    })
  def filmFacts(tmdb: Int): FilmFacts = films.get(tmdb).fold(FilmFacts(FilmRef.tmdb(tmdb)))(_.facts)
  lazy val reviewMembers: Seq[ReviewMember] = members.map(_.member)
  /** Where the venues' own facts contradict the film the card puts forward. */
  lazy val disagreements: Seq[Disagreement] = shown.toSeq.flatMap(film => FactCheck.disagreements(reviewMembers, filmFacts(film)))

  /** The candidates that survived their vetoes the card lists below the film it puts forward. */
  lazy val otherCandidates: Seq[ReviewCandidate] = cluster.candidates.filter(_.survives).filterNot(c => shown.contains(c.film))
  /** The vetoed candidates, folded apart on the card: still answerable, never put forward. */
  def vetoedCandidates: Seq[ReviewCandidate] = cluster.vetoedCandidates

  /** What the members themselves say, and what a feed's catalogue copied onto them ([[MemberClaims]]). */
  lazy val claims: MemberClaims = MemberClaims(members)

  /** What the page's answer buttons post back: the cluster as the card showed it ([[ReviewCardPayload]]). */
  def payload(page: ReviewPage): JsObject = ReviewCardPayload.of(this, page)
}
