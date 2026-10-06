package services.review

import services.movies.ListingKey

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant

/** What a reviewer said about a cluster. `Undo` withdraws the cluster's previous answer. */
enum ReviewVerdict(val code: String, val label: String) {
  /** The film the card shows (the resolver's match, or its best candidate no veto denied) is the cluster's film. */
  case Right       extends ReviewVerdict("right", "Right")
  /** The film the card shows is NOT the cluster's film. */
  case Wrong       extends ReviewVerdict("wrong", "Wrong")
  /** The cluster's film is the one `ref` names ("This film" on a candidate, or "Other film"). */
  case Film        extends ReviewVerdict("film", "Other film")
  /** None of the candidates shown is the cluster's film. */
  case NoneOfThese extends ReviewVerdict("none", "None of these")
  /** The cluster bills an event, not a film. */
  case Event       extends ReviewVerdict("event", "Not a film")
  /** The cluster bills two films or more at once. */
  case Bill        extends ReviewVerdict("bill", "Double bill")
  case Undo        extends ReviewVerdict("undo", "Undo")
}

object ReviewVerdict {
  def byCode(code: String): Option[ReviewVerdict] = values.find(_.code == code)
}

/** Which review page an answer was given on. */
enum ReviewPage(val code: String, val title: String) {
  case Queue     extends ReviewPage("queue", "Queue (unmatched)")
  case Matchable extends ReviewPage("matchable", "Matchable but unmatched")
  case Recent    extends ReviewPage("recent", "Recently matched")
}

object ReviewPage {
  /** The published review pages before these named the queue `review`. */
  def byCode(code: String): Option[ReviewPage] =
    if (code == "review") Some(Queue) else values.find(_.code == code)
}

/** One listing of a cluster, as an answer remembers it: enough to tell it again after the cluster's
 *  membership moves, and to write its `labels.tsv` row. `year`/`directors` are what the VENUE stated. */
final case class ReviewMember(venue: String, rawTitle: String, page: Option[String],
                              year: Option[Int] = None, directors: Seq[String] = Nil) {
  /** The listing this member is: its venue and page, or its venue and raw title when it has no page. */
  def identity: String = venue + "\u0000" + page.getOrElse(rawTitle)
}

object ReviewMember {
  def of(key: ListingKey): ReviewMember = key match {
    case ListingKey.Native(venue, page, raw)          => ReviewMember(venue, raw, Some(page))
    case ListingKey.Published(venue, raw, year, dirs) => ReviewMember(venue, raw, None, year, dirs)
  }
}

/** A film as an answer remembers it: its ref and the facts a contradiction is checked against. */
final case class FilmFacts(ref: FilmRef, title: Option[String] = None, year: Option[Int] = None, directors: Seq[String] = Nil) {
  def describe: String = title.fold(ref.render)(t => t + year.fold("")(y => s" ($y)"))
}

/**
 * One answer, as stored. Answers are never rewritten: a changed mind is a newer answer for the same
 * cluster, and `Undo` withdraws the latest — so the whole history stays readable.
 *
 * @param clusterId [[ReviewClusterId]] of the members as the card showed them
 * @param shown     the film the card put forward (the match, the best candidate no veto denied, or the labelled film)
 * @param warnings  where the venue's own facts contradict the film the answer chose
 * @param legacyId  the item id of an answer imported from the hand-built review pages
 */
final case class ReviewAnswer(clusterId: String, country: String, page: ReviewPage, verdict: ReviewVerdict,
                              ref: Option[FilmRef], shown: Option[FilmFacts], title: String, members: Seq[ReviewMember],
                              who: String, at: Instant, warnings: Seq[String] = Nil, legacyId: Option[String] = None) {
  /** The film this answer says the cluster IS, if it names one. */
  def chosen: Option[FilmRef] = verdict match {
    case ReviewVerdict.Right => shown.map(_.ref)
    case ReviewVerdict.Film  => ref
    case _                   => None
  }
}

/** A cluster's stable id: a digest of its members' identities, whatever order the model lists them in. */
object ReviewClusterId {
  def of(members: Seq[ReviewMember]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
      .digest(members.map(_.identity).distinct.sorted.mkString("\u0001").getBytes(StandardCharsets.UTF_8))
    digest.take(8).map(b => f"${b & 0xff}%02x").mkString
  }

  def ofKeys(keys: Seq[ListingKey]): String = of(keys.map(ReviewMember.of))
}
