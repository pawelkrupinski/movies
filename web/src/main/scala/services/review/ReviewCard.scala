package services.review

import play.api.libs.json.{JsObject, Json}
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

  /** The venues' posters, each once — a venue's own before a feed catalogue's copy: the card shows the first that loads. */
  lazy val posters: Seq[String]     = members.sortBy(_.factsFromCatalogue).flatMap(_.poster).distinct
  /** The venue's synopsis: a venue's own before a feed catalogue's copy. */
  lazy val synopsis: Option[String] = members.sortBy(_.factsFromCatalogue).flatMap(_.synopsis).headOption

  /** What the venues themselves say, merged across members — the first stated value per fact wins — with a
   *  feed catalogue's copied claims left to [[catalogueSays]]. Screenings are every member's: no catalogue claims those. */
  lazy val venueSays: Seq[(String, String)] = {
    val own    = members.filterNot(_.factsFromCatalogue)
    val stated = own.map(_.member)
    val facts  = own.flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq)
    def list(f: VenueFacts => Seq[String]) = facts.map(f).find(_.nonEmpty)
    val feeds  = members.flatMap(_.feed)
    rows(
      "Original title" -> facts.flatMap(_.originalTitle).headOption,
      "Year"           -> stated.flatMap(_.year).headOption.map(_.toString),
      "Director"       -> stated.map(_.directors).find(_.nonEmpty).map(_.mkString(", ")),
      "Cast"           -> Option(venueCast.mkString(", ")),
      "Runtime"        -> facts.flatMap(_.runtime).headOption.map(r => s"$r min"),
      "Country"        -> list(_.countries).map(_.mkString(", ")),
      "Catalogue ids"  -> Some(own.flatMap(_.feed).filter(_.catalogueIds.nonEmpty).map(_.catalogue).distinct.mkString(", ")),
      "Screenings"     -> Option.when(feeds.nonEmpty) {
        val span = (feeds.flatMap(_.first).minOption, feeds.flatMap(_.last).maxOption) match {
          case (Some(a), Some(b)) if a != b => s" · $a → $b"
          case (a, b)                       => a.orElse(b).fold("")(" · " + _)
        }
        s"${feeds.map(_.screenings).sum}$span"
      })
  }

  /** The cast the venues themselves list — the first member's that lists one; the page cuts it short ([[ReviewCards.CastShown]]). */
  lazy val venueCast: Seq[String] =
    members.filterNot(_.factsFromCatalogue).flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq).map(_.cast).find(_.nonEmpty).getOrElse(Nil)
  /** The cast an aggregator's catalogue entry lists, copied onto its listings by the feed. */
  lazy val catalogueCast: Seq[String] =
    members.filter(_.factsFromCatalogue).flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq).map(_.cast).find(_.nonEmpty).getOrElse(Nil)

  /** What an aggregator's catalogue entry claims, copied onto its listings by the feed — never the venue's word. */
  lazy val catalogueSays: Seq[(String, String)] = {
    val fed   = members.filter(_.factsFromCatalogue)
    val facts = fed.flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq)
    rows(
      "Year"          -> facts.flatMap(_.year).headOption.map(_.toString),
      "Director"      -> facts.map(_.directors).find(_.nonEmpty).map(_.mkString(", ")),
      "Cast"          -> Option(catalogueCast.mkString(", ")),
      "Runtime"       -> facts.flatMap(_.runtime).headOption.map(r => s"$r min"),
      "Catalogue ids" -> Some(fed.map(m => m.feed.map(_.catalogue).filter(_.nonEmpty).orElse(m.pageUrl).getOrElse("")).distinct.mkString(", ")))
  }

  private def rows(stated: (String, Option[String])*): Seq[(String, String)] =
    stated.collect { case (name, Some(value)) if value.nonEmpty => name -> value }

  /** What the page's answer buttons post back: the cluster as the card showed it. */
  def payload(page: ReviewPage): JsObject = item.fold(clusterPayload(page))(_.payload)

  private def clusterPayload(page: ReviewPage): JsObject = Json.obj(
    "clusterId" -> cluster.id, "country" -> cluster.country.code, "page" -> page.code, "title" -> cluster.title,
    // a listing the venues share is named once with its venues: a film at hundreds of venues of one aggregator page
    // otherwise repeated its every fact hundreds of times, most of a recently-matched page's weight
    "listings" -> reviewMembers.groupBy(m => (m.rawTitle, m.page, m.year, m.directors)).toSeq
      .sortBy { case (_, ms) => reviewMembers.indexOf(ms.head) }.map { case ((raw, page, year, directors), ms) =>
        Json.obj("rawTitle" -> raw, "page" -> page, "year" -> year, "directors" -> directors, "venues" -> ms.map(_.venue)) },
    "shown" -> shown.map(filmJson),
    "films" -> (shown.toSeq ++ cluster.candidates.map(_.film)).distinct.map(filmJson))

  private def filmJson(tmdb: Int): JsObject = {
    val f = filmFacts(tmdb)
    Json.obj("ref" -> f.ref.render, "title" -> f.title, "year" -> f.year, "directors" -> f.directors)
  }
}
