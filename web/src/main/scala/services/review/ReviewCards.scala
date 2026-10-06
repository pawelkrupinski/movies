package services.review

import play.api.libs.json.{JsObject, Json}
import services.movies.ListingKey

import java.time.Instant

/** One member listing of a card: its listing row, its own film page, and the catalogue ids its feed names it by.
 *  When the feed is an aggregator's that copies its catalogue entry's facts onto the listing
 *  ([[services.identity.CatalogueSources.feedStated]] — Webedia's ids, kinoprogramm.com's pages), the listing row's
 *  year, directors, running time and poster are THAT CATALOGUE's claim, not the venue's, and are shown and weighed so. */
final case class MemberView(key: ListingKey, slot: Option[SlotFacts], page: Option[VenueFacts], feed: Option[ListingFeed]) {
  def venue: String = key.venue
  def pageUrl: Option[String] = key match { case ListingKey.Native(_, page, _) => Some(page); case _ => None }
  def factsFromCatalogue: Boolean =
    services.identity.CatalogueSources.feedStated(feed.toSeq.flatMap(_.catalogueIds), pageUrl)
  /** The year and directors the VENUE stated — its key's, else its listing row's, else its own page's — and none
   *  when its listing's facts are a feed catalogue's: those are no venue's statement to check an answer against. */
  def member: ReviewMember = {
    val base = ReviewMember.of(key)
    if (factsFromCatalogue) base.copy(year = None, directors = Nil)
    else {
      val own = slot.map(_.facts).toSeq ++ page.toSeq
      base.copy(year = base.year.orElse(own.flatMap(_.year).headOption),
        directors = if (base.directors.nonEmpty) base.directors else own.map(_.directors).find(_.nonEmpty).getOrElse(Nil))
    }
  }
  /** The venue's poster: its listing row's, else its last scrape's listing (`identity_listings`), else its own film page's —
   *  never a site-wide default image ([[tools.SiteDefaultImage]]) taken for one. */
  def poster: Option[String]   =
    (slot.flatMap(_.facts.poster) ++ feed.flatMap(_.poster) ++ page.flatMap(_.poster)).find(p => !tools.SiteDefaultImage(p))
  def synopsis: Option[String] = slot.flatMap(_.facts.synopsis).orElse(page.flatMap(_.synopsis))
}

/** A cluster as a review card renders it. `shown` is the film put forward (the match or best candidate);
 *  `updatedAt` the recently-matched page's stand-in for when it was decided. */
final case class ReviewCard(cluster: ReviewCluster, members: Seq[MemberView], films: Map[Int, FilmCard],
                            labels: Seq[LabelRow], answer: Option[ReviewAnswer], updatedAt: Option[Instant]) {
  def shown: Option[Int] = cluster.shown
  def filmFacts(tmdb: Int): FilmFacts = films.get(tmdb).fold(FilmFacts(FilmRef.tmdb(tmdb)))(_.facts)
  def reviewMembers: Seq[ReviewMember] = members.map(_.member)
  /** Where the venues' own facts contradict the film the card puts forward. */
  def disagreements: Seq[Disagreement] = shown.toSeq.flatMap(film => FactCheck.disagreements(reviewMembers, filmFacts(film)))

  /** The candidates the card lists below the film it puts forward. */
  def otherCandidates: Seq[ReviewCandidate] = cluster.candidates.filterNot(c => shown.contains(c.film))

  /** The venues' posters, each once — a venue's own before a feed catalogue's copy: the card shows the first that loads. */
  def posters: Seq[String]     = members.sortBy(_.factsFromCatalogue).flatMap(_.poster).distinct
  /** The venue's synopsis: a venue's own before a feed catalogue's copy. */
  def synopsis: Option[String] = members.sortBy(_.factsFromCatalogue).flatMap(_.synopsis).headOption

  /** What the venues themselves say, merged across members — the first stated value per fact wins — with a
   *  feed catalogue's copied claims left to [[catalogueSays]]. Screenings are every member's: no catalogue claims those. */
  def venueSays: Seq[(String, String)] = {
    val own    = members.filterNot(_.factsFromCatalogue)
    val stated = own.map(_.member)
    val facts  = own.flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq)
    def list(f: VenueFacts => Seq[String]) = facts.map(f).find(_.nonEmpty)
    val feeds  = members.flatMap(_.feed)
    rows(
      "Original title" -> facts.flatMap(_.originalTitle).headOption,
      "Year"           -> stated.flatMap(_.year).headOption.map(_.toString),
      "Director"       -> stated.map(_.directors).find(_.nonEmpty).map(_.mkString(", ")),
      "Cast"           -> list(_.cast).map(c => c.take(8).mkString(", ") + (if (c.size > 8) " …" else "")),
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

  /** What an aggregator's catalogue entry claims, copied onto its listings by the feed — never the venue's word. */
  def catalogueSays: Seq[(String, String)] = {
    val fed   = members.filter(_.factsFromCatalogue)
    val facts = fed.flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq)
    rows(
      "Year"          -> facts.flatMap(_.year).headOption.map(_.toString),
      "Director"      -> facts.map(_.directors).find(_.nonEmpty).map(_.mkString(", ")),
      "Runtime"       -> facts.flatMap(_.runtime).headOption.map(r => s"$r min"),
      "Catalogue ids" -> Some(fed.map(m => m.feed.map(_.catalogue).filter(_.nonEmpty).orElse(m.pageUrl).getOrElse("")).distinct.mkString(", ")))
  }

  private def rows(stated: (String, Option[String])*): Seq[(String, String)] =
    stated.collect { case (name, Some(value)) if value.nonEmpty => name -> value }

  /** What the page's answer buttons post back: the cluster as the card showed it. */
  def payload(page: ReviewPage): JsObject = Json.obj(
    "clusterId" -> cluster.id, "country" -> cluster.country.code, "page" -> page.code, "title" -> cluster.title,
    "members" -> reviewMembers.map(m => Json.obj("venue" -> m.venue, "rawTitle" -> m.rawTitle, "page" -> m.page,
      "year" -> m.year, "directors" -> m.directors)),
    "shown" -> shown.map(filmJson),
    "films" -> (shown.toSeq ++ cluster.candidates.map(_.film)).distinct.map(filmJson))

  private def filmJson(tmdb: Int): JsObject = {
    val f = filmFacts(tmdb)
    Json.obj("ref" -> f.ref.render, "title" -> f.title, "year" -> f.year, "directors" -> f.directors)
  }
}

/** Builds the cards of the clusters on screen — and reads only theirs. */
object ReviewCards {
  def build(source: ReviewSource, clusters: Seq[(ReviewCluster, Option[Instant])], labels: Seq[LabelRow],
            answers: ReviewAnswers.Index): Seq[ReviewCard] = {
    val keys   = clusters.flatMap(_._1.members)
    val slots  = source.slots(keys.map(ListingKey.serialised))
    val pages  = source.venuePages(keys.collect { case ListingKey.Native(_, page, _) => page })
    val feeds  = source.feeds(keys.map(k => k.venue -> k.rawTitle))
    val ids    = clusters.flatMap { case (c, _) => c.film.toSeq ++ c.candidates.map(_.film) }.distinct
    val films  = withRecords(source.films(ids), source.filmRecords(ids))
    clusters.map { case (cluster, at) =>
      val members = cluster.members.map(k => MemberView(k, slots.get(ListingKey.serialised(k)),
        k match { case ListingKey.Native(_, page, _) => pages.get(page); case _ => None }, feeds.get(k.venue -> k.rawTitle)))
      val code = cluster.country.code
      val overlay = labels.filter(l => l.country == code &&
        cluster.members.exists(m => m.rawTitle == l.rawTitle && (l.venue == "*" || l.venue == m.venue)))
      ReviewCard(cluster, members, films, overlay, answers.answerFor(cluster.id, cluster.reviewMembers), at)
    }
  }

  /** Each film as the resolver weighed it — its stored TMDB record's title, year, directors and running time — with the
   *  corpus's poster and overview (never a live TMDB call); the corpus's facts only where no record states them. */
  private[review] def withRecords(corpus: Map[Int, FilmCard], records: Map[Int, FilmCard]): Map[Int, FilmCard] =
    (corpus.keySet ++ records.keySet).toSeq.flatMap { tmdb =>
      ((records.get(tmdb), corpus.get(tmdb)) match {
        case (Some(r), Some(c)) => Some(FilmCard(tmdb, c.imdb.orElse(r.imdb), r.title.orElse(c.title), r.originalTitle.orElse(c.originalTitle),
          r.year.orElse(c.year), if (r.directors.nonEmpty) r.directors else c.directors, r.runtime.orElse(c.runtime), c.poster, c.overview))
        case (r, c) => r.orElse(c)
      }).map(tmdb -> _)
    }.toMap
}
