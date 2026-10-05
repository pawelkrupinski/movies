package services.identity.agreement

import services.identity.{Answer, CatalogueAnswers, CatalogueHit, CatalogueId, CatalogueQuestion, CatalogueSources, Listing}

import scala.collection.mutable

/**
 * The film a cluster's own catalogue ids name ([[services.identity.CatalogueSources]]): the venue's exact naming of it in
 * another film database — Webedia's id, a Letterboxd, Rotten Tomatoes or IMDb link on its film page — mapped by
 * Wikidata's statement of the id (its item's TMDB and IMDb ids), else the source's own page, an IMDb id by TMDB's find.
 *
 * Taken DIRECTLY, not as one vote among the families': an id is no search, so it needs no quorum. Replayed on the
 * unmatched clusters (2026-10-05) as a vote — the agreement's `catalogue` corroboration of a family's pick of the item or
 * IMDb title — it completed one agreement (DE "Mein neues altes Ich", Wikidata's item); taken, it took that and DE "Die
 * Nibelungen - Teil 1: Siegfried" and "Camp der Verlorenen" too, none wrong. It is guarded only as an exact id must be:
 * every id the cluster carries names the same film (its item, TMDB and IMDb ids alike), the record of it — Wikidata's
 * item, or IMDb's title — exists and its year and director do not contradict a listing's own
 * ([[Agreement.contradictedByTheListing]]: a venue linking the wrong film; US "A Night at the Opera", the Marx Brothers'
 * by its Flicks page, is left as the venue credits Edmund Goulding), and no listing bills several works. A stage work's
 * title is no guard here, as it is for a search: an id names its record whatever the title ("Die Nibelungen" is Lang's
 * film by its Filmstarts id, not Wagner's opera).
 */
object Catalogue {

  /** A catalogue take: the TMDB film, else the fallback film it stands on (IMDb's title, else Wikidata's item), and the
   *  explanation line naming the id and the mapping. */
  final case class Taken(film: Option[Int], fallback: Option[(String, String)], title: Option[String], year: Option[Int], line: String)

  /** The one film the listings' catalogue ids name, as the id it is named by (the one stating its Wikidata item, where
   *  one does), whether a venue page linked it, and every hit. */
  final case class Named(id: CatalogueId, linked: Boolean, hit: CatalogueHit, hits: Seq[CatalogueHit]) {
    def item: Option[String] = hits.flatMap(_.item).headOption
    def tmdb: Option[Int]    = hits.flatMap(_.tmdb).headOption
    def imdb: Option[String] = hits.flatMap(_.imdb).headOption
  }

  /** The one film the listings' catalogue ids name — `Known(None)` when they carry no mappable id, or their ids name two
   *  films; `Unknown`, each open question noted in `catalogueAsked`, while one is not answered yet. */
  def named(listings: Seq[Listing], catalogue: CatalogueAnswers, catalogueAsked: mutable.Set[CatalogueQuestion]): Answer[Option[Named]] = {
    var open = false
    def known[A](answer: Answer[A], question: => Unit): Option[A] = answer match {
      case Answer.Known(value) => Some(value)
      case Answer.Unknown      => question; open = true; None
    }
    // the ids each listing carries, then those its venue's page links (marked: the explanation says where it came from)
    val ids: Seq[(CatalogueId, Boolean)] = (listings.flatMap(_.catalogueIds).map(_ -> false) ++ listings.flatMap(_.page).distinct.flatMap { page =>
      known(catalogue.linked(page), catalogueAsked += CatalogueQuestion.Page(page)).getOrElse(Nil).map(_ -> true)
    }).filter { case (id, _) => CatalogueSources.mappable(id) }.distinctBy(_._1)
    val hits: Seq[(CatalogueId, Boolean, CatalogueHit)] = ids.flatMap { case (id, linked) =>
      if (id.source == CatalogueSources.Imdb) Seq((id, linked, CatalogueHit(None, None, Some(id.id), "TMDB's find")))
      else known(catalogue.mapped(id), catalogueAsked += CatalogueQuestion.Id(id)).getOrElse(Nil).map(hit => (id, linked, hit))
    }
    if (open) Answer.Unknown
    else if (hits.isEmpty || !oneFilm(hits.map(_._3))) Answer.Known(None)
    else {
      val all  = hits.map(_._3)
      val item = all.flatMap(_.item).headOption
      val (id, linked, hit) = hits.find(_._3.item == item).getOrElse(hits.head)
      Answer.Known(Some(Named(id, linked, hit, all)))
    }
  }

  /** What the cluster's catalogue ids name, to take: `Known(None)` for no take, `Unknown` while a question is open — each
   *  one noted, a catalogue question in `catalogueAsked`, a family's record in `familyAsked`, TMDB's find of an IMDb id
   *  in `finding`. */
  def take(listings: Seq[Listing], catalogue: CatalogueAnswers, families: Map[VoterFamily, FamilyAnswers], tmdbOf: String => Answer[Option[Int]],
           catalogueAsked: mutable.Set[CatalogueQuestion], familyAsked: mutable.Set[(VoterFamily, String)], finding: mutable.Set[String]): Answer[Option[Taken]] =
    named(listings, catalogue, catalogueAsked) match {
      case Answer.Unknown => Answer.Unknown
      case Answer.Known(None) => Answer.Known(None)
      case Answer.Known(Some(_)) if listings.exists(Agreement.billsSeveral) => Answer.Known(None)
      case Answer.Known(Some(film)) =>
        // the record whose facts the listings must not contradict: Wikidata's item, else IMDb's title
        val record: Option[Answer[Option[SourceRecord]]] = film.item.flatMap(q => families.get(VoterFamily.Wiki).map(f => noted(f, q, familyAsked)))
          .orElse(film.imdb.flatMap(tt => families.get(VoterFamily.Imdb).map(f => noted(f, tt, familyAsked))))
        record.fold[Answer[Option[Taken]]](Answer.Known(None)) {
          case Answer.Unknown => Answer.Unknown
          case Answer.Known(None) => Answer.Known(None)   // no film item, no IMDb title: nothing a card can stand on
          case Answer.Known(Some(facts)) if Agreement.contradictedByTheListing(listings, facts) => Answer.Known(None)
          case Answer.Known(Some(facts)) =>
            val imdb = film.imdb.orElse(facts.crossIds.get("imdb"))
            val tmdb: Answer[Option[Int]] = film.tmdb.orElse(facts.crossIds.get("tmdb").flatMap(_.toIntOption)) match {
              case Some(id) => Answer.Known(Some(id))
              case None     => imdb.fold[Answer[Option[Int]]](Answer.Known(None))(tt => { val found = tmdbOf(tt); if (found == Answer.Unknown) finding += tt; found })
            }
            tmdb match {
              case Answer.Unknown => Answer.Unknown
              case Answer.Known(tmdbFilm) =>
                val stands = tmdbFilm.fold(imdb.map(VoterFamily.Imdb.database -> _).orElse(film.item.map(VoterFamily.Wiki.database -> _)))(_ => None)
                val as     = tmdbFilm.fold(stands.fold("no film")((source, at) => s"$source $at"))(tmdbId => s"TMDB $tmdbId")
                val line   = s"catalogue id ${film.id.source}:${film.id.id}${if (film.linked) " (linked from the venue's page)" else ""} → $as via ${film.hit.via}" +
                  film.hit.item.fold("")(q => s" ($q)") + s" '${facts.film.title}'" + facts.film.year.fold("")(year => s" ($year)")
                Answer.Known(Option.when(tmdbFilm.isDefined || stands.isDefined)(Taken(tmdbFilm, stands, Some(facts.film.title), facts.film.year, line)))
            }
        }
    }

  /** Do the hits name one film: at most one Wikidata item, one TMDB id and one IMDb id among them? */
  private def oneFilm(hits: Seq[CatalogueHit]): Boolean =
    hits.flatMap(_.item).distinct.sizeIs <= 1 && hits.flatMap(_.tmdb).distinct.sizeIs <= 1 && hits.flatMap(_.imdb).distinct.sizeIs <= 1

  /** `family`'s record of `id`, a question noted when it is not answered yet. */
  private def noted(family: FamilyAnswers, id: String, asked: mutable.Set[(VoterFamily, String)]): Answer[Option[SourceRecord]] = {
    val answer = family.record(id)
    if (answer == Answer.Unknown || !family.fresh(s"record|$id")) asked += family.family -> s"record|$id"
    answer
  }
}
