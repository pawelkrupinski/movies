package services.identity.agreement

import services.identity.{Answer, CatalogueAnswers, CatalogueHit, CatalogueId, CatalogueQuestion, CatalogueSources, Listing, ListingShape}

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
 * item, or IMDb's title — exists and its year and director do not contradict a listing's own, or its venue page's
 * ([[Agreement.contradictedByTheListing]]: a venue linking the wrong film; US "A Night at the Opera", the Marx Brothers'
 * by its Flicks page, is left as the venue credits Edmund Goulding), and no listing bills several works. A feed's id
 * (Webedia's, [[services.identity.CatalogueSources.FeedIds]]) is the feed's link of a screening to its entry, and its
 * facts are that entry's own: they rule nothing out and confirm nothing, so the film needs corroboration they do not
 * enter ([[corroborated]]: a venue's own facts, or TMDB's search of the title picking it). A stage work's
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
   *  in `finding`. `stated`: the listings with what their venue's own pages state — the year and director a record must
   *  not contradict (PL Kinoteka's "Czarne zombie" page credits Bedward's 2026 film, and links Corman's 1963 one).
   *  `titlePicks`: the films the listings' own titles pick, each search's most popular hit ([[AgreementStage]]), read
   *  only for a feed's id ([[fedOnly]]). */
  def take(listings: Seq[Listing], stated: Seq[Listing], catalogue: CatalogueAnswers, families: Map[VoterFamily, FamilyAnswers],
           tmdbOf: String => Answer[Option[Int]], titlePicks: () => Set[Int],
           catalogueAsked: mutable.Set[CatalogueQuestion], familyAsked: mutable.Set[(VoterFamily, String)], finding: mutable.Set[String]): Answer[Option[Taken]] =
    // a feed's ids alone, and nothing that could corroborate the film they name: no question worth asking
    if (uncorroborable(listings, stated, titlePicks)) Answer.Known(None)
    else named(listings, catalogue, catalogueAsked) match {
      case Answer.Unknown => Answer.Unknown
      case Answer.Known(None) => Answer.Known(None)
      case Answer.Known(Some(_)) if listings.exists(ListingShape.billsSeveral) => Answer.Known(None)
      case Answer.Known(Some(film)) =>
        // the record whose facts the listings must not contradict: Wikidata's item, else IMDb's title
        val record: Option[Answer[Option[SourceRecord]]] = film.item.flatMap(q => families.get(VoterFamily.Wiki).map(f => noted(f, q, familyAsked)))
          .orElse(film.imdb.flatMap(tt => families.get(VoterFamily.Imdb).map(f => noted(f, tt, familyAsked))))
        record.fold[Answer[Option[Taken]]](Answer.Known(None)) {
          case Answer.Unknown => Answer.Unknown
          case Answer.Known(None) => Answer.Known(None)   // no film item, no IMDb title: nothing a card can stand on
          case Answer.Known(Some(facts)) if Agreement.contradictedByTheListing(stated, facts) => Answer.Known(None)
          case Answer.Known(Some(facts)) =>
            val imdb = film.imdb.orElse(facts.crossIds.get("imdb"))
            // TMDB's own find of the IMDb id first: Wikidata's TMDB id can name a record TMDB since deleted or merged (DE
            // "Dann passiert das Leben": P4947 1517080, gone; TMDB finds its IMDb id as 1445025)
            val tmdb: Answer[Option[Int]] = imdb match {
              case Some(tt) => val found = tmdbOf(tt); if (found == Answer.Unknown) finding += tt; found
              case None     => Answer.Known(film.tmdb.orElse(facts.crossIds.get("tmdb").flatMap(_.toIntOption)))
            }
            tmdb match {
              case Answer.Unknown => Answer.Unknown
              case Answer.Known(tmdbFilm) if fedOnly(film, listings) && !corroborated(stated, facts, tmdbFilm, titlePicks) => Answer.Known(None)
              case Answer.Known(tmdbFilm) =>
                val stands = tmdbFilm.fold(imdb.map(VoterFamily.Imdb.database -> _).orElse(film.item.map(VoterFamily.Wiki.database -> _)))(_ => None)
                val as     = tmdbFilm.fold(stands.fold("no film")((source, at) => s"$source $at"))(tmdbId => s"TMDB $tmdbId")
                val line   = s"catalogue id ${film.id.source}:${film.id.id}${if (film.linked) " (linked from the venue's page)" else ""} → $as via ${film.hit.via}" +
                  film.hit.item.fold("")(q => s" ($q)") + s" '${facts.film.title}'" + facts.film.year.fold("")(year => s" ($year)")
                Answer.Known(Option.when(tmdbFilm.isDefined || stands.isDefined)(Taken(tmdbFilm, stands, Some(facts.film.title), facts.film.year, line)))
            }
        }
    }

  /** Is the film named only by a feed's catalogue id ([[services.identity.CatalogueSources.FeedIds]]) — none the venue's
   *  page links, no other id of the cluster's? The feed's facts are that entry's own, so they cannot confirm it:
   *  agreeing with the record of the entry they were copied from, they say nothing of whether the feed linked the
   *  venue's film. */
  private def fedOnly(film: Named, listings: Seq[Listing]): Boolean =
    !film.linked && listings.flatMap(_.catalogueIds).filter(CatalogueSources.mappable).forall(id => CatalogueSources.FeedIds(id.source))

  /** Could nothing corroborate what the listings' ids name ([[corroborated]]): every id they carry a feed's, no venue page
   *  to link another, no listing whose venue states a year, and no title search picking a film? */
  private def uncorroborable(listings: Seq[Listing], stated: Seq[Listing], titlePicks: () => Set[Int]): Boolean = {
    val ids = listings.flatMap(_.catalogueIds).filter(CatalogueSources.mappable)
    ids.nonEmpty && ids.forall(id => CatalogueSources.FeedIds(id.source)) && listings.forall(_.page.isEmpty) &&
      stated.forall(listing => listing.factsFromCatalogue || listing.year.isEmpty) && titlePicks().isEmpty
  }

  /** Is a feed's film corroborated by evidence the feed did not copy from its own entry: a venue's own year and director
   *  crediting it ([[Agreement.listingVotes]] over the listings whose facts their venue states), or — its TMDB film —
   *  TMDB's search of the title picking it, the most popular of its namesakes ([[take]]'s `titlePicks`)? Measured on
   *  the unmatched clusters (2026-10-06): DE "Die Nibelungen - Teil 1: Siegfried", Lang's 1924 film, is the title's
   *  pick over Reinl's 1966 one and kept; "To The Bone", whose feed links Erin Li's 2014 short, is not — the title
   *  picks Noxon's 2017 feature. The model's own lean is no corroboration: it reads the feed's facts (it leans to the
   *  short). */
  private def corroborated(stated: Seq[Listing], facts: SourceRecord, tmdbFilm: Option[Int], titlePicks: () => Set[Int]): Boolean =
    Agreement.listingVotes(stated.filterNot(_.factsFromCatalogue), Seq(facts)).nonEmpty || tmdbFilm.exists(titlePicks().contains)

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
