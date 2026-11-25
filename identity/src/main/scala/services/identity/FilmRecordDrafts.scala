package services.identity

import models.{Cinema, Filmweb, FilmwebPages, Imdb, MovieRecord, Source, SourceData}
import services.identity.measures.ResolverDecision
import services.titles.TitleNormalizer

import IdentityProjectionPlan.ResolverAttempt
import FilmPlans.Planned

/** Each planned film's record, for one draft ([[ScopeDrafting]]): its venues' slots as `venueSlots` built them,
 *  over what other writers keep on the stored film while it is the same film, and its venues' shapes kept for the next
 *  draft. `within`: the scope's clusters; `previousIdOf`: the stored film a counter numbered, if any. */
private[identity] final class FilmRecordDrafts(index: ProjectionIndex, within: ProjectionIndex.Within, shapes: FilmShapes,
                                               normalizer: TitleNormalizer, venueSlots: LeanVenueSlots,
                                               previousIdOf: Long => Option[String]) {
  import index.storedById

  private val filmOf        = within.clusters.iterator.map(c => c.members -> c.film).toMap
  private val provisionalOf = within.clusters.iterator.map(c => c.members -> c.provisional).toMap
  private val unmatchedOf   = within.clusters.filter(_.film.isEmpty).map(c => c.members -> c).toMap

  /** One planned film's draft: its venues' shapes kept for the next draft, and its record. */
  def filmDraftOf(plan: Planned): FilmDraft = {
    import plan.{counter, members, keys}
    val previous = previousIdOf(counter).flatMap(storedById.get)
    val film     = filmOf(members)
    val anchor   = plan.titles.toSeq.sortBy { case (t, n) => (-n, t) }.headOption.map(_._1).getOrElse("")
    val venueShapes = keptShapes(plan)
    val venueData = venueShapes.valuesIterator.flatMap(_.lean).toMap
    // What other writers keep on the stored film — its ratings, TMDB's, IMDb's and Filmweb's slots and links — is theirs,
    // kept while it is the same film. Everything the projection writes is worked out from the inputs as they are now,
    // exactly as a projection from scratch over them would: nothing stands because an earlier projection wrote it.
    val base = previous.filter(_.record.tmdbId == film).map(_.record).getOrElse(MovieRecord())
    val held     = unmatchedOf.get(members).fold(base)(FilmRecordDrafts.heldLinks(_, base, anchor))
    val nonVenue = held.data.filter { case (source, _) => Source.cinemaOf(source).isEmpty }
    val networks = previous.fold(Map.empty[Source, SourceData])(_.record.data.filter { case (source, _) => Cinema.Networks.contains(source) })
    val previousNonVenue = previous.fold(Set.empty[Source])(_.record.data.keysIterator.filter(Source.cinemaOf(_).isEmpty).toSet)
    val record = held.copy(
      tmdbId        = film,
      tmdbAttempt   = Option.when(film.isEmpty)(ResolverAttempt),
      // its take Pending -- the decision on the inputs as they stand, an answer it reads not filed yet: served marked
      // provisional, never held to an earlier take (step 6, the owner 2026-10-31)
      tmdbProvisional = Option.when(provisionalOf.getOrElse(members, false))(true),
      // The one exception, still carried: the title its first draft read, while it is the same film. It decides no
      // take, only what the IMDb id resolver searches for a film TMDB links no IMDb id for — and that id, kept with the
      // film, would move with it ("Samson i Dalila" (2026) is found as its first listing's "Opera-samson i dalila", not
      // as its later listings' title). That resolver now falls back to every title the film's venues publish (after
      // its other rungs), so a film whose take came late is still found (Main #2617); this title only decides what it
      // asks FIRST. Dropping it for the current inputs alone is what is left of identity-no-drift.md, item 7.
      searchTitle   = base.searchTitle.orElse(Some(normalizer.apiQuery(normalizer.recase(anchor)))),
      // A synopsis kept after its venue stopped listing the film is no input any more: no writer files one since the
      // landing's slot drop went, and a projection from scratch would not hold it.
      retainedSynopses = Map.empty,
      // A chain's network detail slot is venue source data no listing is published at: kept from the
      // stored film whatever it is matched to, as its venue slots are rebuilt from theirs.
      // The venue slots first, and the rest over them: no venue slot is a non-venue source or a chain's network one.
      // Over the stored record's own map where a slot is the very one it holds (the cache keeps the projection's lean
      // slots): a wide film written for a few venues built its whole map anew, kept until its next write — promoted
      // and left to die old, every projection.
      data          = previous.fold(venueData ++ nonVenue ++ networks)(p =>
        FilmShapes.sharing(p.record.data, venueData ++ nonVenue ++ networks, _ eq _)))
    // Of a film drafted again, what may differ from the last draft's write: its non-venue sources, and the venues it
    // rebuilt — their slots as written then and as built now. Every other venue's slot is the one written.
    val touched = plan.was.map { was =>
      val rebuilt = plan.groups.collect { case (cinema, Right(_)) => cinema }
      previousNonVenue ++ nonVenue.keySet ++ networks.keySet ++
        rebuilt.flatMap(cinema => was.venues(cinema).lean.map(_._1) ++ venueShapes(cinema).lean.map(_._1))
    }
    FilmDraft(counter, previous.map(_.id), keys, record, anchor, touched)
  }

  /** Each venue's slots of a planned film — of a film drafted again, the venues it rebuilt replace theirs — kept as the
   *  film's shape for its next draft. A venue drafted again alike is the one kept, and a film drafted again alike at
   *  every venue keeps its venues' map. */
  private def keptShapes(plan: Planned): Map[Cinema, VenueShape] = {
    import plan.{counter, members, keys}
    val kept        = shapes.get(counter)
    val venueShapes = plan.groups.iterator.map {
      case (cinema, Left(venue))            => cinema -> venue
      case (cinema, Right((group, previous, priors))) => cinema -> FilmShape.reused(kept.flatMap(_.venues.get(cinema)),
        VenueShape(group.rows.map(_.listing.key), group.key, venueSlots(group.key), VenueShape.inputs(previous, priors, group.key.pages)))
    }.toMap
    // Over the kept map: only a venue that is not the shape held is put in — a wide film moved at a few venues built its
    // map of thousands anew each projection, kept until the next one (dead in the old generation, heap dump 2026-10-05).
    // Its keys too, when they are the same listings in the same order.
    shapes.put(counter, FilmShape(members, kept.map(_.keys).filter(k => (k eq keys) || k == keys).getOrElse(keys), plan.titles,
      kept.fold(venueShapes)(k => FilmShapes.sharing(k.venues, venueShapes, _ eq _))))
    venueShapes
  }
}

private[identity] object FilmRecordDrafts {
  /** The ids an unmatched film is linked by, from its cluster's decision alone: a film TMDB has no record of carries the
   *  IMDb id of the fallback film the resolver took for it, else none. The TMDB film its listings' evidence leans to though
   *  no rule took it ([[ResolverDecision.Leaning]]) links nothing: no rule took it — "Łowca androidów: Final Cut" leans to
   *  Ridley Scott's 2026 film — and an id an earlier answer or a title search held for it is no input — PL "Lalka" (2026)
   *  held the 1968 film's, and its 6.9. The rating and IMDb's slot go with an id that goes. A no-match reached before
   *  every question was answered links what it names too: what an earlier decision linked is no input.
   *  One standing on Filmweb's or Wikidata's own id (the agreement's, where no record links an IMDb id) links that
   *  record instead: its Filmweb page — replacing a page a title search guessed, with its rating and slot — or its
   *  Wikidata item, which nothing but such a stand gives a film TMDB has no record of. Without a stand, its Filmweb page
   *  is Filmweb's own search's (`TmdbLessRatingLinks`), which keeps or drops it. */
  def heldLinks(cluster: Cluster, base: MovieRecord, anchor: String): MovieRecord = {
    val imdb = cluster.fallback.filter(_.source == "imdb").map(_.id)
    val linked = (if (imdb == base.imdbId) base else base.copy(imdbId = imdb, imdbRating = None, data = base.data - Imdb))
      .copy(wikidataId = cluster.fallback.collect { case ResolverDecision.Fallback("wikidata", id, _, _, _) => id })
    cluster.fallback match {
      case Some(ResolverDecision.Fallback("filmweb", id, _, title, year))
          if !linked.filmwebUrl.flatMap(FilmwebPages.idOf).map(_.toString).contains(id) =>
        linked.copy(filmwebUrl = id.toIntOption.map(FilmwebPages.url(_, "film", title.getOrElse(anchor), year)), filmwebRating = None,
          data = linked.data - Filmweb)
      case _ => linked
    }
  }
}
