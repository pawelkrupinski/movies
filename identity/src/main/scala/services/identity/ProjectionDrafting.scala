package services.identity

import services.movies.LatestTitleYear

import models.{Cinema, CinemaMovie, Filmweb, FilmwebPages, Imdb, MovieRecord, Source, SourceData}
import services.movies.{CinemaSlotBuilder, FilmId, ListingKey, ScreeningTokens, ShowtimesDigest, TitleNormalizer}

import IdentityProjectionPlan.{RowBatch, ResolverAttempt, VenueGroup}

/** One run of [[IdentityProjectionPlan.draftOf]]: the drafts of the films `scope` names over `index`, worked out in its
 *  steps — the films' ids by overlap ([[plannedFilms]]), the venue slots each film's venues need built
 *  ([[buildVenueSlots]]), each film's record ([[filmDraftOf]]) and the projection's draft as a whole ([[projectionDraft]]).
 *  The mutable state a run reads and writes — the memo of venue slots, the films' shapes — is written in that order. */
private[identity] final class ProjectionDrafting(index: ProjectionIndex, scope: ProjectionScope, normalizer: TitleNormalizer,
                                                 slots: CinemaSlotBuilder, tokens: ScreeningTokens,
                                                 rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]], memo: VenueSlotMemo,
                                                 shapes: FilmShapes, changed: Set[ListingKey])(using LatestTitleYear) {
  import index.{byKey, storedById, covered}
  import ProjectionDrafting.{Planned, PlannedGroup}

  // Within the scope: its stored films' listings, and its clusters. Everything outside it is a film no change reached,
  // whose draft is the film as stored.
  private val within        = index.within(scope)
  import within.{previousOf, stored}
  private val previousFilms = within.listingsOf.map { case (id, ls) => IdSeeding.Film(id, ls) }
  private val clusters      = within.clusters.map(c => c.members -> c.film)
  private val unmatchedOf   = within.clusters.filter(_.film.isEmpty).map(c => c.members -> c).toMap
  private val numbered      = previousFilms.map(f => covered.counterOf(f.id).get -> f.listings)

  // 2. Ids by overlap.
  private val filmOf   = clusters.toMap
  private val assigned = IdAssigner.assign(numbered, clusters.map(_._1), covered.nextCounter)
  private def previousIdOf(c: Long): Option[String] = Option.when(c < covered.nextCounter)(covered.filmIdOf(c)).flatten

  // 3. Each film's record. Its venue slots are LEAN, each venue's from the memo when nothing it is built from
  // moved, else built from the venue's rows, read a batch of venues at a time and let go.
  // Each stored film's venue slots by venue, worked out once per film, not once per venue of every film it lends a
  // listing to — a US film showing at hundreds of venues was scanned whole for each of them.
  private val slotsByFilm = scala.collection.mutable.HashMap.empty[String, Map[Cinema, Seq[(Source, SourceData)]]]
  private def slotsByVenueOf(id: String): Map[Cinema, Seq[(Source, SourceData)]] = slotsByFilm.getOrElseUpdate(id,
    storedById.get(id).fold(Map.empty[Cinema, Seq[(Source, SourceData)]])(stored => VenueSlots.byVenue(stored.record)))
  private def priorsOf(id: String): Map[Cinema, Int] = storedById.get(id).fold(Map.empty[Cinema, Int])(r => shapes.priorsOf(id, r.record))
  // One venue's listings on a film, and what its slots are built from.
  private def previousAt(keys: Seq[ListingKey]): Seq[Option[String]] = keys.map(k => index.previousOf.get(k).map(_.id))
  private def priorsAt(cinema: Cinema, previous: Seq[Option[String]]): Seq[Int] =
    previous.flatten.distinct.sorted.map(id => priorsOf(id).getOrElse(cinema, 0))
  // What the venue's pages for these listings said when last read: a slot carries its page-written detail from them.
  private def pagesAt(cinema: Cinema, keys: Seq[ListingKey]): Int = slots.pages.digest(cinema, keys)
  private def groupOf(cinema: Cinema, ofVenue: Seq[ProjectedListing], counter: Long): PlannedGroup = {
    // The prior slots any of these rows' previous films held at the venue: a superset of the one each built
    // slot carries forward, so a change to any of them is a change to the key — and which film each row is on,
    // since a slot carries forward its own listing's film's slot: two rows trading films trade the slot each
    // carries, the set of priors unchanged. Left out, a slot built over one film's prior was taken from the memo
    // for the other's (a venue's "Lalka" and "Lalka 2D" trading films kept the director only one of them had).
    val previous = previousAt(ofVenue.map(_.listing.key))
    val priors   = priorsAt(cinema, previous)
    // Each row's film by its counter, which a film keeps for good and has from its first draft: what the key of a
    // slot once written is worked out from ([[VenueSlotMemo.written]]), a film new to the store included.
    (VenueGroup(cinema, ofVenue, VenueSlotMemo.keyOf(cinema.displayName, ofVenue, priors, previous.map(_.flatMap(covered.counterOf)),
      pagesAt(cinema, ofVenue.map(_.listing.key))),
      counter), previous, priors)
  }

  // A film drafted again with the listings it was last drafted with is drafted again only where it moved: a venue one of
  // whose listings, or whose listings' previous films' stored slots, moved since ([[FilmShapes]]).
  private val reuse   = !scope.whole
  private lazy val placedAt: Map[ListingKey, Long] = assigned.idOfListing
  private val dirtyAt = if (!reuse) Map.empty[Long, Set[ListingKey]]
                        else changed.iterator.filter(byKey.contains).flatMap(k => placedAt.get(k).map(_ -> k)).toSeq.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  /** Each film's plan: its venues' groups, kept from its last draft where nothing they are built from moved. */
  private def plannedFilms: Seq[Planned] = assigned.ids.map { case (counter, members) =>
    // The same listings: the same keys in the same order, the same titles (a key holds its listing's title). An equal set
    // of other key objects is the same listings read again — a venue's scrape reads every listing it prints, moved or not.
    shapes.get(counter).filter(sh => reuse && ((sh.members eq members) || sh.members == members))
      .map(_.over(members, k => byKey.get(k).fold(k)(_.listing.key))) match {
      case Some(was) => plannedAgain(counter, members, was)
      case _         => plannedAnew(counter, members)
    }
  }

  /** A film drafted again with the listings it was last drafted with: its venues that moved are worked out again. */
  private def plannedAgain(counter: Long, members: Set[ListingKey], was: FilmShape): Planned = {
    val dirtyVenues = dirtyAt.getOrElse(counter, Set.empty).map(byKey(_).listing.cinema)
    val groups = was.venues.toSeq.sortBy(_._1.displayName).map { case (cinema, venue) =>
      // Moved: one of its listings did, or which film one is on, or the slots those films hold at the venue, or
      // what one's page said.
      val moved = dirtyVenues(cinema) || { val previous = previousAt(venue.keys)
        VenueShape.inputs(previous, priorsAt(cinema, previous), pagesAt(cinema, venue.keys)) != venue.inputs }
      if (!moved) cinema -> Left(venue)
      else cinema -> Right(groupOf(cinema, venue.keys.map(byKey), counter))
    }
    Planned(counter, members, was.keys, was.titles, groups, Some(was))
  }

  /** A film drafted with other listings than last time, or for the first time: every venue worked out. */
  private def plannedAnew(counter: Long, members: Set[ListingKey]): Planned = {
    val keys   = members.toSeq.sorted
    val rows   = keys.map(byKey)
    val titles = rows.groupMapReduce(_.listing.cleanTitle)(_ => 1)(_ + _)
    val groups = rows.groupBy(_.listing.cinema).toSeq.sortBy(_._1.displayName).map { case (cinema, ofVenue) =>
      cinema -> Right(groupOf(cinema, ofVenue, counter))
    }
    Planned(counter, members, keys, titles, groups, None)
  }

  private val build: (Cinema, Seq[ListingKey], VenueSlots.VenueRows) => Seq[(Source, SourceData)] =
    // Over the whole index: a slot's prior is its representative listing's film's, whichever of the venue's listings that is.
    (cinema, keys, rows) => VenueSlots.buildVenue(cinema, keys, rows, index.previousOf, storedById, normalizer, slots, tokens)
  private val venueSlots = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, Seq[(Source, SourceData)]]
  // The slots the stored film of a group's listings holds at its venue: what a restarted worker's memo reuses
  // when they are recorded as built from the group's key.
  private def storedAt(group: VenueGroup): Option[Seq[(Source, SourceData)]] =
    group.rows.flatMap(r => previousOf.get(r.listing.key)).map(_.id).distinct match {
      case Seq(id) => storedById.get(id).map(_ => slotsByVenueOf(id).getOrElse(group.cinema, Nil))
      case _ => None
    }

  /** Every planned venue's lean slots into `venueSlots`: a kept venue's as kept, a worked-out one's from the memo, else
   *  built from its venue's rows, read a batch of venues at a time. */
  private def buildVenueSlots(planned: Seq[Planned]): Unit = {
    planned.foreach(_.groups.foreach { case (_, Left(venue)) => venueSlots(venue.memoKey) = venue.lean; case _ => () })
    val pending    = planned.flatMap(_.groups.collect { case (_, Right((group, _, _))) => group }).filter(group =>
      memo.lookup(group.key, storedAt(group)).fold(true) { lean => venueSlots(group.key) = lean; false })
    pending.groupBy(_.cinema).toSeq.sortBy(_._1.displayName).grouped(RowBatch).foreach(buildBatch)
  }

  private def buildBatch(batch: Seq[(Cinema, Seq[VenueGroup])]): Unit = {
    val fetched = rowsOf(batch.map(_._1).toSet)
    batch.foreach { case (cinema, groups) =>
      val rows = VenueSlots.rowsByKey(cinema, fetched.getOrElse(cinema, Nil), normalizer)
      groups.foreach { group =>
        val lean = build(cinema, group.rows.map(_.listing.key), rows).map { case (source, slot) => source -> ShowtimesDigest.stripSlot(slot) }
        venueSlots(group.key) = lean
        memo.store(group.key, lean, keep = VenueSlots.readAsListed(cinema, group.rows, rows, normalizer), writtenAs = Some(group.counter))
      }
    }
  }

  /** One planned film's draft: its venues' shapes kept for the next draft, and its record. */
  private def filmDraftOf(plan: Planned): FilmDraft = {
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
    val held     = heldLinks(members, base, anchor)
    val nonVenue = held.data.filter { case (source, _) => Source.cinemaOf(source).isEmpty }
    val networks = previous.fold(Map.empty[Source, SourceData])(_.record.data.filter { case (source, _) => Cinema.Networks.contains(source) })
    val previousNonVenue = previous.fold(Set.empty[Source])(_.record.data.keysIterator.filter(Source.cinemaOf(_).isEmpty).toSet)
    val record = held.copy(
      tmdbId        = film,
      tmdbAttempt   = Option.when(film.isEmpty)(ResolverAttempt),
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
  private def heldLinks(members: Set[ListingKey], base: MovieRecord, anchor: String): MovieRecord =
    unmatchedOf.get(members).fold(base) { cluster =>
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

  /** The projection's draft of `planned` films drafted as `drafts`: the films retired and the regroupings. */
  private def projectionDraft(planned: Seq[Planned], drafts: Seq[FilmDraft]): ProjectionDraft = {
    val venues = planned.map(plan => plan.counter -> plan.groups.map {
      case (cinema, Left(venue))           => cinema -> venue.keys
      case (cinema, Right((group, _, _)))  => cinema -> group.rows.map(_.listing.key)
    }.toMap).toMap

    // Retired: a previous film no cluster kept, and a stored film none of whose listings is published.
    val kept    = drafts.flatMap(_.inherited).toSet
    val retired = stored.map(_.id).filterNot(kept).distinct.sortBy(_.value)

    ProjectionDraft(drafts, retired, retired.filterNot(id => index.placed(id.value)), covered, index.additions,
      regroupings(drafts, retired), venues, build, rowsOf, VenueSlots.rowsByKey(_, _, normalizer))
  }

  private def regroupings(drafts: Seq[FilmDraft], retired: Seq[FilmId]): Regroupings = {
    val newIdOf = assigned.idOfListing
    Regroupings(
      merges  = clusters.map { case (ms, _) => ms.flatMap(previousOf.get).map(_.id).size - 1 }.filter(_ > 0).sum,
      splits  = previousFilms.count(f => f.listings.flatMap(newIdOf.get).sizeIs > 1),
      moves   = previousOf.count { case (l, ref) => newIdOf.get(l).map(c => previousIdOf(c).getOrElse(s"#$c")) != Some(ref.id) },
      fresh   = drafts.count(_.inherited.isEmpty),
      retired = retired.size)
  }

  /** The draft: plan every film, build the venue slots the plans need, then draft each film's record. */
  def draft: ProjectionDraft = {
    val planned = plannedFilms
    buildVenueSlots(planned)
    projectionDraft(planned, planned.map(filmDraftOf))
  }
}

private[identity] object ProjectionDrafting {
  /** A venue group worked out: the group, and the films its listings were on and the slots those held at the venue. */
  type PlannedGroup = (VenueGroup, Seq[Option[String]], Seq[Int])

  /** A film [[IdentityProjectionPlan.draftOf]] plans: its venues' groups, each one kept from the last draft (`Left`) or
   *  worked out (`Right`, with the stored records of the films its priors are from). */
  final case class Planned(counter: Long, members: Set[ListingKey], keys: Seq[ListingKey], titles: Map[String, Int],
                           groups: Seq[(Cinema, Either[VenueShape, PlannedGroup])], was: Option[FilmShape])
}
