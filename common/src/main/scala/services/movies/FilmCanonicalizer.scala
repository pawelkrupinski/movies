package services.movies

import models.{MovieRecord, Source, Tmdb}
import services.resolution.YearWindow

/**
 * Pure collapse of a cluster of same-film rows into the single canonical
 * `(CacheKey, MovieRecord)` it should be stored under.
 *
 * Extracted from `MovieCache.collapseCluster` so the SAME decision — which
 * year, which spelling, which merged record — can be reused outside the cache
 * (a future projector / reconciler) without dragging in the cache's mutation,
 * locks, or Caffeine state. This object is side-effect-free: it never reads or
 * writes the cache; it only computes. `MovieCache` keeps the mutation half
 * (the `needsFix` guard + `withTitleLock`/`invalidate`/`put`) and delegates the
 * decision here, so there is ONE definition of the canonicalisation rules.
 */
object FilmCanonicalizer {

  /** Total order picking the canonical (surviving) key among same-tmdbId,
   *  same-normalised-title rows: prefer a row that carries a release year over
   *  a yearless one, then the lower year, then the cleanTitle. A pure function
   *  of the key, so the canonical never depends on write order. */
  private[services] def canonicalRank(k: CacheKey): (Boolean, Int, String) =
    (k.year.isEmpty, k.year.getOrElse(Int.MaxValue), k.cleanTitle)

  /** The id a merged cluster keeps — THE one rule, asked by every fold (the settle's
   *  `collapseCluster`, the write-time tmdbId/imdbId fold, a re-key onto a held key, the
   *  staging fold): the row already stored under the canonical key, else the best-ranked
   *  existing member; `None` only for a cluster with no stored member at all (a brand-new
   *  film, whose caller mints an id). A canonical key no member holds is a RETITLE of the
   *  survivor, never a new document — see `FilmId`. */
  def survivor(members: Seq[(CacheKey, FilmId)], canonical: CacheKey): Option[FilmId] =
    members.collectFirst { case (k, id) if k == canonical => id }
      .orElse(members.sortBy { case (k, _) => canonicalRank(k) }.headOption.map(_._2))

  /** The search-title grouping key: the title with the decorations the search rules
   *  strip removed, romanised, sanitized. The settle's search-title edge unions rows
   *  by it; the scrape-time gate asks the corpus index the same question
   *  (`CorpusIndex.keysWithSearchKey`), so a listing that equals a stored film under
   *  this key lands on it instead of becoming a newcomer the edge folds a tick later.
   *  Romanisation is confined to THIS key — never the stored key, the display title,
   *  or the real TMDB query. */
  def searchKey(title: String, normalizer: TitleNormalizer): String =
    normalizer.sanitize(tools.TextNormalization.romanizeCyrillic(normalizer.apiQuery(title)))

  /** Year a cluster collapses to — TMDB's resolved year if any member carries
   *  one (all resolved members of a cluster share a tmdbId hence a tmdbYear),
   *  else the lowest present KEY year, else yearless.
   *
   *  Deliberately NOT a fallback to slot `releaseYear`: a deferred-detail cinema
   *  scrapes a film YEARLESS (yearless key) and its detail later adds a production
   *  year to the SLOT only. If that provisional slot year promoted the row's key
   *  here, a row that folds ALONE (before its resolved siblings — the interleaved
   *  arrival the reaper folds one cinema at a time) would become a year-bearing
   *  movies row that the siblings can no longer absorb (Δ>window) — the order-
   *  dependent "Głos Hind Rajab" / Kino Amondo (slot 2022 vs resolved 2025) split.
   *  Keeping a yearless-key cluster yearless leaves it a rule-(4) row the settle
   *  folds into the resolved film, regardless of fold order. A row whose KEY
   *  carries a year (a non-deferred year-bearing scrape) still windows normally. */
  private[services] def clusterYear(cluster: Seq[(CacheKey, MovieRecord)]): Option[Int] =
    cluster.flatMap { case (_, e) => e.tmdbYear }.minOption
      .orElse(cluster.flatMap { case (k, _) => k.year }.minOption)

  private type Row = (CacheKey, MovieRecord)
  private def rank(r: Row): (Boolean, Int, String) = canonicalRank(r._1)

  /** One per-film cluster within a `sanitize(title)` group — its member rows and
   *  the reference year an unresolved row's year is measured against (rule 2,
   *  `YearWindow.ProductionToRelease`). `minRank` is the cluster's
   *  smallest `canonicalRank`, the deterministic tie-break for "which cluster is
   *  canonical / nearest". */
  private case class Cluster(refYear: Option[Int], rows: Seq[Row]) {
    def minRank: (Boolean, Int, String) = rows.map(rank).min
  }

  /** A resolved row beside the identity its cinemas published, read ONCE: rule 1 asks
   *  `MixedFilmDetector` about a row against every sibling, and rebuilding the
   *  identity per question re-read one row's slots once per comparison. */
  private case class Identified(row: Row, identity: Option[MixedFilmDetector.Group])

  /** Partition one `sanitize(title)` group into per-film clusters. A pure
   *  function of the row SET — every intermediate collection is sorted by
   *  `canonicalRank` (or a total order on its key) before iterating, so the
   *  result is independent of cache/scrape/iteration order. Every rule refuses
   *  when it cannot tell which film a row belongs to. Precedence:
   *
   *    1. [[resolvedClusters]] — resolved rows cluster by tmdbId, split where their
   *       cinemas published different films, folded where TMDB holds one film
   *       under two ids (shared imdbId).
   *    2. [[attachWithinYearWindow]] — unresolved year-bearing rows join the
   *       nearest resolved cluster within `YearWindow.ProductionToRelease`.
   *    2b. [[reclaimOrphans]] — a rule-2 orphan that is provably the same listing
   *       (identical venue synopsis, or a decoration of the film's title) is
   *       reclaimed whatever its year.
   *    3. [[yearWindowClusters]] — remaining orphans cluster by
   *       `YearWindow.PublishedAdjacency`-wide windows from the lowest year.
   *    4. [[foldYearless]] — yearless unresolved rows fold into the ONE plausible
   *       film, unless their own published evidence contradicts it. */
  def clusterByFilm(group: Seq[(CacheKey, MovieRecord)], normalizer: TitleNormalizer): Seq[Seq[(CacheKey, MovieRecord)]] = {
    val resolved                   = resolvedClusters(group.filter(_._2.tmdbId.isDefined), normalizer)
    val (yeared, yearless)         = group.filter(_._2.tmdbId.isEmpty).partition(_._1.year.isDefined)
    val (withAdjacent, orphans)    = attachWithinYearWindow(resolved, yeared)
    val (reclaimed, stillOrphaned) = reclaimOrphans(withAdjacent, orphans, normalizer)
    val seeded                     = reclaimed ++ yearWindowClusters(stillOrphaned)
    foldYearless(seeded, resolvedCount = resolved.size, yearless, normalizer).map(_.rows).filter(_.nonEmpty)
  }

  /** Rule 1: one cluster per tmdbId, sorted by id, with two corrections.
   *
   *  A shared tmdbId is not on its own permission to merge: a row can hold an id that
   *  is not its film's, and merging a sibling onto it buries the disagreement for good
   *  (prod "Mistyczka" absorbed a different 2026 film). So keep the best-ranked row and
   *  split off every sibling [[differentFilms]] says contradicts it — and then treat the
   *  split-off rows by the SAME rule, best-ranked first: two venues listing one other
   *  film under the shared id are that one film, and splitting them one row each left
   *  their reunion to whenever imdbId enrichment reached both (the fold below).
   *
   *  What this never does is merge ACROSS a split: a row split off stays apart from the
   *  row it contradicts for as long as its cinemas publish the contradiction. It is
   *  recomputed from the row set every settle, so once the cause is gone (the id is
   *  corrected, the venue's evidence changes) the row rejoins with no memory of the
   *  split. Deliberately-separate editions ("Klub Konesera", dubs, "Zaproszenie |
   *  Kinoteka dla rodziców") are not this rule's business: they are kept apart, or
   *  not, by `groupByFilm`'s edges and the read-model's per-title cards.
   *
   *  Conversely TMDB sometimes holds ONE film under two ids (a re-release catalogued
   *  separately), both carrying the same imdbId — left apart the site shows the film
   *  twice (prod `ghost2bigtorig` 2025/2026, 2026-08-14). Clusters sharing an imdbId
   *  fold, under the same published-evidence veto. */
  private def resolvedClusters(rows: Seq[Row], normalizer: TitleNormalizer): Seq[Cluster] = {
    val identified = rows.map(row => Identified(row, MixedFilmDetector.publishedIdentity(row._2, normalizer)))
    val byTmdbId: Seq[Seq[Identified]] = identified.groupBy(_.row._2.tmdbId.get).toSeq.sortBy(_._1)
      .flatMap { case (_, members) => splitByPublishedFilm(members.sortBy(r => rank(r.row))) }
    foldSharedImdbIds(byTmdbId).map(rows => Cluster(refYear = rows.flatMap(_._2.tmdbYear).minOption, rows = rows))
  }

  /** One tmdbId's rows, in rank order, as the films their cinemas published: the
   *  best-ranked row with every sibling not contradicting it, then the rest the same way. */
  @scala.annotation.tailrec
  private def splitByPublishedFilm(ordered: Seq[Identified], films: Vector[Seq[Identified]] = Vector.empty): Seq[Seq[Identified]] =
    ordered match {
      case main +: rest =>
        val (different, same) = rest.partition(differentFilms(main, _))
        splitByPublishedFilm(different, films :+ (main +: same))
      case _ => films
    }

  /** Union-find over rule 1's tmdbId groups: groups carrying a common imdbId fold
   *  unless any pair across them describes different films. A component's root is
   *  always its lowest index, so the partition does not depend on the order the pairs
   *  fold in; only groups that actually share an imdbId are ever compared. */
  private def foldSharedImdbIds(byTmdbId: Seq[Seq[Identified]]): Seq[Seq[Row]] = {
    val sameFilm = Array.tabulate(byTmdbId.length)(identity)
    def root(x: Int): Int = { var r = x; while (sameFilm(r) != r) r = sameFilm(r); r }
    def fold(a: Int, b: Int): Unit = {
      val (ra, rb) = (root(a), root(b))
      if (ra != rb) sameFilm(math.max(ra, rb)) = math.min(ra, rb)
    }
    val sharingAnImdbId: Seq[(Int, Int)] =
      byTmdbId.indices.flatMap(i => byTmdbId(i).flatMap(_.row._2.imdbId).distinct.map(_ -> i))
        .groupMap(_._1)(_._2).values
        .flatMap(_.combinations(2).map { case Seq(i, j) => (i, j) })
        .toSeq.distinct.sorted
    sharingAnImdbId.foreach { case (i, j) =>
      if (!byTmdbId(i).exists(a => byTmdbId(j).exists(differentFilms(a, _)))) fold(i, j)
    }
    byTmdbId.indices.groupBy(root).toSeq.sortBy(_._1).map { case (_, indices) =>
      indices.sorted.flatMap(byTmdbId).map(_.row)
    }
  }

  /** Do two resolved rows describe different films? `MixedFilmDetector`'s test (a
   *  differing published original title corroborated by runtime or year, vetoed by an
   *  agreeing director — a title difference alone is a translation one film in eight),
   *  OR the two are curated franchise siblings.
   *
   *  The franchise check deliberately bypasses the director veto: sequels share
   *  directors (UK prod, 2026-09-16: every Hunger Games film ended up on one wrong
   *  tmdbId, with Odeon publishing nothing else to tell them apart). It reads only the
   *  cinemas' bare titles and asks `SequelMarker.curatedSiblingTitles`, not the general
   *  ordinal logic, which misfires on raw cinema text ("Ghost 2 (1)"). */
  private def differentFilms(a: Identified, b: Identified): Boolean =
    MixedFilmDetector.describeDifferentFilms(a.identity, b.identity) ||
      SequelMarker.curatedSiblingTitles(a.row._2.evidence.titles, b.row._2.evidence.titles)

  /** Rule 2: each unresolved year-bearing row joins the NEAREST resolved cluster whose
   *  tmdbYear is within `YearWindow.ProductionToRelease` (±2 — a cinema's production
   *  year vs TMDB's release year, "Zawieście czerwone latarnie" 1989 vs 1991), ties on
   *  the cluster's `minRank` then index. Bounded rather than "nearest at any distance"
   *  so a same-titled remake awaiting its own tmdbId is not swallowed. Returns the
   *  clusters with their adjacent rows, and the orphans (rank order) no window took. */
  private def attachWithinYearWindow(resolved: Seq[Cluster], yeared: Seq[Row]): (Seq[Cluster], Seq[Row]) = {
    val homes: Seq[(Row, Option[Int])] = yeared.sortBy(rank).map { row =>
      val year = row._1.year.get
      row -> resolved.zipWithIndex
        .filter { case (c, _) => YearWindow.agrees(Some(year), c.refYear, YearWindow.ProductionToRelease).contains(true) }
        .minByOption { case (c, index) => (YearWindow.distance(year, c.refYear.get), c.minRank, index) }
        .map(_._2)
    }
    (withHomedRows(resolved, homes), homes.collect { case (row, None) => row })
  }

  /** Each cluster plus, appended in `homes` order, the rows whose home is its index. */
  private def withHomedRows(clusters: Seq[Cluster], homes: Seq[(Row, Option[Int])]): Seq[Cluster] =
    clusters.zipWithIndex.map { case (c, index) =>
      c.copy(rows = c.rows ++ homes.collect { case (row, Some(`index`)) => row })
    }

  /** Rule 2b: reclaim a rule-2 orphan into the ONE resolved cluster it provably
   *  belongs to, however far its year — two matching clusters is ambiguous and
   *  refused. Two proofs:
   *
   *   - [[sharesADuplicateListing]]: a venue re-published the same listing under a
   *     wrong year (Odeon's rerelease season stamps every title with the current
   *     year; "The Hunger Games" at 2026 vs 2012). Neither row's own TMDB search can
   *     ever succeed, so left alone they are permanent ghosts.
   *   - [[decoratesResolvedCluster]]: a re-release banner keyed at the SCREENING's
   *     year at a venue the film has no print at ("Opętanie - plakatowa trasa…" 2026
   *     vs 1981) — the same containment the settle's `groupByFilm` edge already
   *     trusted to put the two in one component. */
  private def reclaimOrphans(resolved: Seq[Cluster], orphans: Seq[Row], normalizer: TitleNormalizer): (Seq[Cluster], Seq[Row]) = {
    val homes: Seq[(Row, Option[Int])] = orphans.map { row =>
      val matches = resolved.zipWithIndex.filter { case (c, _) =>
        c.rows.exists(cr => sharesADuplicateListing(cr._2, row._2)) || decoratesResolvedCluster(c, row, normalizer)
      }
      row -> Option.when(matches.lengthIs == 1)(matches.head._2)
    }
    (withHomedRows(resolved, homes), homes.collect { case (row, None) => row })
  }

  /** A venue's synopsis text for its cinema slots (live or retained). Cinema slots
   *  only: a shared TMDB/IMDb/Filmweb blurb says the same title-keyed lookup ran for
   *  both rows — the very same-title confusion the year window guards against. */
  private def slotTexts(r: MovieRecord): Map[Source, String] =
    (r.retainedSynopses.filter { case (_, text) => text.trim.nonEmpty } ++
      r.data.flatMap { case (source, sd) => sd.synopsis.filter(_.trim.nonEmpty).map(source -> _) })
      .filter { case (source, _) => Source.cinemaOf(source).isDefined }

  /** Same venue, byte-identical synopsis — two prints of one listing. A coincidental
   *  same-title different film would also have to coin the exact same blurb. */
  private def sharesADuplicateListing(a: MovieRecord, b: MovieRecord): Boolean = {
    val bTexts = slotTexts(b)
    slotTexts(a).exists { case (source, text) => bTexts.get(source).contains(text) }
  }

  /** Does `row`'s title decorate one of the cluster's titles/aliases (the
   *  `TitleContainment` + `MixedFilmDetector` guard `groupByFilm`'s containment edge
   *  uses)? Never when the row's title carries its OWN delimited year that disagrees
   *  with the cluster's — "It (1990)" beside a resolved 2017 "It" is the different
   *  film `EmbeddedYear` exists to keep apart, and a plain year is not a sequel marker
   *  `TitleContainment` would see — UNLESS the venue's published runtime agrees with
   *  the film's. A printed year alone cannot tell a release year from the event year a
   *  rerelease season brackets on (Odeon's "The Hunger Games: Catching Fire (2026)",
   *  146 minutes like TMDB's 2013 film; reverts 829eb309d / 3e4cbb3c5 are what trusting
   *  the text cost), so the veto needs a second, independent signal to stand down:
   *  the same runtime waiver [[contradictsHome]] gives a rule-4 straggler. Wallace's
   *  "It" runs 168 minutes against Muschietti's 135 and stays apart. */
  private def decoratesResolvedCluster(c: Cluster, row: Row, normalizer: TitleNormalizer): Boolean = {
    val whole              = TitleContainment.tokens(row._1.cleanTitle)
    val ownYearContradicts = EmbeddedYear.of(row._1.cleanTitle).exists(y => !c.refYear.contains(y)) &&
      !MixedFilmDetector.runtimesAgree(row._2.evidence.runtimes, tmdbRuntime(c).toSeq)
    val clusterTitles      = c.rows.flatMap { case (k, e) => (e.tmdbTitleAliases + k.cleanTitle).toSeq }.distinct.map(TitleContainment.tokens)
    !ownYearContradicts && whole.nonEmpty &&
      clusterTitles.exists(base => TitleContainment.decorates(base, whole)) &&
      !c.rows.exists(cr => MixedFilmDetector.describeDifferentFilms(cr._2, row._2, normalizer))
  }

  /** Rule 3: the remaining orphans form greedy windows from the lowest distinct year,
   *  each `YearWindow.PublishedAdjacency` wide — {y, y+1} absorbs every orphan at y or
   *  y+1, the next window opens at the next distinct year past it. */
  private def yearWindowClusters(orphans: Seq[Row]): Seq[Cluster] = {
    val windows   = scala.collection.mutable.ListBuffer.empty[Cluster]
    var remaining = orphans.map(_._1.year.get).distinct.sorted
    while (remaining.nonEmpty) {
      val lo = remaining.head
      val hi = lo + YearWindow.PublishedAdjacency
      windows += Cluster(refYear = Some(lo), rows = orphans.filter(r => r._1.year.get >= lo && r._1.year.get <= hi))
      remaining = remaining.dropWhile(_ <= hi)
    }
    windows.toSeq
  }

  /** Rule 4: yearless unresolved rows fold into the group's ONE plausible film — the
   *  single resolved cluster (which leads `seeded`), else the only cluster at all.
   *  Otherwise they stay singletons: a bare title that could be two films ("Guru" is
   *  three on TMDB; "Diuna" 1984/2021 before either resolves) would inherit whichever
   *  ranked first — an order-dependent wrong guess that then mis-pins its re-resolve.
   *  Never onto an unresolved orphan beside a resolved film: rule 2 already refused to
   *  identify the two. Even with one home, a row whose own evidence contradicts it
   *  stays apart ([[contradictsHome]]). */
  private def foldYearless(seeded: Seq[Cluster], resolvedCount: Int, yearless: Seq[Row],
                           normalizer: TitleNormalizer): Seq[Cluster] = {
    def singletons(rows: Seq[Row]) = rows.map(r => Cluster(refYear = None, rows = Seq(r)))
    if (resolvedCount != 1 && seeded.sizeIs != 1) seeded ++ singletons(yearless)
    else {
      val home             = seeded.head
      val (refuse, attach) = yearless.partition(contradictsHome(home, _, normalizer))
      (home.copy(rows = home.rows ++ attach) +: seeded.tail) ++ singletons(refuse)
    }
  }

  /** The runtime TMDB gives a cluster's film — the reference a row's published runtime
   *  is measured against. */
  private def tmdbRuntime(c: Cluster): Option[Int] =
    c.rows.flatMap(_._2.data.get(Tmdb).flatMap(_.runtimeMinutes)).headOption

  /** Does a yearless straggler's OWN published evidence contradict the film rule 4
   *  would fold it into? Title-silent venues are the norm (and not evidence), but a
   *  published year or runtime is: DE "Hope" (2026-09-16) folded a 2014 91-minute
   *  drama onto a 2026 Korean horror film for want of reading it.
   *
   *   - Year: tolerance `YearWindow.SlotYearImplausibility`, wider than rule 2's,
   *     because a yearless-KEY row's slot year is the noisy deferred-detail kind
   *     ("Głos Hind Rajab"'s Δ3 must still fold; "Hope"'s Δ12 must not). Waived when
   *     the runtime agrees closely — a rerelease stamped with its screening date
   *     (Kinoteka's "Happy Together", Δ29 but 96 min on both sides).
   *   - Runtime: `RuntimeCorroboration.plausible`.
   *   - A differing published original title (`MixedFilmDetector`).
   *
   *  The year and runtime readings stand down when the straggler credits a person the
   *  film credits — `MixedFilmDetector`'s own veto, for the same reason: a venue's
   *  typo manufactures a runtime (Kino Parczew's 9-minute "Vincent.legenda oceanu", a
   *  91-minute film by the Steven Majaury it names), while two unrelated films sharing
   *  a title do not also share a director. Without it the straggler stood apart only
   *  when it folded AFTER the film had TMDB's runtime, and the next rescrape's landing
   *  put it on the film anyway. */
  private def contradictsHome(home: Cluster, row: Row, normalizer: TitleNormalizer): Boolean = {
    val ev              = row._2.evidence
    val homeRuntime     = tmdbRuntime(home)
    val yearContradicts = YearWindow.contradicts(ev.years, home.refYear, YearWindow.SlotYearImplausibility)
    val runtimeAgrees   = MixedFilmDetector.runtimesAgree(ev.runtimes, homeRuntime.toSeq)
    val sharesCredit    = MixedFilmDetector.creditSamePerson(
      row._2.data.values.flatMap(_.director), home.rows.flatMap(_._2.data.values.flatMap(_.director)), normalizer)
    (!sharesCredit && ((yearContradicts && !runtimeAgrees) || !RuntimeCorroboration.plausible(ev.runtimes, homeRuntime))) ||
      home.rows.exists(cr => MixedFilmDetector.describeDifferentFilms(cr._2, row._2, normalizer))
  }

  /** Is this row's KEY one of the film's own TMDB titles (its Polish or original
   *  title) — i.e. a bare film title in some language, not a decorated edition? A
   *  translation ("Tangled" == originalTitle, "Zaplątani" == TMDB Polish title)
   *  qualifies; a dub / programme / festival variant ("Straszny film ukraiński
   *  dubbing", "Zaproszenie | Kinoteka dla rodziców") adds words beyond any alias
   *  and does NOT. Rows sharing a tmdbId now ALWAYS merge regardless of spelling
   *  (the read-model split renders a card per shown title from the one record), so
   *  this no longer gates the tmdbId fold; it still gates the cross-title ALIAS
   *  edge — an UNRESOLVED row is adopted onto a resolved row only when the resolved
   *  row is a bare film title, so a decorated edition's alias never adopts a
   *  genuinely-new bare film. */
  private[services] def isBareFilmTitle(row: (CacheKey, MovieRecord), normalizer: TitleNormalizer): Boolean = {
    val norm = normalizer.sanitize(row._1.cleanTitle)
    row._2.tmdbTitleAliases.exists(a => normalizer.sanitize(a) == norm)
  }

  /** Partition the corpus into FILM-IDENTITY components before per-film
   *  clustering. Two rows are the same film when they share a `sanitize(title)`
   *  OR — both being bare film titles (see [[isBareFilmTitle]]) — a tmdbId. The
   *  tmdbId edge is what folds a film keyed under two languages ("Tangled" /
   *  "Zaplątani", same tmdbId) into one component so the duplicate `movies` row
   *  collapses; gating it on bare titles keeps decorated editions (which carry the
   *  base tmdbId but are separate by design) in their own component.
   *
   *  Connected components via union-find over the row set — a pure, order-
   *  independent function: parents always point to the lowest index, components
   *  are returned sorted by their smallest `canonicalRank` (rows within each
   *  sorted too), so the settle stays deterministic (the `ScrapeOrderDeterminismSpec`
   *  guard). Replaces the prior `groupBy(sanitize)`: a sanitized-title group is
   *  always wholly inside one component (the sanitize edges union it), so every
   *  same-title row a per-title group saw still clusters together — plus the
   *  cross-title bare-alias rows. Each component is then sub-clustered by
   *  [[clusterByFilm]]. */
  def groupByFilm(rows: Seq[(CacheKey, MovieRecord)], normalizer: TitleNormalizer): Seq[Seq[(CacheKey, MovieRecord)]] = {
    val n      = rows.length
    val parent = Array.tabulate(n)(identity)
    def find(x: Int): Int = {
      var root = x
      while (parent(root) != root) root = parent(root)
      var cur = x
      while (parent(cur) != cur) { val next = parent(cur); parent(cur) = root; cur = next }
      root
    }
    def union(a: Int, b: Int): Unit = {
      val ra = find(a); val rb = find(b)
      if (ra != rb) parent(math.max(ra, rb)) = math.min(ra, rb)
    }
    def unionAllIndices(idxs: Iterable[Int]): Unit =
      idxs.reduceLeftOption { (a, b) => union(a, b); b }
    // sanitize(title) edges — always (preserves the prior title-scoped grouping).
    rows.indices.groupBy(i => normalizer.sanitize(rows(i)._1.cleanTitle))
      .valuesIterator.foreach(unionAllIndices)
    // tmdbId edges — connect EVERY pair of rows sharing a tmdbId: a film TMDB
    // resolves to one id is one record, however its rows are spelled (Polish /
    // Cyrillic / a dubbed or festival-decorated listing). The split back into a
    // card per shown title now lives in the read-model projection, so a
    // decorated edition no longer needs its own storage row to stay visible.
    rows.indices
      .filter(i => rows(i)._2.tmdbId.isDefined)
      .groupBy(i => rows(i)._2.tmdbId.get)
      .valuesIterator.foreach(unionAllIndices)
    // imdbId edges — the same statement one level up the identity ladder, for the case
    // the tmdbId edge cannot see: TMDB holding the SAME film under two ids. Two rows
    // TMDB resolved to one IMDb id are one film whatever their tmdbIds say, so they
    // belong in one component; whether they actually collapse is still `clusterByFilm`'s
    // call, and it refuses when the cinemas published different films.
    rows.indices
      .filter(i => rows(i)._2.imdbId.isDefined)
      .groupBy(i => rows(i)._2.imdbId.get)
      .valuesIterator.foreach(unionAllIndices)
    // tmdbTitleAlias edges — fold an UNRESOLVED row whose key sanitizes to one of a
    // RESOLVED row's TMDB titles (Polish / original / English) onto that row, even
    // though the straggler has no tmdbId of its own. The tmdbId edge above can only
    // connect two ALREADY-resolved rows; a cinema's English listing of a film whose
    // English-title TMDB search has no hit (only the Polish title resolves) never
    // gets its own tmdbId, so it would otherwise sit forever in a separate sanitize
    // group — its adoption hinged on `concludedKeyFor` redirecting it onto the
    // canonical at resolve time, which is order-dependent (the canonical must
    // already be concluded when the straggler is swept). This corpus-wide edge makes
    // the cross-title fold a pure function of the settled row set instead: the
    // straggler ("The mandalorian and grogu") unions with the resolved Polish row
    // ("Gwiezdne wojny: Mandalorian i Grogu", englishTitle alias "The Mandalorian
    // and Grogu") regardless of which resolved first. Gated on the alias SANITIZE-
    // EQUALLING the straggler's whole key, so a decorated edition that merely
    // contains the base title ("Zaproszenie | Kinoteka dla rodziców") never matches.
    val resolvedBareByAlias: Map[String, Seq[Int]] =
      rows.indices
        .filter(i => rows(i)._2.tmdbId.isDefined && isBareFilmTitle(rows(i), normalizer))
        .flatMap(i => rows(i)._2.tmdbTitleAliases.map(a => normalizer.sanitize(a) -> i))
        .groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    rows.indices.foreach { j =>
      val norm = normalizer.sanitize(rows(j)._1.cleanTitle)
      resolvedBareByAlias.get(norm).foreach(_.foreach(union(_, j)))
    }
    // SEARCH-TITLE edges — union rows whose decoration-STRIPPED title (the
    // `apiQuery` form the scrape uses for TMDB lookup) matches. A decorated edition
    // ("WAJDA: re-wizje: Człowiek z marmuru", "Ojczyzna - pokaz przedpremierowy",
    // "Klątwa doliny węży z autorską narracją", "Straszny film ukraiński dubbing")
    // has the SAME search title as its base film, so this folds it onto the base as
    // a PURE function of the titles — independent of whether the edition resolved
    // its OWN tmdbId. That independence is the whole point: a director-less decorated
    // string often can't resolve on its own, and whether it does was arrival-order-
    // dependent (it resolved only if a director-bearing sibling's slot happened to
    // merge in before the `StagingReaper` concluded it `tmdbNoMatch`) — the
    // `StagingOrderDeterminismSpec` flap. Anchoring the fold on the search title
    // removes order from the equation entirely.
    //
    // EXACT equality, not a substring: a different film that merely contains the base
    // words ("Moja Ojczyzna" vs "Ojczyzna") has a different search title and never
    // matches. Same-search-title REMAKES ("Diuna" 1984 vs 2021) land in one component
    // but split back into per-tmdbId clusters in `clusterByFilm` (whose rule-4
    // ambiguity-refuse leaves a yearless edition of an ambiguous title on its own).
    // `apiQuery` strips only the decorations its rules recognise, so an edition under
    // an UNrecognised banner keeps its own search title and stays separate —
    // deterministically, not by an order-dependent resolution race.
    // The union key is ROMANIZED (Cyrillic → Latin) so an unresolved Cyrillic
    // orphan folds onto its resolved Latin sibling: "Ваяна" (a Ukrainian-dubbed
    // listing TMDB never matched → no tmdbId) romanizes to the same "vaiana" as
    // the Polish/Latin "Vaiana" row, so this edge unites them even though neither
    // the tmdbId edge (the orphan has none) nor the alias edge (TMDB doesn't list
    // the Ukrainian title as an alias) could. Romanization is confined to THIS
    // grouping key — never the stored key, the display title, or the real TMDB
    // query (`apiQuery`), which stays in the original script. Exact-match gated,
    // so a lossy transliteration can only fail to fold, never mis-fold.
    rows.indices.groupBy(i => searchKey(rows(i)._1.cleanTitle, normalizer))
      .valuesIterator.foreach(unionAllIndices)
    // title-CONTAINMENT edges — the complement of the search-title edge, for banners
    // `apiQuery` does NOT recognise ("WAJDA: re-wizje: Człowiek z marmuru",
    // "Podziemny Krąg (Fight Club)"): those keep their decorated search title, so the
    // edge above can't fold them, and their own resolution is the arrival-order race
    // we're killing. Fold an UNRESOLVED edition onto a RESOLVED base whose title (or
    // alias) appears as a contiguous TOKEN-RUN inside the edition's — a PURE function
    // of titles + which rows resolved (and a bare base resolves deterministically on
    // its own title). Safeguards: the edition is strictly longer (a real decoration),
    // REFUSE ON AMBIGUITY (runs of two different resolved films → attach to neither),
    // and REFUSE ON CONTRADICTION (the edition's cinemas published a different film —
    // see below).
    // The predicate is `TitleContainment`'s — the same one the scrape-time divert gate
    // asks, so a decorated listing lands on its film's row instead of becoming a
    // newcomer this edge later folds.
    def titleTokens(s: String): Seq[String] = TitleContainment.tokens(s)
    val resolvedBaseRuns: Seq[(Seq[String], Int)] =
      rows.indices
        .filter(i => rows(i)._2.tmdbId.isDefined)
        .flatMap(i => (rows(i)._2.tmdbTitleAliases + rows(i)._1.cleanTitle).iterator.map(titleTokens(_) -> i))
        .toSeq
    // Index the resolved bases by their FIRST and LAST token so an unresolved row is
    // only checked against bases that could edge-match it, instead of every base
    // (this was O(unresolved × resolved) over the whole corpus each settle). A
    // token-run is `whole.startsWith(base)` (⟹ base.head == whole.head) OR
    // `whole.endsWith(base)` (⟹ base.last == whole.last), so the union of the two
    // buckets is a SUPERSET of the real matches and `isTokenRun` still filters
    // exactly — same result, far fewer comparisons.
    val nonEmptyBases    = resolvedBaseRuns.filter(_._1.nonEmpty)
    val basesByFirstToken = nonEmptyBases.groupBy(_._1.head)
    val basesByLastToken  = nonEmptyBases.groupBy(_._1.last)
    // A title that is ITSELF a resolved film's title is not a decoration of a shorter
    // one, and this edge only exists for decorations.
    //
    // The contradiction guard below asks the cinemas, and the cinemas can be silent:
    // `MixedFilmDetector` reads its evidence only from slots that publish an
    // `originalTitle`, so a venue that publishes none contradicts nothing however
    // plainly its other fields disagree. Poland, 2026-09-02: thirteen Cinema City
    // venues list "Ktoś całkiem obcy" (2024) at 104 minutes with no original title,
    // so the row carried no identity group at all; the edge read its tokens as a
    // decoration of the resolved one-token base "Obcy" (Ozon's "L'étranger", 2025,
    // 122 min) and `clusterByFilm` then attached it — 2024 being within ±2 of 2025.
    // Every settle moved those thirteen venues onto Ozon's film and every re-scrape
    // pulled them back, which is the churn the convergence leg kept failing on.
    //
    // The corpus already held the answer without asking any cinema anything: a
    // RESOLVED row keyed "Ktoś całkiem obcy" (tmdbId 7183, the 2007 *Perfect
    // Stranger*) sits right there, so that title is a film's own name — whichever
    // film it turns out to be — and the same-sanitize edge above has already put the
    // two in one component, where the year rules decide between them. Reaching past
    // that to adopt the row onto a DIFFERENT, shorter title is the edge overruling
    // the very clustering it feeds.
    val resolvedTitleKeys: Set[String] =
      rows.indices.iterator
        .filter(i => rows(i)._2.tmdbId.isDefined)
        .map(i => normalizer.sanitize(rows(i)._1.cleanTitle))
        .toSet
    rows.indices
      .filter(j => rows(j)._2.tmdbId.isEmpty &&
                   !resolvedTitleKeys.contains(normalizer.sanitize(rows(j)._1.cleanTitle)))
      .foreach { j =>
        val whole = titleTokens(rows(j)._1.cleanTitle)
        if (whole.nonEmpty) {
          val cands   = (basesByFirstToken.getOrElse(whole.head, Nil) ++ basesByLastToken.getOrElse(whole.last, Nil)).distinct
          // A token-run is a claim about the SPELLING of two titles, and spelling is
          // the one thing a same-named different film also satisfies: Kino Pionier's
          // "Ktoś całkiem obcy" (Brandt Andersen's "I Was A Stranger", 103 min) ends
          // with the whole of the resolved one-token base "Obcy" (Ozon's "L'étranger",
          // 122 min), so this edge adopted it and the collapse put two films on one
          // row. `MixedFilmSplitter` then split that row back out on the cinemas' own
          // published evidence, the fold gave the stray a row of its own, and this
          // edge adopted it again — a settle that never reaches a fixpoint, which is
          // how the Poland convergence leg found it. Refusing on the SAME evidence the
          // splitter uses is what makes the two agree: an edge may only fold what the
          // splitter would not split back out.
          // …and refusing on what the TITLE itself says, because the cinemas can be silent
          // on this axis too: UK slots publish an original title one time in nine, so
          // "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)" folded onto the 2012
          // "The Hunger Games" twenty times in nine days of logs with nothing to stop it.
          // A sequel carries the base title exactly the way a decoration does; the
          // ordinal is the difference (`SequelMarker`).
          val matched = cands.collect {
            case (base, i) if TitleContainment.decorates(base, whole) &&
              !MixedFilmDetector.describeDifferentFilms(rows(i)._2, rows(j)._2, normalizer) => i
          }
          if (matched.map(i => rows(i)._2.tmdbId.get).distinct.lengthIs == 1) matched.foreach(union(_, j))
        }
      }
    rows.indices.groupBy(find).valuesIterator.toSeq
      .map(idxs => idxs.toSeq.sortBy(i => canonicalRank(rows(i)._1)).map(rows))
      .sortBy(comp => comp.map(r => canonicalRank(r._1)).min)
  }

  /** The order [[canonical]] hands a cluster to `MovieRecordMerge.unionAll`, which
   *  takes the FIRST tmdbId-bearing row as the base — so this decides which identity
   *  (tmdbId, and the TMDB slot behind it) the surviving row keeps.
   *
   *  `canonicalRank` alone, for a cluster folded on a shared imdbId, would hand that to
   *  the lower YEAR — which says nothing about which of the two TMDB records is the
   *  right one. On prod that picked the wrong one: `ghost2bigtorig|2025`'s tmdbId
   *  resolves to a Def Leppard concert film, and folding on the shared imdbId would have
   *  replaced a correct card and a wrong one with a single wrong one. Let the cinemas
   *  break it instead — their published runtimes are exactly the evidence
   *  [[RuntimeCorroboration]] exists for — and fall back to `canonicalRank` when they
   *  are silent or split. A cluster with one tmdbId (every ordinary fold) is untouched.
   *
   *  The candidates are the FILMS (one per tmdbId), not the rows: every row of one
   *  tmdbId carries the same TMDB runtime, so asking per row made a film with two rows
   *  tie with itself, and `strictNearest` refuses a tie. Each film is represented by its
   *  best-`canonicalRank` row — the same rule [[survivor]] uses — so which row leads is
   *  a pure function of the row set. */
  private def mergeOrder(cluster: Seq[(CacheKey, MovieRecord)]): Seq[(CacheKey, MovieRecord)] = {
    val ranked = cluster.sortBy { case (k, _) => canonicalRank(k) }
    val films  = ranked.filter(_._2.tmdbId.isDefined).groupBy(_._2.tmdbId).values.toSeq
    if (films.sizeIs <= 1) ranked
    else RuntimeCorroboration.strictNearest(
      cluster.flatMap(_._2.evidence.runtimes).distinct,
      films.map(rows => rows.head -> rows.flatMap(_._2.data.get(Tmdb).flatMap(_.runtimeMinutes)).headOption)
    ).map(best => best +: ranked.filterNot(_._1 == best._1)).getOrElse(ranked)
  }

  /** The single canonical key + merged record for a cluster of same-film rows.
   *  Spelling is decoupled from year:
   *    - year:    TMDB's resolved year is authoritative (it overrides
   *      cinema-reported years, which often carry the production year and
   *      disagree — the "Dzień objawienia" 2025 vs 2026 split); only an
   *      all-unresolved cluster falls back to the lowest present year.
   *    - spelling: the min cleanTitle across ALL variants regardless of year, so
   *      a yearless all-caps variant ("SAVAGE HOUSE") can't win the spelling
   *      just because it's the only one at the resolved year.
   *
   *  The merge is `unionAll`, not `reduce(union)`: it picks the tmdbId-bearing
   *  row as the union base, so a lower-canonicalRank UNRESOLVED ±1 sibling (e.g.
   *  the production-year 2025 row attached to a TMDB-2026 resolved cluster)
   *  can't clobber the resolved row's tmdbId/imdbId/ratings. Order independent
   *  for the per-source `data` (it's a keyed merge). */
  def canonical(cluster: Seq[(CacheKey, MovieRecord)], normalizer: TitleNormalizer,
                extraCinemaTitles: Seq[String] = Nil): (CacheKey, MovieRecord) = {
    // Every reported variant: each CINEMA slot's derived title plus the rows'
    // current keys. Enrichment-source slots (Tmdb/Imdb/Filmweb) are excluded on
    // purpose: a row's identity spelling must come from what CINEMAS call it, not
    // from TMDB's title for the base film — otherwise a decorated variant (a dub
    // "Straszny film ukraiński dubbing", a programme "Kino Dostępne: …") whose
    // Tmdb slot carries the bare base title ("Straszny film") would canonicalise
    // to that base title and collapse onto the base row. Cinema titles keep the
    // variant distinct; `displayTitle` still derives a nice label separately.
    val slotKeys = cluster.flatMap { case (_, e) => e.cinemaData.values.flatMap(d => d.title.map(t => CacheKey(t, d.releaseYear, normalizer))) }
    val keys     = cluster.map(_._1)
    val allKeys  = slotKeys ++ keys
    val canonicalYear = clusterYear(cluster)
    // Prefer a normally-cased spelling over a SHOUTING one ("Savage House" over
    // "SAVAGE HOUSE"), then break ties by string order — a pure function of the
    // variant set.
    def isAllCaps(t: String): Boolean = t.exists(_.isLetter) && t == t.toUpperCase(java.util.Locale.ROOT)
    val minSpelling = allKeys.map(_.cleanTitle).minBy(t => (isAllCaps(t), t))
    val merged = MovieRecordMerge.unionAll(mergeOrder(cluster).map(_._2))
    // Always key on `displayTitle` (the dominant cinema-reported clean form → TMDB
    // PL title → recased `minSpelling` ladder) — the SAME spelling
    // `StoredMovieRecord.fromStorage` rebuilds a hydrated row's key from. Keying the
    // settle on `minSpelling` instead made the two disagree for a row whose cinema
    // slots spell the title differently (decorated "Federico Fellini: …" vs
    // "…SŁODKIE ŻYCIE"), so every hydrate/settle re-keyed it — the per-deploy flap.
    // A cross-title cluster (a film folded across two languages by shared tmdbId,
    // "Tangled" + "Zaplątani") ALSO needs this: keying on the alphabetical `min`
    // could pick an original-language title no cinema reports, so every localised
    // scrape would miss it (`ScrapeLanding.concludedKeyFor` matches by sanitize) and
    // re-spawn the duplicate. `minSpelling` remains the ladder's last-resort tie-
    // break. The rating SEARCH query is now case/diacritic-folded (FilmwebClient;
    // RT/MC already slug-fold), so this re-spelling no longer shifts which fixture
    // a rating lookup hits.
    val canonicalTitle = merged.displayTitle(minSpelling, normalizer, extraCinemaTitles)
    val canonicalKey   = CacheKey(canonicalTitle, canonicalYear, normalizer)
    (canonicalKey, merged)
  }
}
