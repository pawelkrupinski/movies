package tools

import models.{CinemaMovie, Country}
import services.movies.{SequelMarker, TitleContainment, TitleNormalizer}
import services.scrapes.ArchivedScrape

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * The HARD CLUSTERS of a country's corpus: the few hundred listings whose folding has
 * actually gone wrong, or is shaped like something that has — franchise siblings,
 * decorated and programme-prefixed spellings, a title reported under two years, two
 * different films under one title, and every film a convergence run ever reported as
 * divergent.
 *
 * The full convergence legs replay a whole country, which is what makes them the
 * authority and also what makes them take one and a half to five hours to say so. Every
 * fold/settle regression they caught in their first months lived in a handful of these
 * clusters, and the rest of the corpus was only the haystack. Replayed on their own the
 * clusters take seconds, so the same order-independence and churn claims run in the
 * `itAll` layer on every push (`HardClusterConvergenceIntegrationSpec`).
 *
 * A cluster is picked WHOLE — every listing of every spelling the corpus holds for it,
 * up to [[MaxListingsPerCluster]] — because the bug is in how the spellings meet: one
 * listing of "Mockingjay - Part 1 (2026)" alone folds perfectly well.
 *
 * The selection is recorded once and CHECKED IN (`cinema-scrapes-hard-clusters-<cc>`),
 * never recomputed by the spec, so a test run is a pure function of the file. It only
 * GROWS: [[extend]] adds a cluster (the ratchet a failing convergence run feeds) without
 * disturbing the listings already there.
 */
object HardClusters {

  /** Why a cluster is in the fixture — the category is printed next to every film the
   *  spec reports, so a failure says which kind of hard case moved. */
  enum Reason(val label: String) {
    case Finding     extends Reason("finding")
    case Franchise   extends Reason("franchise")
    case Decorated   extends Reason("decorated")
    case Programme   extends Reason("programme")
    case YearSplit   extends Reason("year-split")
    case TwoFilms    extends Reason("same-title-different-film")
    /** The widest releases: no special shape, but the most venues folding one film —
     *  the cross-venue pressure a country with few decorated titles still has. */
    case Wide        extends Reason("wide")
  }

  /** A cluster's listings, from venues in name order: a wide release keeps enough venues
   *  to exercise the cross-venue fold (every distinct spelling and year first, then the
   *  rest) without one film becoming the fixture. */
  val MaxListingsPerCluster = 8

  /** The AUTOMATIC picks across every country, on top of every seed's cluster (the
   *  seeds are the ~150 listings that have already gone wrong, and are never dropped
   *  for budget). Together about 300-350 listings. */
  val DefaultBudget = 200

  def corpusKey(country: Country): String = s"hard-clusters-${country.code}"

  /** The seeds every past cluster was picked from, one `code<TAB>title<TAB>reason` per
   *  line — the provenance of the fixture, and what the ratchet appends to. */
  val SeedsPath: Path = Paths.get("test", "resources", "fixtures", "corpus", "hard-clusters-seeds.tsv")

  final case class Seed(country: String, title: String, reason: String)

  def readSeeds(path: Path = SeedsPath): Seq[Seed] =
    if (!Files.exists(path)) Nil
    else Files.readAllLines(path, StandardCharsets.UTF_8).toArray(Array.empty[String]).toSeq
      .map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#"))
      .flatMap(_.split("\t", -1) match {
        case Array(c, t, r, _*) => Some(Seed(c.trim.toLowerCase, t.trim, r.trim))
        case Array(c, t)        => Some(Seed(c.trim.toLowerCase, t.trim, Reason.Finding.label))
        case _                  => None
      })

  def appendSeeds(seeds: Seq[Seed], path: Path = SeedsPath): Unit = {
    val known = readSeeds(path).map(s => (s.country, s.title)).toSet
    val fresh = seeds.filterNot(s => known.contains((s.country, s.title))).distinctBy(s => (s.country, s.title))
    if (fresh.nonEmpty) {
      Files.createDirectories(path.getParent)
      val header = if (Files.exists(path)) "" else "# country\ttitle\treason — see tools.HardClusters\n"
      Files.writeString(path, header + fresh.map(s => s"${s.country}\t${s.title}\t${s.reason}\n").mkString,
        StandardCharsets.UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
    }
  }

  /** One listing, addressed by the venue that published it. */
  private final case class Listing(venue: Int, film: CinemaMovie)

  /** The identity a cluster is built around: the search form of the title (decoration
   *  and programme banners stripped, the same query a resolver sends), as tokens. Two
   *  spellings of one film share it; two films sharing a title share it too, which is
   *  exactly the collision the fixture wants. */
  private def baseTokens(title: String, normalizer: TitleNormalizer): Seq[String] =
    TitleContainment.tokens(normalizer.searchQuery(title))

  private def rawKey(title: String, normalizer: TitleNormalizer): String = normalizer.sanitize(title)

  /** A seed's cluster: every listing whose search form STARTS with the seed's first
   *  three tokens (a franchise and its entries, a title and its decorated re-releases),
   *  or whose raw title contains the seed's (a programme banner in front of it). */
  private def matchesSeed(seedTokens: Seq[String], seedRaw: String, title: String, normalizer: TitleNormalizer): Boolean =
    seedTokens.nonEmpty && (
      baseTokens(title, normalizer).startsWith(seedTokens.take(3)) ||
      (seedRaw.length >= 4 && rawKey(title, normalizer).contains(seedRaw)))

  /**
   * Pick the clusters of one country: every seed of `seeds` first, then the automatic
   * categories in turn until they have added `budget` listings. Deterministic — clusters are
   * ranked by a stable score and ties broken by name — so the same corpus and seeds
   * always write the same file.
   */
  def select(country: Country, rows: Seq[ArchivedScrape], seeds: Seq[Seed], budget: Int): (Seq[ArchivedScrape], Seq[Seed]) = {
    val normalizer = TitleNormalizer.forCountry(country)
    val indexed    = rows.toIndexedSeq.sortBy(_.cinema.displayName)
    val listings   = indexed.indices.flatMap(v => indexed(v).films.map(Listing(v, _)))
    val byBase     = listings.groupBy(l => baseTokens(l.film.movie.title, normalizer).mkString(" ")).filter(_._1.nonEmpty)

    def cap(members: Seq[Listing], limit: Int = MaxListingsPerCluster): Seq[Listing] = {
      val ordered = members.sortBy(l => (indexed(l.venue).cinema.displayName, l.film.movie.title))
      // Every distinct spelling and every distinct year first — those are what meet in
      // the fold — then the remaining venues in name order.
      // The RAW spelling, not the sanitized one: sanitize folds "Part 1 (2026)" onto
      // "Part 1", and that re-release decoration is precisely what split the UK corpus.
      val diverse = ordered.distinctBy(l => (l.film.movie.title.trim.toLowerCase, l.film.movie.releaseYear))
      (diverse ++ ordered.filterNot(diverse.contains)).take(limit)
    }

    val seedClusters: Seq[(Seed, Seq[Listing])] = seeds.filter(_.country == country.code).map { seed =>
      val tokens = baseTokens(seed.title, normalizer)
      val raw    = rawKey(seed.title, normalizer)
      // Twice the room of an automatic pick: a seed is a cluster that has ALREADY gone
      // wrong, and a franchise seed spans several entries.
      seed -> cap(listings.filter(l => matchesSeed(tokens, raw, l.film.movie.title, normalizer)), 2 * MaxListingsPerCluster)
    }

    def spellings(ls: Seq[Listing]) = ls.map(_.film.movie.title.trim.toLowerCase).distinct
    def years(ls: Seq[Listing])     = ls.flatMap(_.film.movie.releaseYear).distinct.sorted
    def venues(ls: Seq[Listing])    = ls.map(_.venue).distinct.size

    // The automatic categories, each a ranked list of (display title, listings).
    val decorated = byBase.toSeq.filter { case (_, ls) => spellings(ls).size >= 2 && venues(ls) >= 2 }
      .sortBy { case (k, ls) => (-spellings(ls).size, -venues(ls), k) }
    val programme = byBase.toSeq.filter { case (_, ls) =>
        ls.exists(l => normalizer.programmePrefix(l.film.movie.title).isDefined) && venues(ls) >= 2 }
      .sortBy { case (k, ls) => (-venues(ls), k) }
    val yearSplit = byBase.toSeq.filter { case (_, ls) => years(ls).size >= 2 && years(ls).last - years(ls).head <= 2 }
      .sortBy { case (k, ls) => (-venues(ls), k) }
    val twoFilms = byBase.toSeq.filter { case (_, ls) =>
        (years(ls).size >= 2 && years(ls).last - years(ls).head > 2) ||
        ls.map(_.film.director.map(_.toLowerCase).sorted).filter(_.nonEmpty).distinct.size >= 2 }
      .sortBy { case (k, ls) => (-venues(ls), k) }
    // Franchise siblings: two base groups sharing their leading tokens that SequelMarker
    // reads as different entries. Bucketed by the first two tokens so the pair search
    // stays linear-ish on a 100k-listing corpus.
    val franchise = byBase.toSeq.groupBy(_._1.split(' ').take(2).mkString(" ")).values.toSeq.flatMap { bucket =>
      val sorted = bucket.sortBy(_._1)
      for {
        (a, la) <- sorted
        (b, lb) <- sorted if a < b && SequelMarker.differentInstalments(a.split(' ').toSeq, b.split(' ').toSeq)
      } yield (s"$a | $b", la ++ lb)
    }.sortBy { case (k, ls) => (-venues(ls), k) }

    val automatic: Seq[(Reason, Seq[(String, Seq[Listing])])] = Seq(
      Reason.Franchise -> franchise, Reason.Decorated -> decorated, Reason.Programme -> programme,
      Reason.YearSplit -> yearSplit, Reason.TwoFilms -> twoFilms,
      Reason.Wide -> byBase.toSeq.filter { case (_, ls) => venues(ls) >= 3 }.sortBy { case (k, ls) => (-venues(ls), k) })

    println(s"[hard-clusters] ${country.code}: candidate clusters " +
      automatic.map { case (r, cs) => s"${r.label}=${cs.size}" }.mkString(", ") +
      s"; seeds ${seedClusters.map { case (s, ls) => s"${s.title}=${ls.size}" }.mkString(", ")}")
    val taken  = scala.collection.mutable.LinkedHashSet.empty[Listing]
    val picked = scala.collection.mutable.ListBuffer.empty[Seed]
    seedClusters.foreach { case (_, ls) => taken ++= ls }
    val limit = taken.size + budget
    // Round-robin over the categories so each gets a share of the budget, rather than
    // the first category eating all of it.
    val cursors = automatic.map(_._2.iterator).toArray
    var progress = true
    while (taken.size < limit && progress) {
      progress = false
      automatic.indices.foreach { i =>
        val it = cursors(i)
        var added = false
        while (!added && it.hasNext && taken.size < limit) {
          val (key, ls) = it.next()
          val fresh = cap(ls).filterNot(taken.contains)
          if (fresh.nonEmpty && taken.size + fresh.size <= limit + MaxListingsPerCluster / 2) {
            taken ++= fresh
            picked += Seed(country.code, ls.map(_.film.movie.title).minOption.getOrElse(key), automatic(i)._1.label)
            added = true
            progress = true
          }
        }
      }
    }
    (keep(indexed, taken.toSet.map(l => (l.venue, l.film))), picked.toList)
  }

  /** The listings `keep` names, and only the venues left holding any — every other
   *  property of a venue row carried through untouched. */
  private def keep(rows: IndexedSeq[ArchivedScrape], kept: Set[(Int, CinemaMovie)]): Seq[ArchivedScrape] =
    rows.indices.flatMap { v =>
      val row = rows(v)
      row.lastSuccess.map(s => row.copy(lastSuccess = Some(s.copy(films = s.films.filter(f => kept.contains((v, f)))))))
    }.filter(_.films.nonEmpty)

  /**
   * The RATCHET: `existing` plus the clusters of `seeds`, drawn from a (newer) full
   * corpus. Listings already in the fixture stay exactly as they are; a venue both hold
   * gains the new listings beside its old ones. Growing rather than re-selecting is what
   * makes the fixture a ratchet — a cluster that once caught a bug is never swapped out
   * because a newer corpus ranks something else higher.
   */
  def extend(country: Country, existing: Seq[ArchivedScrape], fullCorpus: Seq[ArchivedScrape], seeds: Seq[Seed]): Seq[ArchivedScrape] = {
    val (added, _) = select(country, fullCorpus, seeds, budget = 0)
    val byVenue    = existing.map(r => r.cinema -> r).toMap
    val merged = added.map { row =>
      byVenue.get(row.cinema) match {
        case None      => row
        case Some(old) =>
          val have = old.films.map(f => (f.movie.title, f.filmUrl)).toSet
          old.copy(lastSuccess = old.lastSuccess.map(s =>
            s.copy(films = s.films ++ row.films.filterNot(f => have.contains((f.movie.title, f.filmUrl))))))
      }
    }
    val touched = merged.map(_.cinema).toSet
    (existing.filterNot(r => touched.contains(r.cinema)) ++ merged).sortBy(_.cinema.displayName)
  }
}
