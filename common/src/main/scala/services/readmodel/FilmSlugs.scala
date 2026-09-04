package services.readmodel

import models.ResolvedMovie

/** The `/{city}/movie/{slug}` addressing rule, for the whole corpus at once.
 *
 *  `tools.Slugify` folds a display title to a slug, and that fold is lossy in
 *  two different ways. "Rocky II" and "Rocky 2" are two spellings of ONE film
 *  that land on `rocky-2` — harmless, either row answers the address. But two
 *  genuinely DIFFERENT films can also share a title: Wanda Jakubowska's
 *  `zaproszenie|1986` and Olivia Wilde's `zaproszenie|2026`, or `lalka|1968`
 *  and `lalka|2026`. There the bare slug can only ever address one of them, and
 *  the other film became unreachable — its card rendered on the city page and
 *  linked to its namesake.
 *
 *  So the slug is assigned over the corpus rather than derived from a title in
 *  isolation:
 *
 *  - A title nothing else folds onto keeps its bare slug. This is the whole
 *    corpus bar a handful of films, so essentially every address in the app,
 *    the sitemap, and every already-shared link is unchanged.
 *  - When several films share a fold, the NEWEST keeps the bare slug and the
 *    others are qualified with their release year (`zaproszenie-1986`). Newest
 *    rather than oldest because the bare, prettiest address should belong to
 *    the film people are currently searching for; the repertory revival is the
 *    one that gains an address it never had.
 *
 *  A qualified slug is checked against the WHOLE corpus, not just its own
 *  collision group: a title can itself end in a year ("Kultowa klasyka:
 *  Absolwent (1967)" folds to `kultowa-klasyka-absolwent-1967`), so appending
 *  one can walk onto a film that already owns that address. The ladder falls
 *  through to a numeric suffix if it has to, which keeps the assignment total
 *  and one-to-one no matter what the corpus holds.
 *
 *  Assignment is deterministic — a pure function of the corpus, ordered by
 *  `(releaseYear desc, title, _id)` — so the same read model always produces
 *  the same addresses, and two web pods serve the same links. Films are
 *  identified by `_id` rather than title precisely because titles are what
 *  collide here.
 */
final class FilmSlugs private(private val idToSlug: Map[String, String],
                              private val slugToId: Map[String, String]) {

  /** This film's address, or `None` for a title that folds to nothing
   *  addressable (entirely punctuation, or a script the fold doesn't cover) —
   *  those keep the legacy `?title=` query form. */
  def slugFor(id: String): Option[String] = idToSlug.get(id)

  /** The film a slug addresses. `None` for an unknown slug — callers fall back
   *  to their own resolution (re-slugging what a city is showing) so a film the
   *  read model has dropped mid-reprojection still resolves. */
  def idFor(slug: String): Option[String] = slugToId.get(slug)

  def size: Int = idToSlug.size
}

object FilmSlugs {

  val empty: FilmSlugs = new FilmSlugs(Map.empty, Map.empty)

  def apply(movies: Seq[ResolvedMovie]): FilmSlugs = {
    // Bare fold per film, dropping those with no addressable slug at all.
    val folded: Seq[(ResolvedMovie, String)] =
      movies.flatMap(m => Option(tools.Slugify(m.title)).filter(_.nonEmpty).map(m -> _))

    // Newest first, then title, then id: total and stable, so the film that
    // keeps the bare slug never depends on the read model's iteration order. A
    // film with no year sorts last — it can't be qualified with one either, so
    // it is the worst candidate to hold a contested address.
    val ordered = folded.sortBy { case (m, _) =>
      (m.releaseYear.isEmpty, -m.releaseYear.getOrElse(0), m.title, m._id)
    }

    val taken  = collection.mutable.Set.empty[String]
    val idTo   = collection.mutable.Map.empty[String, String]
    val slugTo = collection.mutable.Map.empty[String, String]

    // Two passes: every bare slug's rightful owner claims it before anyone
    // qualifies, so a qualified slug can never displace a film whose own title
    // folds to that exact address.
    val ownerOf: Map[String, String] = ordered.groupBy(_._2).map { case (slug, es) => slug -> es.head._1._id }
    val (owners, rest) = ordered.partition { case (m, slug) => ownerOf(slug) == m._id }

    def claim(m: ResolvedMovie, slug: String): Unit = {
      taken += slug
      idTo += (m._id -> slug)
      slugTo += (slug -> m._id)
    }

    owners.foreach { case (m, slug) => claim(m, slug) }

    rest.foreach { case (m, slug) =>
      val year       = m.releaseYear.map(y => s"$slug-$y")
      val candidates = year.iterator ++ Iterator.from(2).map(n => s"${year.getOrElse(slug)}-$n")
      claim(m, candidates.find(!taken.contains(_)).get)
    }

    new FilmSlugs(idTo.toMap, slugTo.toMap)
  }
}
