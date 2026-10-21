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
 *  `(releaseYear desc, title, tmdbId, _id)` — so the same read model always
 *  produces the same addresses, and two web pods serve the same links. Films
 *  are identified by `_id` rather than title precisely because titles are what
 *  collide here.
 *
 *  THE SLUG FOLDS THE FILM'S CLEAN TITLE, not the billing the card shows. A card
 *  can carry a cinema's event billing — "Kino na obcasach: Jak żyć, żeby nie
 *  zwariować" — and folding that put the film at
 *  `/poznan/movie/kino-na-obcasach-jak-zyc-zeby-nie-zwariowac`. `clean` is the
 *  banner/decoration strip external lookups already use (`TitleRuleSet.search`),
 *  and a decorated card claims the clean address when nothing else holds it. The
 *  plain listing of a title always outranks a decorated one for it: while the plain
 *  card exists, the event card keeps the billing address it already had (its own
 *  page, its own showtimes) instead of a year-qualified clean slug that would read
 *  as a different film. An address a card USED to have under the billing fold stays
 *  answerable through [[formerIdFor]], which the controller 301s onto the new one.
 */
final class FilmSlugs private(private val idToSlug: Map[String, String],
                              private val slugToId: Map[String, String],
                              private val formerToId: Map[String, String]) {

  /** This film's address, or `None` for a title that folds to nothing
   *  addressable (entirely punctuation, or a script the fold doesn't cover) —
   *  those keep the legacy `?title=` query form. */
  def slugFor(id: String): Option[String] = idToSlug.get(id)

  /** The film a slug addresses. `None` for an unknown slug — callers fall back
   *  to their own resolution (re-slugging what a city is showing) so a film the
   *  read model has dropped mid-reprojection still resolves. */
  def idFor(slug: String): Option[String] = slugToId.get(slug)

  /** The film a slug USED to address — its billing-title fold, before addresses
   *  folded the clean title — when that slug is nobody's address now. Callers
   *  301 it onto the film's current slug, so every link already shared keeps
   *  landing on its film. */
  def formerIdFor(slug: String): Option[String] = formerToId.get(slug)
}

object FilmSlugs {

  /** Newest first, then title, then IMDb address, then film id: total and stable, so
   *  the film that keeps the bare slug never depends on the read model's
   *  iteration order. A film with no year sorts last — it can't be qualified
   *  with one either, so it is the worst candidate to hold a contested address.
   *  [[FilmTitles]] lists a same-title pair in this order too, so both indexes
   *  agree on which film a bare title means.
   *
   *  WHY THE IMDb ADDRESS SITS BEFORE `_id`, which is the whole reason this is not
   *  just the obvious three fields. Two films that share a title AND a year are
   *  separated only by the last field, and since 2026-09-07 `_id` is an opaque
   *  `FilmId` minted from whichever key the row was FIRST created under — a
   *  function of arrival order, not of the film (see `docs/design/identity-resolver.md` §6).
   *  That made the bare address flip between two same-titled films whenever a
   *  worker rebuilt its corpus in a different order: a shared link would open a
   *  different film, and every replay of one corpus disagreed with the last
   *  (caught by the Polish order-independence leg on `lalka` / `lalka-2026`).
   *  The IMDb address is the film's identity at the source, it is already on
   *  the card, and it does not move, so it decides first — a film that has one
   *  also outranks a film that has none, since an unresolved row is the worse
   *  holder of a contested address. `_id` remains only as the total-order
   *  backstop for a pair with neither a distinguishing year nor an IMDb id. */
  private[readmodel] def newestFirst(m: ResolvedMovie): (Boolean, Int, String, Boolean, String, String) =
    (m.releaseYear.isEmpty, -m.releaseYear.getOrElse(0), m.title,
     m.ratings.imdbUrl.isEmpty, m.ratings.imdbUrl.getOrElse(""), m._id)

  /** The addressing for a country's corpus: [[apply]] with that country's
   *  decoration strip as the clean title. Compiles the country's rule set once —
   *  hold the returned function, never call this per corpus. */
  def forCountry(country: models.Country): Seq[ResolvedMovie] => FilmSlugs = {
    val rules = services.titlerules.TitleRuleSet.forCountry(country)
    movies => apply(movies, rules.search)
  }

  def apply(movies: Seq[ResolvedMovie], clean: String => String = identity): FilmSlugs = {
    val current = assign(movies, clean)
    // What the billing fold assigned (identical whenever no title cleans to something
    // else, so this costs one more pass and no map for an undecorated corpus).
    val former  = if (current.cleaned) assign(movies, identity).idTo else Map.empty[String, String]
    val formerToId = former.collect { case (id, slug) if !current.slugTo.contains(slug) && !current.idTo.get(id).contains(slug) => slug -> id }
    new FilmSlugs(current.idTo, current.slugTo, formerToId)
  }

  private final case class Assigned(idTo: Map[String, String], slugTo: Map[String, String], cleaned: Boolean)

  private def fold(title: String): Option[String] = Option(tools.Slugify(title)).filter(_.nonEmpty)

  private def assign(movies: Seq[ResolvedMovie], clean: String => String): Assigned = {
    // Per film: the billing fold, and the clean-title fold (the billing fold when the
    // clean title folds to nothing addressable). Films with no billing fold drop out.
    val folded: Seq[(ResolvedMovie, String, String)] =
      movies.flatMap(m => fold(m.title).map(raw => (m, raw, fold(clean(m.title)).getOrElse(raw))))

    val ordered = folded.sortBy { case (m, _, _) => newestFirst(m) }

    val taken  = collection.mutable.Set.empty[String]
    val idTo   = collection.mutable.Map.empty[String, String]
    val slugTo = collection.mutable.Map.empty[String, String]

    def claim(m: ResolvedMovie, slug: String): Unit = {
      taken += slug
      idTo += (m._id -> slug)
      slugTo += (slug -> m._id)
    }

    val (plain, decorated) = ordered.partition { case (_, raw, cln) => raw == cln }

    // Three passes. First every bare slug's rightful owner among the PLAIN titles claims
    // it, so neither a qualified slug nor a decorated card can displace a film whose own
    // title folds to that exact address.
    val ownerOf: Map[String, String] = plain.groupBy(_._2).map { case (slug, es) => slug -> es.head._1._id }
    val (owners, rest) = plain.partition { case (m, slug, _) => ownerOf(slug) == m._id }
    owners.foreach { case (m, slug, _) => claim(m, slug) }

    // Then each decorated card takes its clean address if it is free, else the billing
    // address it always had.
    val unplaced = decorated.filter { case (m, raw, cln) =>
      Seq(cln, raw).find(!taken.contains(_)).map(claim(m, _)).isEmpty
    }

    // Last, whoever is left qualifies its billing fold with its year.
    (rest ++ unplaced).foreach { case (m, slug, _) =>
      val year       = m.releaseYear.map(y => s"$slug-$y")
      val candidates = year.iterator ++ Iterator.from(2).map(n => s"${year.getOrElse(slug)}-$n")
      claim(m, candidates.find(!taken.contains(_)).get)
    }

    Assigned(idTo.toMap, slugTo.toMap, cleaned = decorated.nonEmpty)
  }
}
