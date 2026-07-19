package services.resolution

import services.movies.TitleNormalizer

/**
 * Deterministic, order-independent keys for the per-source [[ResolutionCache]]s.
 * Each builder includes exactly the hints that source's resolver consumes, so
 * the cache deduplicates per real hint-combination — no more, no less.
 *
 * "Order-independent" is load-bearing: `ScrapeOrderDeterminismSpec` proves the
 * pipeline resolves the same film regardless of scrape/merge order, and the
 * cache key must not reintroduce order dependence. Titles are `sanitize`d,
 * director lists are `distinct.sorted`, and Options collapse to a stable token,
 * so the key is a pure function of the hint SET.
 */
object ResolutionKeys {
  // `sanitize` strips a title to letters+digits only, so `|` never appears
  // inside a field — a safe, collision-free separator between key fields.
  private val Sep = "|"

  private def norm(title: String): String = TitleNormalizer.sanitize(title)
  private def year(y: Option[Int]): String = y.map(_.toString).getOrElse("")
  private def directors(ds: Iterable[String]): String =
    ds.map(norm).filter(_.nonEmpty).toSeq.distinct.sorted.mkString(",")
  private def opt(s: Option[String]): String = s.map(norm).getOrElse("")

  /** TMDB id resolution: search title + year + director set + original title. */
  def tmdb(cleanTitle: String, year: Option[Int], directors: Iterable[String], originalTitle: Option[String]): String =
    Seq("tmdb", norm(cleanTitle), this.year(year), this.directors(directors), opt(originalTitle)).mkString(Sep)

  /** IMDb id recovery: search title + year. */
  def imdb(searchTitle: String, year: Option[Int]): String =
    Seq("imdb", norm(searchTitle), this.year(year)).mkString(Sep)

  /** Filmweb url: title + year + fallback title + director set. */
  def filmweb(title: String, year: Option[Int], fallback: Option[String], directors: Iterable[String]): String =
    Seq("filmweb", norm(title), this.year(year), opt(fallback), this.directors(directors)).mkString(Sep)

  /** Rotten Tomatoes url: title + fallback title + year. */
  def rt(title: String, fallback: Option[String], year: Option[Int]): String =
    Seq("rt", norm(title), opt(fallback), this.year(year)).mkString(Sep)

  /** Metacritic url: title + fallback title + year. */
  def mc(title: String, fallback: Option[String], year: Option[Int]): String =
    Seq("mc", norm(title), opt(fallback), this.year(year)).mkString(Sep)

  /** True when `hintKey` was built for the film whose cache key title is
   *  `cleanTitle` — i.e. that title appears as one of the key's fields.
   *
   *  Every builder above places the film's sanitized title in SOME field, but
   *  not the same one: `tmdb`/`imdb`/`filmweb` lead with it, while `mc`/`rt` lead
   *  with the ORIGINAL title and carry the film's own title in the fallback field
   *  (`mc|theodyssey|odyseja|2026`). Matching on whole fields — never a substring
   *  — is what keeps that uniform: `sanitize` strips everything but letters and
   *  digits, so `|` cannot occur inside a field.
   *
   *  Deliberately NOT year-scoped. The year in a key is the resolver's (TMDB's
   *  release year), which can differ from the row's cache-key year, so filtering
   *  on it would silently miss the entry we most want gone. A forced re-enrich
   *  means "forget what we concluded about this title", and clearing a same-title
   *  remake's entry too is harmless — it re-resolves on its next pass. */
  def belongsTo(hintKey: String, cleanTitle: String): Boolean = {
    val title = norm(cleanTitle)
    // -1 keeps trailing empty fields (an absent fallback / year), so a key ending
    // in a blank field still splits into the full field list.
    title.nonEmpty && hintKey.split("\\|", -1).contains(title)
  }
}
