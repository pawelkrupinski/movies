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
}
