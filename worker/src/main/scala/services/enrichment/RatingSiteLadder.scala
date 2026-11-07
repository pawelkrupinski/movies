package services.enrichment

import tools.{HttpFetch, MemoizedHttpFetch}

/**
 * How a rating site (Metacritic, Rotten Tomatoes, Filmweb) is asked for one film
 * under several candidate titles: as ONE attempt that shares a
 * [[MemoizedHttpFetch]], so a URL two titles both reach is fetched once.
 *
 * Every site has several titles to try (TMDB's original title, its en-US title,
 * the US release title; Filmweb's decorated-title variants) and used to run its
 * ladder once per title against the live site, so slugs an earlier title had
 * already probed were probed again ("The Sting" and "Sting" both end at
 * `/movie/sting`). The order is each title's full ladder in turn, the
 * `fallback` belonging to the first title only, the first answer winning; later
 * titles are never asked once one answers.
 */
object RatingSiteLadder {

  /** Runs `ladder` for each of `titles` in turn on a client built over one fresh
   *  fetch memo, and returns the first answer. */
  def across[Client, A](http: HttpFetch, client: HttpFetch => Client)(titles: Seq[String], fallback: Option[String] = None)(
    ladder: (Client, String, Option[String]) => Option[A]
  ): Option[A] = {
    val attempt = client(new MemoizedHttpFetch(http))
    titles.iterator.zipWithIndex
      .flatMap { case (title, index) => ladder(attempt, title, if (index == 0) fallback else None) }
      .nextOption()
  }

  /** One title's ladder on a slug-probing site: the title's slug probes, the
   *  fallback's slug probes, then the search-page pick for each. A fallback that
   *  only repeats the title (case-insensitively) is not asked again. */
  def probe[A](title: String, fallback: Option[String])(slugProbe: String => Option[A], searchPick: String => Option[A]): Option[A] = {
    val effectiveFallback = fallback.filterNot(_.equalsIgnoreCase(title))
    slugProbe(title)
      .orElse(effectiveFallback.flatMap(slugProbe))
      .orElse(searchPick(title))
      .orElse(effectiveFallback.flatMap(searchPick))
  }
}
