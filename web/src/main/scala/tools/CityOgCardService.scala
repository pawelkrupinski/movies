package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.util.concurrent.TimeUnit

/**
 * Orchestrates the per-city Open Graph card: fetch + decode a handful of the
 * city's current posters, hand them to the pure [[OgCardRenderer.renderCityCard]],
 * and memoise the PNG by city so repeated shares + Facebook's re-scrapes don't
 * re-fetch posters and re-rasterise every time.
 *
 * The card is fully dynamic — no committed image, no headless browser. The
 * decorative rating pills are a fixed sample (the card sells "we show ratings",
 * not any one film's score). `posters` is injected so tests drive a fake
 * returning fixture bytes and never touch the network.
 */
class CityOgCardService(posters: PosterFetch) {
  private val loader = new PosterImageLoader(posters)
  private val cache: Cache[String, Array[Byte]] =
    Caffeine.newBuilder().maximumSize(200).expireAfterWrite(12, TimeUnit.HOURS).build()

  // Decorative IMDb·Metacritic·RT·Filmweb pills (same look as the static cards);
  // the city card advertises that we carry ratings, not a specific film's score.
  private val sampleBadges =
    OgCardRenderer.ratingBadges(imdb = Some(7.8), metascore = Some(81), rottenTomatoes = Some(91), filmweb = Some(7.4))

  /** `posterUrls` is the city's current films' posters, in display order. We try
   *  the first [[CityOgCardService.MaxCandidates]] until [[CityOgCardService.Montage]]
   *  decode, then render. Cached by `cacheKey` (the city slug) for 12h; FB/X
   *  cache per-URL on top. A transient all-fail fetch isn't cached, so the next
   *  share retries instead of freezing a gradient-only card. */
  def card(cacheKey: String, cityLine: String, posterUrls: Seq[String]): Array[Byte] =
    Option(cache.getIfPresent(cacheKey)).getOrElse {
      val candidates = posterUrls.filter(_.nonEmpty).distinct.take(CityOgCardService.MaxCandidates)
      val images     = candidates.iterator.flatMap(loader.load(_).iterator).take(CityOgCardService.Montage).toVector
      val bytes      = OgCardRenderer.renderCityCard(cityLine, images, sampleBadges)
      if (images.nonEmpty) cache.put(cacheKey, bytes)
      bytes
    }
}

object CityOgCardService {
  /** Posters tiled into the montage grid (6×2). */
  private val Montage = 12
  /** Cap on URLs fetched before giving up, so a city whose first posters all
   *  fail can't stack dozens of slow connect timeouts on one request. */
  private val MaxCandidates = 24
}
