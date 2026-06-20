package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.util.concurrent.TimeUnit

/**
 * One film column of the city's page-like share card — the data
 * [[OgCardRenderer.renderCityPageCard]] draws per `.card`.
 *
 * @param meta       small grey pills (runtime, year, up to a couple of genres)
 * @param badges     rating pills (IMDb·Metacritic·RT·Filmweb)
 * @param posterUrls ordered poster candidates — the primary then the cinema
 *                   fallbacks — walked until one decodes. The primary is often a
 *                   Multikino origin Cloudflare 403s from our Fly IP, so without
 *                   the fallbacks the slot renders empty.
 * @param dayLabel   the soonest showing day, e.g. "Sobota 20 czerwca"
 * @param showings   (cinema name, time chips like "12:15 DUB") for that day
 */
case class CityCardFilm(
  title:      String,
  meta:       Seq[String],
  badges:     Seq[OgCardRenderer.Badge],
  posterUrls: Seq[String],
  dayLabel:   String,
  showings:   Seq[(String, Seq[String])],
)

/**
 * Orchestrates the per-city Open Graph card: decode the posters of the city's
 * first few films, hand them to the pure [[OgCardRenderer.renderCityPageCard]],
 * and memoise the PNG by city. Fully dynamic — no committed image, no headless
 * browser. `posters` is injected so tests drive a fake and never hit the network.
 */
class CityOgCardService(posters: PosterFetch) {
  private val loader = new PosterImageLoader(posters)
  private val cache: Cache[String, Array[Byte]] =
    Caffeine.newBuilder().maximumSize(200).expireAfterWrite(12, TimeUnit.HOURS).build()

  /** Render (and memoise by `cacheKey`, the city slug, for 12h) the page-like
   *  card from the city's first distinct films. A render where no poster decoded
   *  isn't cached, so the next share retries instead of freezing a poster-less
   *  card. */
  def card(cacheKey: String, cityLine: String, films: Seq[CityCardFilm]): Array[Byte] =
    Option(cache.getIfPresent(cacheKey)).getOrElse {
      val columns = films.take(CityOgCardService.Columns)
        .map(f => f -> loader.loadFirst(f.posterUrls.take(CityOgCardService.MaxPosterCandidates)))
      val bytes = OgCardRenderer.renderCityPageCard(cityLine, columns)
      if (columns.exists(_._2.isDefined)) cache.put(cacheKey, bytes)
      bytes
    }
}

object CityOgCardService {
  /** Film columns in the page grid. */
  private val Columns = 5
  /** Poster candidates tried per film before falling back to an empty slot —
   *  caps a pathological all-dead chain (same intent as `OgCardService`). */
  private val MaxPosterCandidates = 6
}
