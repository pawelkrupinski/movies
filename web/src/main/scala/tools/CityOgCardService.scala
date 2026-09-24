package tools

/**
 * Orchestrates the per-city Open Graph card: decode the posters of the city's
 * first few films, hand them to the pure [[OgCardRenderer.renderCityPageCard]],
 * and memoise the PNG by city. Fully dynamic — no committed image, no headless
 * browser. `posters` is injected so tests drive a fake and never hit the network.
 */
class CityOgCardService(posters: PosterFetch) {
  private val loader = new PosterImageLoader(posters)
  private val cache  = new OgCardCache(OgCard.MaxCacheBytes / 4)

  /** This service's card cache, for `kinowo_web_cache_*` — the city cards' own
   *  budget, held separately from the other card service's. */
  def cacheOccupancy: services.metrics.CacheOccupancy = cache.occupancy

  /** Render (and memoise by `cacheKey`, the city slug, for 12h) the page-like
   *  card from the city's first distinct films. A render where no poster decoded
   *  isn't cached, so the next share retries instead of freezing a poster-less
   *  card. */
  def card(cacheKey: String, cityLine: String, brand: String, host: String, films: Seq[CityCardFilm],
           filmweb: Boolean): Array[Byte] =
    cache.getOrRender(cacheKey) {
      val columns = films.take(CityOgCardService.Columns)
        .map(f => f -> loader.loadFirst(f.posterUrls.take(OgCard.MaxPosterCandidates)))
      val bytes = OgCardRenderer.renderCityPageCard(cityLine, brand, host, columns, filmweb)
      (bytes, columns.exists(_._2.isDefined))
    }
}

object CityOgCardService {
  /** Film columns in the page grid. */
  private val Columns = 5
}
