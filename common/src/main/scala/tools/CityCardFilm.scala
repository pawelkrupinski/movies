package tools

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
