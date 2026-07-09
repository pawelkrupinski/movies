package services.enrichment

import org.jsoup.Jsoup
import tools.HttpFetch

import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Cross-references a film's external ids via Letterboxd. Every Letterboxd film
 * page carries BOTH ids we care about:
 *   - the TMDB id on the `<body data-tmdb-id="…" data-tmdb-type="movie">` tag, and
 *   - the IMDb id in the footer link `<a href="…imdb.com/title/tt…">`.
 *
 * Letterboxd exposes two redirect endpoints that resolve straight to a film
 * page from a single id — `/tmdb/{id}/` and `/imdb/{id}/` — so given whichever
 * id a film already has we can fetch its page and read the OTHER id off it.
 * This is an ID-RESOLUTION source (fills a missing tmdbId/imdbId so the existing
 * rating sources can fire), NOT a rating source: Letterboxd's own rating is
 * deliberately ignored.
 *
 * The value over TMDB's own `/movie/{id}/external_ids` is coverage of the
 * arthouse / festival long tail: Letterboxd's community catalogues obscure and
 * regional titles whose external-id links TMDB frequently leaves blank.
 */
class LetterboxdClient(http: HttpFetch) {
  import LetterboxdClient._

  /** Resolve the film page keyed by a known IMDb id (`tt…`) and read its ids. */
  def byImdbId(imdbId: String): Option[FilmIds] =
    fetchAndParse(s"$Site/imdb/${imdbId.trim}/")

  /** Resolve the film page keyed by a known TMDB (movie) id and read its ids. */
  def byTmdbId(tmdbId: Int): Option[FilmIds] =
    fetchAndParse(s"$Site/tmdb/$tmdbId/")

  private def fetchAndParse(url: String): Option[FilmIds] =
    Try(http.get(url)).toOption.map(parse)

  /** Pure parse of a Letterboxd film page. Returns whichever ids are present.
   *  A `data-tmdb-type` other than "movie" (Letterboxd also catalogues TV)
   *  suppresses the tmdbId so we never hand a TV id to the movie pipeline. */
  def parse(html: String): FilmIds = {
    val body = Jsoup.parse(html).body()
    val tmdbId =
      if (body.attr("data-tmdb-type") == "movie")
        Try(body.attr("data-tmdb-id").trim.toInt).toOption
      else None
    val imdbId = body.select("a[href*=/title/tt]").asScala.iterator
      .flatMap(a => ImdbIdPattern.findFirstIn(a.attr("href")))
      .nextOption()
    FilmIds(tmdbId, imdbId)
  }
}

object LetterboxdClient {
  private val Site         = "https://letterboxd.com"
  private val ImdbIdPattern = """tt\d+""".r

  /** The two external ids Letterboxd publishes on a film page. */
  case class FilmIds(tmdbId: Option[Int], imdbId: Option[String])
}
