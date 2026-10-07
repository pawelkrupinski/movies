package services.cinemas.common

import org.jsoup.nodes.Document
import org.jsoup.parser.Parser
import play.api.libs.json.{JsArray, JsLookupResult, JsObject, JsString, Json}

import java.time.LocalDate
import scala.jdk.CollectionConverters._

/**
 * A Flicks film page — `<market base>/movie/<slug>/`, the page every Flicks listing's `filmUrl` names — read
 * for the facts the day fragment lacks: the release year above all, then every director (the fragment's card
 * credits the first only), countries, the whole cast, synopsis and poster.
 *
 * They come from the page's schema.org `Movie` block (`<script type="application/ld+json">`): `dateCreated`
 * (the year), `director[]`, `actor[]`, `countryOfOrigin` ("Mexico, Spain, USA"), `genre[]`, `description`,
 * `duration` ("T1H36M"). The hero line ("1935 • 96mins") states the same year and runtime and stands in where
 * the block is missing. The poster is the hero's own (`.movie-hero-v6__poster img`): the page's og:image is
 * the film's BACKDROP, so it is taken only where [[ScraperParse.ogImage]] admits it and it is a poster.
 *
 * Measured 2026-10-06 on ten pages across both markets: no page states an original title ("Spirited Away",
 * "Stalker (1979)", "Pan's Labyrinth" and "The Broken Circle Breakdown" carry their English title only), so
 * none is read. A re-release gets its own page dated by its re-run — see [[ReReleaseBilling]].
 */
object FlicksFilmPage {

  def parse(html: String, slug: String, today: LocalDate): FilmDetail =
    parseDocument(Parser.htmlParser().parseInput(slimmedReader(html), ""), slug, today)

  /** What [[parse]] reads off a parsed page — the whole page or its [[slimmed]] parts alike. */
  private[common] def parseDocument(document: Document, slug: String, today: LocalDate): FilmDetail = {
    val movie    = movieBlock(document)
    def text(field: String): Option[String] = movie.flatMap(m => (m \ field).asOpt[String]).map(_.trim).filter(_.nonEmpty)
    def names(field: String): Seq[String]   = movie.fold(Seq.empty[String])(m => listOf(m \ field)).distinct
    val heroMeta = document.select(".movie-hero-v6__meta > span").asScala.map(_.text.trim).toSeq
    val title    = text("name").orElse(Option(document.selectFirst(".movie-hero-v6__title h1")).map(_.text.trim)).getOrElse("")
    val year     = text("dateCreated").flatMap(YearPattern.findFirstIn).orElse(heroMeta.find(YearPattern.matches))
      .flatMap(_.toIntOption)
    FilmDetail(
      synopsis       = text("description"),
      cast           = names("actor"),
      director       = names("director"),
      runtimeMinutes = heroMeta.flatMap(MinutesPattern.findFirstMatchIn).headOption.map(_.group(1).toInt)
        .orElse(text("duration").flatMap(isoMinutes)).filter(_ > 0),
      releaseYear    = year.flatMap(ReReleaseBilling.filmYear(_, s"$title $slug", today)),
      countries      = text("countryOfOrigin").toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty),
      genres         = names("genre"),
      posterUrl      = heroPoster(document).orElse(ScraperParse.ogImage(document).filter(_.contains(PosterPath))),
      trailerUrl     = movie.flatMap(m => (m \ "trailer" \ "url").asOpt[String]).map(_.trim).filter(_.nonEmpty)
    )
  }

  /** `html` as the parts [[parse]] reads — the head (og:image), the hero (title, year and runtime line, poster) and
   *  every schema.org block after it — without the showtimes tabs between, so the parser does not tokenise them. A page
   *  is ~350 KB and the tabs nearly all of it; whole-page parsing was 42% of a US detail drain's CPU (JFR, run
   *  37619819275). Cut where the tabs open (`id="movie-tabs"`); a page without that mark is read whole. */
  private[common] def slimmedReader(html: String): java.io.Reader = new RangesReader(html, keptRanges(html))

  /** [[slimmedReader]]'s text as a string — for the spec. */
  private[common] def slimmed(html: String): String = {
    val out = new java.io.StringWriter(html.length); slimmedReader(html).transferTo(out); out.toString
  }

  private def keptRanges(html: String): Array[Int] = html.indexOf(TabsMark) match {
    case -1   => Array(0, html.length)
    case tabs =>
      val cut     = html.lastIndexOf('<', tabs)
      val scripts = Iterator.iterate(html.indexOf(SchemaType, cut))(at => html.indexOf(SchemaType, at + 1))
        .takeWhile(_ >= 0)
        .map(at => (html.lastIndexOf("<script", at), html.indexOf("</script>", at)))
        .collect { case (open, close) if open >= cut && close >= 0 => Array(open, close + "</script>".length) }
        .flatten.toArray
      Array(0, cut) ++ scripts
  }

  private val TabsMark   = "id=\"movie-tabs\""
  private val SchemaType = "application/ld+json"

  /** The page's schema.org `Movie` block. A block that is not JSON throws: that is not a page we can read. */
  private def movieBlock(document: Document): Option[JsObject] =
    document.select("script[type=application/ld+json]").asScala.iterator
      .map(script => Json.parse(script.data))
      .collectFirst { case block: JsObject if (block \ "@type").asOpt[String].contains("Movie") => block }

  /** A schema.org list field, which may be one string or an array of them. */
  private def listOf(value: JsLookupResult): Seq[String] = value.toOption match {
    case Some(JsArray(items)) => items.toSeq.collect { case JsString(s) => s.trim }.filter(_.nonEmpty)
    case Some(JsString(s))    => Seq(s.trim).filter(_.nonEmpty)
    case _                    => Nil
  }

  private def heroPoster(document: Document): Option[String] =
    Option(document.selectFirst(".movie-hero-v6__poster img")).map(_.attr("src").trim)
      .filter(src => src.nonEmpty && !src.contains(PlaceholderPoster))

  /** "T2H33M" → 153. */
  private def isoMinutes(duration: String): Option[Int] =
    IsoDuration.findFirstMatchIn(duration).map(m => Option(m.group(1)).fold(0)(_.toInt) * 60 + Option(m.group(2)).fold(0)(_.toInt))

  private val YearPattern       = """\d{4}""".r
  private val MinutesPattern    = """^(\d+)\s*mins?$""".r
  private val IsoDuration       = """T(?:(\d+)H)?(?:(\d+)M)?""".r
  private val PosterPath        = "/images/movies/poster"
  private val PlaceholderPoster = FlicksClient.PlaceholderPoster
}
