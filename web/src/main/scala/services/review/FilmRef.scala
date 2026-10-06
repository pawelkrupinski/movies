package services.review

import java.net.URI
import java.util.Locale
import scala.util.Try

/**
 * A film named in another database, in the `source:id` form `labels.tsv` files its `film` column
 * in (`tmdb:291289`, `imdb:tt4020156`, `filmweb:10008278`, `wikidata:Q101245192`,
 * `rt:1018413-scrooge`, `letterboxd:13-souls`, `metacritic:a-prayer-for-the-dying`).
 */
final case class FilmRef(source: String, id: String) {
  def render: String = s"$source:$id"

  /** The film's page on its own site, where one can be built from the id alone. */
  def url: Option[String] = source match {
    case "tmdb"       => Some(s"https://www.themoviedb.org/movie/$id")
    case "imdb"       => Some(s"https://www.imdb.com/title/$id/")
    case "filmweb"    => Some(s"https://www.filmweb.pl/film/x-0-$id")
    case "wikidata"   => Some(s"https://www.wikidata.org/wiki/$id")
    case "rt"         => Some(s"https://www.rottentomatoes.com/m/$id")
    case "letterboxd" => Some(s"https://letterboxd.com/film/$id/")
    case "metacritic" => Some(s"https://www.metacritic.com/movie/$id/")
    case _            => None
  }

  def tmdb: Option[Int] = Option.when(source == "tmdb")(id.toIntOption).flatten
}

object FilmRef {
  def tmdb(id: Int): FilmRef = FilmRef("tmdb", id.toString)

  val Sources: Set[String] = Set("tmdb", "imdb", "filmweb", "wikidata", "rt", "letterboxd", "metacritic")

  private val ImdbId     = "(tt\\d{5,})".r
  private val WikidataId = "(Q\\d+)".r
  private val Digits     = "(\\d+)".r

  /**
   * What the reviewer pasted, as a ref: a link to a film's page on TMDB, IMDb, Filmweb, Wikidata,
   * Rotten Tomatoes, Letterboxd or Metacritic, an already-formed `source:id`, or a bare IMDb
   * (`tt…`), Wikidata (`Q…`) or TMDB (digits) id. `None` for anything else — a TV page, a search
   * page, a person — rather than a guess.
   */
  def parse(input: String): Option[FilmRef] = {
    val text = input.trim
    text match {
      case ""              => None
      case ImdbId(id)      => Some(FilmRef("imdb", id))
      case WikidataId(id)  => Some(FilmRef("wikidata", id))
      case Digits(id)      => Some(FilmRef("tmdb", id.toInt.toString))
      case _ if text.contains("://") || text.matches("^(www\\.)?[a-z0-9.-]+\\.[a-z]{2,}/.*") => fromUrl(text)
      case _               => formed(text)
    }
  }

  /** `source:id` as `labels.tsv` spells it. */
  def formed(text: String): Option[FilmRef] = text.split(":", 2) match {
    case Array(source, id) if Sources(source.toLowerCase(Locale.ROOT)) && id.trim.nonEmpty =>
      val s = source.toLowerCase(Locale.ROOT)
      val i = id.trim
      s match {
        case "tmdb"     => i.toIntOption.map(n => FilmRef(s, n.toString))
        case "imdb"     => Option.when(ImdbId.matches(i))(FilmRef(s, i))
        case "wikidata" => Option.when(WikidataId.matches(i))(FilmRef(s, i))
        case "filmweb"  => i.toLongOption.map(n => FilmRef(s, n.toString))
        case _          => Some(FilmRef(s, i))
      }
    case _ => None
  }

  private def fromUrl(text: String): Option[FilmRef] = {
    val withScheme = if (text.contains("://")) text else "https://" + text
    Try(new URI(withScheme.replace(" ", "%20"))).toOption.flatMap { uri =>
      val host = Option(uri.getHost).getOrElse("").toLowerCase(Locale.ROOT).stripPrefix("www.").stripPrefix("m.")
      val segments = Option(uri.getRawPath).getOrElse("").split("/").filter(_.nonEmpty).toList
      def after(marker: String): Option[String] = segments.dropWhile(_ != marker).drop(1).headOption
      host match {
        case h if h.endsWith("themoviedb.org") =>
          after("movie").flatMap(slug => "^(\\d+)".r.findFirstMatchIn(slug)).map(m => FilmRef("tmdb", m.group(1).toInt.toString))
        case h if h.endsWith("imdb.com") =>
          after("title").filter(ImdbId.matches).map(FilmRef("imdb", _))
        case h if h.endsWith("filmweb.pl") =>
          // /film/Franz+Kafka-2025-10008278 — the id is the slug's last dash-separated number
          after("film").flatMap(slug => "-(\\d+)$".r.findFirstMatchIn(slug)).map(m => FilmRef("filmweb", m.group(1)))
        case h if h.endsWith("wikidata.org") =>
          after("wiki").orElse(after("entity")).filter(WikidataId.matches).map(FilmRef("wikidata", _))
        case h if h.endsWith("rottentomatoes.com") =>
          after("m").map(FilmRef("rt", _))
        case h if h.endsWith("letterboxd.com") =>
          after("film").map(FilmRef("letterboxd", _))
        case h if h.endsWith("metacritic.com") =>
          after("movie").map(FilmRef("metacritic", _))
        case _ => None
      }
    }
  }
}
