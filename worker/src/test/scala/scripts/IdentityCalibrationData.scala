package scripts

import clients.tools.RecordingHttpFetch
import models.{Cinema, CinemaMovie, Country}
import play.api.libs.json.{JsObject, JsValue, Json}
import services.identity.IdentityMeasures
import services.identity.IdentityMeasures.{Category, Film, Listing, Measure, Number}
import services.movies.{ListingKey, ScrapeListing, TitleNormalizer}
import tools.CorpusFixture

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * The calibration's INPUT: every recorded listing, the TMDB answers the recorded trees hold for
 * it, and production's filing of it (a proposal, never a label). See [[IdentityCalibrate]].
 */
object IdentityCalibrationData {

  /** One listing as its venue published it. `prodFilm` is the production film it is filed under. */
  final case class Obs(idx: Int, country: String, venue: String, listingKey: String, listing: Listing,
                       filmUrl: Option[String], chainIds: Map[String, String], source: String,
                       prodFilm: Option[String])

  /** A TMDB search result. */
  final case class Hit(id: Int, title: String, originalTitle: Option[String], year: Option[Int], popularity: Double)

  /** TMDB's own record of a film, from the recorded details answers. */
  final case class Details(id: Int, film: Film, imdbId: Option[String])

  /** Production's decision for one film: never ground truth on its own. */
  final case class ProdFilm(id: String, tmdbId: Option[Int], imdbId: Option[String], ratingUrls: Seq[String])

  private def readGz(path: Path): String = {
    val in = new GZIPInputStream(Files.newInputStream(path))
    try new String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
  }

  private def yearOf(date: Option[String]): Option[Int] =
    date.filter(_.length >= 4).flatMap(d => Try(d.take(4).toInt).toOption)

  // ── recorded TMDB answers ─────────────────────────────────────────────────────────────

  /**
   * The TMDB answers of one country: a recorded fixture tree (`enrichment-<cc>/`, as
   * `RecordingHttpFetch` wrote it) and/or a hard-cluster responses file (URL → body). Looked up
   * by `RecordingHttpFetch.fixtureKey`, the one spelling of a fixture's name, so the search a
   * listing's title issues is found exactly where the recorder put it.
   */
  final class TmdbAnswers(trees: Seq[Path], responses: Map[String, String], language: String) {
    private val movieDirFiles: Map[Int, Seq[String]] = {
      val fromTrees = trees.flatMap { t =>
        val dir = t.resolve("api.themoviedb.org/3/movie")
        if (!Files.isDirectory(dir)) Nil
        else Files.list(dir).iterator().asScala.toSeq.flatMap { p =>
          val name = p.getFileName.toString
          val id = Try(name.takeWhile(_ != '.').toInt).toOption
          id.toSeq.flatMap { i =>
            if (Files.isDirectory(p)) Files.list(p).iterator().asScala.map(q => i -> q.toString).toSeq
            else Seq(i -> p.toString)
          }
        }
      }
      val fromResponses = responses.keys.toSeq.flatMap { k =>
        val rest = k.stripPrefix("api.themoviedb.org/3/movie/")
        if (rest == k) Nil else Try(rest.takeWhile(c => c != '.' && c != '/').toInt).toOption.map(_ -> s"response:$k").toSeq
      }
      (fromTrees ++ fromResponses).groupMap(_._1)(_._2)
    }

    private def body(ref: String): Option[String] =
      if (ref.startsWith("response:")) responses.get(ref.stripPrefix("response:"))
      else Try(new String(Files.readAllBytes(Path.of(ref)), StandardCharsets.UTF_8)).toOption

    private def fixture(key: String): Option[String] =
      responses.get(key).orElse(trees.iterator.map(_.resolve(key)).find(Files.isRegularFile(_))
        .flatMap(p => Try(new String(Files.readAllBytes(p), StandardCharsets.UTF_8)).toOption))

    /** Every film the recorded answers know, by each of its titles' keys: the candidates a
     *  family's title names even where no search for that exact spelling was recorded. */
    lazy val byTitleKey: Map[String, Seq[Int]] =
      movieDirFiles.keys.toSeq.flatMap(id => details(id).toSeq.flatMap { d =>
        (Seq(d.film.title) ++ d.film.originalTitle ++ d.film.alternativeTitles).map(IdentityMeasures.key).filter(_.nonEmpty).distinct.map(_ -> id)
      }).groupMap(_._1)(_._2).view.mapValues(_.distinct.sorted).toMap

    private val searchCache  = mutable.HashMap.empty[String, Option[Seq[Hit]]]
    private val detailsCache = mutable.HashMap.empty[Int, Option[Details]]

    /** The recorded answer to `TmdbClient`'s yearless title search, `None` when not recorded. */
    def search(query: String): Option[Seq[Hit]] = searchCache.getOrElseUpdate(query, {
      val url = s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=" +
        URLEncoder.encode(query, StandardCharsets.UTF_8)
      fixture(RecordingHttpFetch.fixtureKey(url)).flatMap(b => Try(Json.parse(b)).toOption).map { js =>
        (js \ "results").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap { r =>
          (r \ "id").asOpt[Int].map(id => Hit(id, (r \ "title").asOpt[String].getOrElse(""),
            (r \ "original_title").asOpt[String], yearOf((r \ "release_date").asOpt[String]),
            (r \ "popularity").asOpt[Double].getOrElse(0.0)))
        }
      }
    })

    /** TMDB's record of `id`, merged from every recorded answer about it. */
    def details(id: Int): Option[Details] = detailsCache.getOrElseUpdate(id, {
      val docs = movieDirFiles.getOrElse(id, Nil).sorted.flatMap(body).flatMap(b => Try(Json.parse(b)).toOption)
        .map(js => (js \ "text").asOpt[String].flatMap(t => Try(Json.parse(t)).toOption).getOrElse(js))
      val main = docs.filter(d => (d \ "title").isDefined)
      if (main.isEmpty) None
      else {
        // The deployment-language answer carries `credits`; the en-US one `alternative_titles`.
        val localized = main.find(d => (d \ "credits").isDefined).getOrElse(main.head)
        val crew = docs.flatMap(d => (d \ "crew").asOpt[Seq[JsValue]].orElse((d \ "credits" \ "crew").asOpt[Seq[JsValue]]).getOrElse(Nil))
        val directors = crew.filter(c => (c \ "job").asOpt[String].contains("Director")).flatMap(c => (c \ "name").asOpt[String]).distinct
        val hasCrew = docs.exists(d => (d \ "crew").isDefined || (d \ "credits" \ "crew").isDefined)
        val alternatives = main.flatMap(d => (d \ "alternative_titles" \ "titles").asOpt[Seq[JsValue]].getOrElse(Nil))
          .flatMap(t => (t \ "title").asOpt[String]) ++ main.flatMap(d => (d \ "title").asOpt[String])
        val title = (localized \ "title").as[String]
        val countries = main.flatMap(d => (d \ "production_countries").asOpt[Seq[JsValue]].getOrElse(Nil)
          .flatMap(c => (c \ "iso_3166_1").asOpt[String]) ++ (d \ "origin_country").asOpt[Seq[String]].getOrElse(Nil)).distinct
        Some(Details(id, Film(
          title             = title,
          originalTitle     = (localized \ "original_title").asOpt[String],
          alternativeTitles = alternatives.distinct.filterNot(_ == title),
          year              = yearOf((localized \ "release_date").asOpt[String]),
          runtime           = main.flatMap(d => (d \ "runtime").asOpt[Int]).find(_ > 0),
          directors         = Option.when(hasCrew)(directors),
          countries         = Option.when(countries.nonEmpty)(countries),
          popularity        = main.flatMap(d => (d \ "popularity").asOpt[Double]).headOption),
          main.flatMap(d => (d \ "imdb_id").asOpt[String]).find(_.nonEmpty)))
      }
    })
  }

  /** A hard-cluster responses file (`"GET <url>" → {"text": body}`) keyed by fixture name. */
  def responsesFile(path: Path): Map[String, String] =
    if (!Files.exists(path)) Map.empty
    else Json.parse(readGz(path)).as[JsObject].fields.toMap.filter { case (k, _) => k.startsWith("GET https://api.themoviedb.org/") }.flatMap { case (k, v) =>
      val url = k.stripPrefix("GET ").trim
      (v \ "text").asOpt[String].orElse(v.asOpt[String]).map(RecordingHttpFetch.fixtureKey(url) -> _)
    }

  // ── production's filing ──────────────────────────────────────────────────────────────

  final case class ProdSnapshot(films: Map[String, ProdFilm], slots: Seq[(String, String, JsObject)])

  /** `prod-<db>.jsonl` as `scripts/identity-calibrate/extract-prod.js` wrote it. */
  def prodSnapshot(path: Path): ProdSnapshot =
    if (!Files.exists(path)) ProdSnapshot(Map.empty, Nil)
    else {
      val films = Map.newBuilder[String, ProdFilm]
      val slots = Seq.newBuilder[(String, String, JsObject)]
      Files.lines(path).iterator().asScala.filter(_.startsWith("{")).foreach { line =>
        val js = Json.parse(line)
        (js \ "kind").as[String] match {
          case "film" =>
            val id = (js \ "id").as[String]
            films += id -> ProdFilm(id, (js \ "tmdbId").asOpt[Int], (js \ "imdbId").asOpt[String],
              Seq("metacriticUrl", "rottenTomatoesUrl").flatMap(k => (js \ k).asOpt[String]))
          case _ =>
            slots += (((js \ "filmId").as[String], (js \ "slotKey").as[String], (js \ "slot").asOpt[JsObject].getOrElse(Json.obj())))
        }
      }
      ProdSnapshot(films.result(), slots.result())
    }

  // ── listings ─────────────────────────────────────────────────────────────────────────

  private val Separator = "␟"

  private def listingOf(cm: CinemaMovie): Listing =
    Listing(cm.movie.title, cm.movie.rawTitle, cm.movie.originalTitle.filter(_.trim.nonEmpty), cm.movie.releaseYear,
      cm.movie.runtimeMinutes.filter(_ > 0), cm.director.map(_.trim).filter(_.nonEmpty).distinct, cm.movie.countries)

  /**
   * Every listing of a country: the recorded corpora (full and hard-cluster), then every
   * production slot no corpus listing is filed under (production's current programme), each
   * joined to the production film it is filed under.
   */
  def listings(country: Country, corpora: Seq[(String, Path)], prod: ProdSnapshot, startIdx: Int): Seq[Obs] = {
    val normalizer = TitleNormalizer.forCountry(country)
    val bySlotKey  = prod.slots.map { case (film, slotKey, _) => slotKey -> film }.toMap
    val byRaw      = prod.slots.flatMap { case (film, slotKey, slot) =>
      val venue = slotKey.takeWhile(_ != Separator.head)
      Seq((slot \ "rawTitle").asOpt[String], (slot \ "title").asOpt[String]).flatten.map(t => (venue, t) -> film)
    }.toMap
    val seen   = mutable.HashSet.empty[String]
    val used   = mutable.HashSet.empty[String]
    val out    = Seq.newBuilder[Obs]
    var idx    = startIdx
    corpora.foreach { case (source, path) =>
      CorpusFixture.readFrom(path).foreach { row =>
        val cinema: Cinema = row.cinema
        row.films.foreach { cm =>
          val lk = ListingKey.of(cinema, cm).toString
          if (seen.add(lk)) {
            val raw  = cm.movie.rawTitle.getOrElse(cm.movie.title)
            val slot = s"${cinema.displayName}$Separator${ScrapeListing.slotKey(cinema, cm.movie.title, normalizer)}"
            val film = bySlotKey.get(slot).orElse(byRaw.get((cinema.displayName, raw))).orElse(byRaw.get((cinema.displayName, cm.movie.title)))
            if (film.isDefined) used += slot
            out += Obs(idx, country.code, cinema.displayName, lk, listingOf(cm), cm.filmUrl.filter(_.trim.nonEmpty),
              cm.externalIds, source, film)
            idx += 1
          }
        }
      }
    }
    prod.slots.foreach { case (film, slotKey, slot) =>
      if (!used.contains(slotKey)) {
        val venue = slotKey.takeWhile(_ != Separator.head)
        val title = (slot \ "title").asOpt[String].getOrElse("")
        val raw   = (slot \ "rawTitle").asOpt[String]
        val year  = (slot \ "releaseYear").asOpt[Int]
        val dirs  = (slot \ "director").asOpt[Seq[String]].getOrElse(Nil).map(_.trim).filter(_.nonEmpty).distinct
        val url   = (slot \ "filmUrl").asOpt[String].filter(_.trim.nonEmpty)
        val lk    = url.fold[ListingKey](ListingKey.Published(venue, raw.getOrElse(title), year, dirs.sorted))(
          u => ListingKey.Native(venue, u, raw.getOrElse(title))).toString
        if (title.nonEmpty && seen.add(lk)) {
          out += Obs(idx, country.code, venue, lk,
            Listing(title, raw, (slot \ "originalTitle").asOpt[String].filter(_.trim.nonEmpty), year,
              (slot \ "runtimeMinutes").asOpt[Int].filter(_ > 0), dirs, (slot \ "countries").asOpt[Seq[String]].getOrElse(Nil)),
            url, Map.empty, "prod", Some(film))
          idx += 1
        }
      }
    }
    out.result()
  }

  /** The queries a listing's own title issues: every title shape and its original title. */
  def queries(l: Listing): Seq[String] = (IdentityMeasures.titleShapes(l) ++ l.originalTitle).distinct

  /** Do the two listings' chains publish an id in a common namespace, and is it the same? */
  def sharedChainId(a: Obs, b: Obs): Option[Boolean] = {
    val common = a.chainIds.keySet intersect b.chainIds.keySet
    Option.when(common.nonEmpty)(common.exists(k => a.chainIds(k) == b.chainIds(k)))
  }

  /** Read a measure as the label code reads it. */
  def category(m: Option[Measure]): Option[String] = m.collect { case Category(v) => v }
  def number(m: Option[Measure]): Option[Double]   = m.collect { case Number(x) => x }

  def languageOf(country: Country): String = country.language.toLanguageTag
}
