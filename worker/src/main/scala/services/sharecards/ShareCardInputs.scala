package services.sharecards

import models.{Country, ResolvedMovie}
import tools.{Digest, OgCardRenderer, ShareCardText, SynopsisMarkdown}

/**
 * Everything one film's share card DRAWS — and therefore everything its content-addressed file
 * name hashes. Two cards with equal inputs are the same picture, so a card is rendered once per
 * distinct picture and a changed picture is a new file and a new URL (which is what makes the
 * `immutable` cache header on `/share-cards/...` true). One card per film: it is drawn in the
 * deployment's language, the one a crawler fetching `og:image` sees.
 *
 * THE HASH IS OF WHAT IS DRAWN, AT THE PRECISION IT IS DRAWN, so a change nobody could see never
 * re-renders: the rating badges' own text (IMDb and Filmweb to one decimal, RT as a whole
 * percentage, Metacritic as an integer), the title, year, genres, director line, synopsis, footer
 * host, language and template version — and, of the poster CANDIDATES, only the one the card was
 * drawn from. A film's fallback poster list churns thousands of times a day across the corpus
 * while its primary poster essentially never changes (measured 2026-09-24), so a card stays current
 * for as long as the poster it chose is still one of the film's candidates.
 *
 * The name is therefore `<film token>-<layout hash><ratings hash><poster hash>.jpg`: five hex
 * characters over everything drawn but the ratings and the poster, five over the rating badges, six
 * over the chosen poster URL — sixteen together, as the contract fixes. "Is there a card for these
 * inputs" is one existence check per candidate poster, and "did only the ratings move" is read off
 * the current card's name, which is what lets a ratings-only change wait (see [[ShareCardService]]).
 */
final case class ShareCardInputs(
  filmId:     String,
  lang:       String,
  title:      String,
  year:       Option[Int],
  genres:     Seq[String],
  posterUrls: Seq[String],
  imdb:       Option[Double],
  metascore:  Option[Int],
  rottenTomatoes: Option[Int],
  filmweb:    Option[Double],
  director:   Option[String],
  synopsis:   Option[String],
  host:       String,
  template:   Int = ShareCardInputs.TemplateVersion
) {
  /** The year · genres line under the title — the web's card subtitle. */
  def subtitle: String = (year.map(_.toString).toSeq ++ Seq(genres.mkString(", ")).filter(_.nonEmpty)).mkString(" · ")

  def badges: Seq[OgCardRenderer.Badge] = OgCardRenderer.ratingBadges(imdb, metascore, rottenTomatoes, filmweb)

  /** The rating badges as the card shows them. */
  private def ratingsText: String = badges.map(_.segs.map(_.text).mkString(" ")).mkString("\u0001")

  /** Every drawn part but the poster, as the card shows it. */
  private def drawn: Map[String, String] = layout + (ShareCardReason.Ratings -> ratingsText)

  /** Every drawn part but the poster and the ratings. */
  private def layout: Map[String, String] = Map(
    ShareCardReason.Title    -> title,
    ShareCardReason.Year     -> year.fold("")(_.toString),
    ShareCardReason.Language -> lang,
    ShareCardReason.Template -> template.toString,
    ShareCardReason.Details  -> Seq(subtitle, director.getOrElse(""), synopsis.getOrElse(""), host).mkString("\u0001")
  )

  /** Five hex characters of SHA-256 over the drawn parts but the ratings, field names included and
   *  NUL-separated, so no two input sets serialise alike. */
  def layoutHash: String =
    Digest.sha256Hex(layout.toSeq.sortBy(_._1).map { case (name, value) => s"$name=$value" }.mkString("\u0000")).take(5)

  /** Five hex characters of SHA-256 over the rating badges' text. */
  def ratingsHash: String = Digest.sha256Hex(ratingsText).take(5)

  /** Everything drawn but the poster: [[layoutHash]] then [[ratingsHash]]. */
  def drawnHash: String = layoutHash + ratingsHash

  /** The card's file name when drawn from `poster` (None: a film with no poster at all). */
  def fileName(poster: Option[String]): String =
    ShareCardFile(ShareCardFile.token(filmId), drawnHash + ShareCardFile.posterHash(poster)).name

  /** Every name a current card of these inputs could have — one per candidate poster, in the
   *  order the renderer tries them. */
  def candidateNames: Seq[String] =
    if (posterUrls.isEmpty) Seq(fileName(None)) else posterUrls.map(url => fileName(Some(url)))

  /** The inputs as a task payload, so the render task draws exactly what was asked for — on
   *  whichever replica claims it, and for a card the first-publish gate holds (which is in no
   *  `web_movies` document yet). [[ShareCardInputs.fromPayload]] reads it back. */
  def toPayload: Map[String, String] = Map(
    "filmId" -> filmId, "lang" -> lang, "title" -> title, "host" -> host, "template" -> template.toString,
    "genres" -> genres.mkString(ShareCardInputs.ListSeparator),
    "posterUrls" -> posterUrls.mkString(ShareCardInputs.ListSeparator)
  ) ++ Seq(
    // The ratings always carry their key ("" for none), so merging a later payload into a waiting
    // task (a deferred ratings render — see ShareCardService) also clears a rating that went away.
    "imdb" -> imdb.fold("")(_.toString), "metascore" -> metascore.fold("")(_.toString),
    "rottenTomatoes" -> rottenTomatoes.fold("")(_.toString), "filmweb" -> filmweb.fold("")(_.toString)
  ) ++ Seq("year" -> year.map(_.toString), "director" -> director, "synopsis" -> synopsis)
    .collect { case (key, Some(value)) => key -> value }

  /** A small per-part digest, kept instead of the inputs themselves (a synopsis is kilobytes) so a
   *  later render can say WHICH drawn parts moved. */
  def fingerprint: ShareCardFingerprint = ShareCardFingerprint(drawn.view.mapValues(_.##).toMap)
}

/** Per-part hashes of one card's drawn inputs — see [[ShareCardInputs.fingerprint]]. */
final case class ShareCardFingerprint(parts: Map[String, Int]) {
  /** The [[ShareCardReason]]s that differ from `previous`, in a stable order. */
  def changedFrom(previous: ShareCardFingerprint): Seq[String] =
    ShareCardReason.InputParts.filter(part => parts.get(part) != previous.parts.get(part))
}

object ShareCardInputs {
  /** Bump when the card's LOOK changes (layout, fonts, colours) so every card re-renders under a
   *  new name; the old files are pruned as superseded. */
  val TemplateVersion = 1

  /** The inputs of `movie`'s card for `country` — the same fields the web's card drew: the
   *  city-independent synopsis (a card is per film, not per city) without its markdown emphasis,
   *  and the director line with its label in the deployment's language. */
  def of(movie: ResolvedMovie, country: Country): ShareCardInputs = {
    val lang = ShareCardText.language(country)
    ShareCardInputs(
      filmId         = movie._id,
      lang           = lang,
      title          = movie.title,
      year           = movie.releaseYear,
      genres         = movie.genres,
      posterUrls     = (movie.posterUrl.toSeq ++ movie.fallbackPosterUrls).filter(_.nonEmpty).distinct.take(MaxPosterCandidates),
      imdb           = movie.ratings.imdb,
      metascore      = movie.ratings.metascore,
      rottenTomatoes = movie.ratings.rottenTomatoes,
      filmweb        = movie.ratings.filmweb,
      director       = Some(movie.directors.mkString(", ")).filter(_.nonEmpty).map(d => s"${ShareCardText.directorLabel(lang)}: $d"),
      synopsis       = movie.synopsis.map(SynopsisMarkdown.strip).map(_.trim).filter(_.nonEmpty),
      host           = country.shareHost
    )
  }

  private val ListSeparator = "\n"

  /** [[ShareCardInputs.toPayload]] read back; None for a payload missing a required field. */
  def fromPayload(p: Map[String, String]): Option[ShareCardInputs] = {
    def list(key: String) = p.get(key).filter(_.nonEmpty).fold(Seq.empty[String])(_.split(ListSeparator).toSeq)
    for {
      filmId   <- p.get("filmId")
      lang     <- p.get("lang")
      title    <- p.get("title")
      host     <- p.get("host")
      template <- p.get("template").flatMap(_.toIntOption)
    } yield ShareCardInputs(filmId, lang, title, p.get("year").flatMap(_.toIntOption), list("genres"), list("posterUrls"),
      p.get("imdb").flatMap(_.toDoubleOption), p.get("metascore").flatMap(_.toIntOption),
      p.get("rottenTomatoes").flatMap(_.toIntOption), p.get("filmweb").flatMap(_.toDoubleOption),
      p.get("director"), p.get("synopsis"), host, template)
  }

  /** How many poster candidates a card walks — the primary and up to five cinema fallbacks. */
  val MaxPosterCandidates = 6
}

/** Why a card was (re-)rendered — the `reason` label on `kinowo_worker_share_cards_render_total`.
 *  A re-render names every input part that moved, one increment each. */
object ShareCardReason {
  val NewFilm  = "new_film"
  val Backfill = "backfill"
  /** The poster the card was drawn from is no longer one of the film's candidates. */
  val Poster   = "poster"
  val Title    = "title"
  val Year     = "year"
  val Ratings  = "ratings"
  val Language = "language"
  val Template = "template"
  /** Genres, director, synopsis or footer host — drawn, but not worth a label each. */
  val Details  = "details"

  val InputParts: Seq[String] = Seq(Title, Year, Ratings, Language, Template, Details)
  val all: Seq[String]        = Seq(NewFilm, Backfill, Poster) ++ InputParts
}

/** A card file name, `<token>-<hash>.jpg`: [[ShareCardInputs.drawnHash]] then the poster's. */
final case class ShareCardFile(token: String, hash: String) {
  def name: String = s"$token-$hash.jpg"
  def drawnHash: String   = hash.take(10)
  def layoutHash: String  = hash.take(5)
  def ratingsHash: String = hash.slice(5, 10)
  def posterHash: String  = hash.drop(10)
}

object ShareCardFile {
  private val Pattern = """([a-z0-9]+)-([0-9a-f]{16})\.jpg""".r

  def parse(name: String): Option[ShareCardFile] = name match {
    case Pattern(token, hash) => Some(ShareCardFile(token, hash))
    case _                    => None
  }

  /** Six hex characters of SHA-256 over the poster URL the card was drawn from ("" for none). */
  def posterHash(poster: Option[String]): String = Digest.sha256Hex(poster.getOrElse("")).take(6)

  /** A film id as it may appear in a file name and a URL path: the id itself when it is already
   *  plain lower-case alphanumerics (every `FilmId.fresh` id — `f` + hex), else `h` + a digest of
   *  it (a legacy `title|year` id, or a `~variant` card id), which keeps `-` out so the name
   *  parses back unambiguously. */
  def token(filmId: String): String =
    if (filmId.nonEmpty && filmId.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))) filmId
    else "h" + Digest.sha256Hex(filmId).take(20)
}
