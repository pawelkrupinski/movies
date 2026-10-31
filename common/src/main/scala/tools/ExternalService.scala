package tools

/**
 * The external service an outbound request reaches, as a stable metric label value — the
 * `service` label of `kinowo_worker_http_total`, and the names CI's recording legs report their
 * per-service call counts under. A closed set, so every series grid built from it stays bounded.
 *
 * Named from the request URL's HOST, because that is the one thing every client has in common at
 * the innermost counter: the clients share one fetch chain per phase, so the host is what tells
 * an OMDb 401 ("Request limit reached!") from a TMDB one.
 */
object ExternalService {
  val Omdb           = "omdb"
  val Tmdb           = "tmdb"
  val Imdb           = "imdb"
  val RottenTomatoes = "rottentomatoes"
  val Metacritic     = "metacritic"
  val Filmweb        = "filmweb"
  val Letterboxd     = "letterboxd"
  val Wikidata       = "wikidata"
  val Cinemeta       = "cinemeta"
  /** The alerting bot's API — reached from the cinema-site chain, but no cinema. */
  val Telegram       = "telegram"
  /** Any cinema site (listing, chunk and per-film detail scrapes). Not named from a host: the
   *  set of cinema hosts is large and changes with the roster, so it is one value. */
  val Cinema         = "cinema"
  /** A host none of the above names. */
  val Other          = "other"

  /** The third-party metadata / rating / resolution APIs. */
  val apis: Seq[String] = Seq(Omdb, Tmdb, Imdb, RottenTomatoes, Metacritic, Filmweb, Letterboxd, Wikidata, Cinemeta)

  /** Each service by the registrable domains its clients call (a host matches a domain exactly or
   *  as a subdomain of it, so `notimdb.com` is not IMDb). */
  private val byDomain: Seq[(String, String)] = Seq(
    "omdbapi.com"        -> Omdb,
    "themoviedb.org"     -> Tmdb,
    "tmdb.org"           -> Tmdb,
    "imdb.com"           -> Imdb,
    "media-imdb.com"     -> Imdb,
    "rottentomatoes.com" -> RottenTomatoes,
    "metacritic.com"     -> Metacritic,
    "filmweb.pl"         -> Filmweb,
    "letterboxd.com"     -> Letterboxd,
    "boxd.it"            -> Letterboxd,
    "wikidata.org"       -> Wikidata,
    "strem.io"           -> Cinemeta,
    "telegram.org"       -> Telegram)

  /** The named service `url`'s host belongs to, if any. */
  def ofUrl(url: String): Option[String] =
    UrlHost.of(url).flatMap(host => byDomain.collectFirst { case (domain, service) if host == domain || host.endsWith("." + domain) => service })
}
