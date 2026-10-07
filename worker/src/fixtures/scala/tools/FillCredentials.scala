package tools

/**
 * The keys a convergence fill signs a listed gap with ([[MissingFixtureFill]]). A hermetic leg lists its gaps with
 * every credential masked ([[RedactedUrl]]), since the list is a public release asset; the fill, holding the keys,
 * puts each back — the masked parameter's value, and the headers its client sends beside it — so the request goes
 * out exactly as the client sent it. What it answers lands at the same fixture and verdict keys, which never hold a
 * credential (`RecordingHttpFetch.fixtureKey`, `LookupQuery.of`).
 *
 * By parameter name, each the one client that spells its key that way: `api_key` is TMDB's (`TmdbClient`, which also
 * sends it as a bearer header). OMDb's `apikey` is left out on purpose — its free key allows 1,000 requests a day,
 * which the workers spend — so its gaps stay the recorder's.
 */
final case class FillCredentials(tmdb: Option[settings.TmdbApiKey]) {
  private val byParameter: Map[String, (String, Map[String, String])] =
    tmdb.map(key => "api_key" -> (key.value, clients.TmdbClient.authorization(key))).toMap

  /** The URL to ask and the headers to send for a listed gap; None when it masks a credential this fill does not hold. */
  def sign(url: String): Option[(String, Map[String, String])] = url.indexOf('?') match {
    case -1 => Some(url -> Map.empty)
    case at =>
      val parameters = url.substring(at + 1).split("&", -1).toSeq.map { parameter =>
        parameter.split("=", 2) match {
          case Array(name, RedactedUrl.Mask) => byParameter.get(name.toLowerCase).map { case (value, headers) => s"$name=$value" -> headers }
          case _                             => Some(parameter -> Map.empty[String, String])
        }
      }
      Option.when(parameters.forall(_.isDefined))(
        s"${url.substring(0, at)}?${parameters.flatten.map(_._1).mkString("&")}" -> parameters.flatten.map(_._2).foldLeft(Map.empty[String, String])(_ ++ _))
  }
}

object FillCredentials {
  /** The keys the fill's process was given. */
  def from(configuration: settings.ProcessConfiguration): FillCredentials = FillCredentials(configuration.tmdbApiKey)
}
