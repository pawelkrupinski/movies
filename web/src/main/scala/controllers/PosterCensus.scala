package controllers

/** The country's served films — distinct across its cities — by the registrable domain their
 *  poster is drawn from ([[tools.PosterHost]]), `none` for a film served with no poster (its card
 *  renders the "no poster" placeholder), and per host the films whose poster image is shared with
 *  [[SharedPosterMin]] − 1 or more others.
 *
 *  WHY, AT THE WEB AND NOT THE WORKER: this is what the pages hand a browser, after projection and
 *  the read model's poster ordering — the layer the 2026-10-21 kinoheld outage sat right next to.
 *  It cannot see whether an image LOADS (that is the synthetic poster probe's job,
 *  synthetic-probes.nix), only whose image every card asks for. posters.rules alerts on a host's
 *  share collapsing, on the placeholder share jumping, and on a burst of shared images. */
final case class PosterCensus(byHost: Map[String, Int], sharing: Map[String, Int]) {
  def render(country: String): String = {
    val sb = new StringBuilder
    sb.append("# HELP ").append(PosterCensus.PosterHostName)
      .append(" The country's served films (distinct across its cities) by the registrable domain of the poster their card draws, `none` for no poster (the card's placeholder), `other` for hosts under the floor.\n")
    sb.append("# TYPE ").append(PosterCensus.PosterHostName).append(" gauge\n")
    byHost.toSeq.sortBy(_._1).foreach { case (h, n) => line(sb, PosterCensus.PosterHostName, country, h, n) }
    sb.append("# HELP ").append(PosterCensus.SharedPosterName)
      .append(" The country's served films whose poster is one image three or more films share (a placeholder or a site-wide image, not a poster), by the image's registrable domain.\n")
    sb.append("# TYPE ").append(PosterCensus.SharedPosterName).append(" gauge\n")
    sharing.toSeq.sortBy(_._1).foreach { case (h, n) => line(sb, PosterCensus.SharedPosterName, country, h, n) }
    sb.toString
  }

  private def line(sb: StringBuilder, name: String, country: String, host: String, n: Int): Unit =
    sb.append(name).append("{country=\"").append(country).append("\",poster_host=\"").append(host)
      .append("\"} ").append(n).append('\n')
}

object PosterCensus {
  /** Three or more films sharing ONE poster image is a placeholder, not a poster: a site-wide
   *  og:image (Kinoteka, 2026-10-06), a "coming soon" card (Multikino `wkrotce`), a not-found image
   *  (bilety24), kinoheld's `fallback-light.jpg`. Two is still a real pairing (a film and its
   *  dubbed screening under separate cards). */
  val SharedPosterMin = 3

  private[controllers] val PosterHostName = "kinowo_web_films_by_poster_host"
  private[controllers] val SharedPosterName = "kinowo_web_films_sharing_poster"

  val NoPoster = "none"
  val Other    = "other"

  def of(schedules: Seq[FilmSchedule], floor: Int): PosterCensus = {
    // One poster per film: a film listed in five cities is one of the country's films.
    val posterByFilm: Map[String, Option[String]] =
      schedules.map(s => s.slug.getOrElse(s.movie.title) -> s.posterHref.map(_.url)).toMap
    val hostByFilm = posterByFilm.view.mapValues(_.flatMap(tools.PosterHost.family)).toMap
    val counts     = hostByFilm.values.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    def label(host: String): String = if (counts.getOrElse(host, 0) >= floor) host else Other
    val byHost = hostByFilm.values.groupMapReduce(_.fold(NoPoster)(label))(_ => 1)(_ + _)
    val filmsPerImage = posterByFilm.values.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    val sharing = posterByFilm.toSeq.collect {
      case (film, Some(url)) if filmsPerImage(url) >= SharedPosterMin => hostByFilm(film)
    }.flatten.groupMapReduce(label)(_ => 1)(_ + _)
    PosterCensus(byHost, sharing)
  }
}
