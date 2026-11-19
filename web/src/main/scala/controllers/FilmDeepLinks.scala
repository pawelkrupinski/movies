package controllers

import models.{City, ResolvedMovie}
import services.readmodel.WebReadModel

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

/** A film page's address resolved to its film's schedule in a city ([[FilmPageController]]): by title (`?title=`,
 *  re-decoded when a chat app encoded it twice) or by the canonical `/{city}/movie/{slug}`, the schedules
 *  [[MovieControllerService]]'s; a film the read model knows with nothing live here rendered with no showings
 *  rather than a 404 -- and the same film's page in each other city showing it ([[otherCities]]). */
final class FilmDeepLinks(readModel: WebReadModel, schedules: MovieControllerService) {
  private val logger = play.api.Logger(classOf[FilmDeepLinks])

  def film(city: City, title: String): Option[FilmSchedule] = {
    // Matched through the read model's title index rather than by folding every
    // schedule's title per request; among the films the index names, the first
    // in schedule order (earliest showtime) wins, as it always has — and only
    // those films are joined.
    def lookup(t: String): Option[FilmSchedule] = {
      val ids = readModel.filmTitles.idsFor(t).toSet
      if (ids.isEmpty) None else schedules.schedulesFor(city, ids).headOption
    }
    // Telegram (and some other chat apps) re-percent-encode a pasted URL whose
    // query already carries %XX escapes: our `%20` becomes `%2520`, `%C5%BC`
    // becomes `%25C5%25BC`. Play decodes that once, so `title` arrives with a
    // literal `%20` / `%C5%BC` still in it and the direct match misses. On a
    // miss, decode the residual escapes once more and retry.
    val decoded: Option[String] =
      Option(title)
        .filter(FilmDeepLinks.looksPercentEncoded)
        .map(t => URLDecoder.decode(t, StandardCharsets.UTF_8))
    lookup(title).orElse(decoded.flatMap(lookup))
      .orElse(knownMovieFallback(city, title, decoded))
  }

  /** Resolve the canonical `/{city}/movie/{slug}` address.
   *
   *  `FilmSlugs` assigned the address, so it is also what reverses it — one
   *  film per slug, whether or not another film shares its title. The re-slug
   *  scan behind it is the fallback for a slug the map doesn't know: a link
   *  minted before a re-key, or the sub-second window while the read model
   *  reloads. Re-slugging alone is what USED to resolve every address, and on a
   *  same-title pair it could only ever reach one of the two films — it stays
   *  as a safety net, not as the rule.
   *
   *  The fallback tie-breaks on the title rather than taking the head, because
   *  `toSchedules` orders by earliest showtime and that shifts through the day. */
  def filmBySlug(city: City, slug: String): Option[FilmSchedule] = {
    // A slug that is no film's address now but was one under the billing-title fold
    // joins its film too; the controller 301s it onto the current address.
    val slugs     = readModel.filmSlugs
    val addressed = slugs.idFor(slug).orElse(slugs.formerIdFor(slug))
    def reslugged(title: String): Boolean = tools.Slugify(title) == slug

    // An address the index knows joins ONE film; only an unknown one walks the
    // city's schedules — and then the corpus — re-slugging titles, the safety
    // net for a stale link.
    addressed.fold(schedules.toSchedules(city).filter(s => reslugged(s.movie.title)).minByOption(_.movie.title))(id => schedules.schedulesFor(city, Set(id)).headOption)
      .orElse {
        // The one fallback that is a fault, not traffic: the address names a film with
        // nothing here while a namesake under another id plays in this city — a re-key
        // or an identity split left the link on the dead row.
        def namesakeHere(id: String): Option[FilmSchedule] =
          schedules.toSchedules(city).filter(s => s.resolved._id != id && reslugged(s.movie.title)).minByOption(_.movie.title)
        readModelFallback(
          city,
          addressed.fold(readModel.allMovies().filter(m => reslugged(m.title)).minByOption(_.title))(readModel.movie),
          reference = s"slug='$slug'",
          namesake = addressed.flatMap(namesakeHere)
        )
      }
  }

  /** Resilience for film deep-links: a title the read model KNOWS but that has no
   *  live schedule in this city right now must not 404 a shared/bookmarked link.
   *  Render the movie with an empty showings list instead. Almost every hit is an
   *  ended run: a crawler (OAI-SearchBot, ExaSearchBot, bingbot, Applebot) or an old
   *  link re-fetching a `/{city}/movie/{slug}` the sitemap or the other-cities modal
   *  listed while the film still played there — the census of 2026-11-12 found
   *  ~2,000 a day, 95% of them for films live in other cities and pruned here,
   *  the rest ended everywhere. That is traffic, logged at INFO. It also covers the
   *  sub-second window while the worker re-projects a film (`web_movies` and
   *  `web_screenings` arrive over two change streams), which reads the same and
   *  self-heals on the next load. A title the read model has never seen still
   *  returns None. */
  /** The same film's page in every other city of `city`'s country currently showing it
   *  ([[MovieControllerService.citiesShowing]]), each with its address. */
  def otherCities(schedule: FilmSchedule, city: City): Seq[(City, String)] =
    schedules.citiesShowing(schedule, city).map(sibling => sibling -> FilmHref.forSlug(schedule.slug, schedule.movie.title, sibling))

  private def knownMovieFallback(city: City, title: String, decoded: Option[String]): Option[FilmSchedule] = {
    // Newest first, per `FilmTitles`: a same-title pair with no live schedule
    // here resolves to the film that holds the bare slug.
    def byTitle(t: String): Option[ResolvedMovie] =
      readModel.filmTitles.idsFor(t).iterator.flatMap(readModel.movie).nextOption()
    readModelFallback(city, byTitle(title).orElse(decoded.flatMap(byTitle)), reference = s"title='$title'")
  }

  /** Shared tail of both deep-link resolvers (by title and by slug): render the
   *  read model's copy of the movie with no showings. `reference` names whichever
   *  key the caller looked up, so the log line stays actionable. An ended run is
   *  INFO; a `namesake` — another film answering the same address that DOES play in
   *  this city — is a WARN, because the link landed on a row a re-key or an identity
   *  split left behind. */
  private def readModelFallback(city: City, resolved: Option[ResolvedMovie], reference: String,
                                namesake: Option[FilmSchedule] = None): Option[FilmSchedule] =
    resolved.map { movie =>
      namesake match {
        case Some(live) =>
          logger.warn(s"film deep-link resolved to a film with no live ${city.slug} schedule while a namesake plays there " +
            s"(rekey or identity split): $reference filmId=${movie._id} namesake=${live.resolved._id}")
        case None =>
          logger.info(s"film deep-link to a film with no live ${city.slug} schedule (ended run), served without showtimes: " +
            s"$reference filmId=${movie._id}")
      }
      schedules.filmSchedule(movie, cinemaFilmUrls = Seq.empty, showings = Seq.empty, city, schedules.nowIn(city).toLocalDate)
    }
}

object FilmDeepLinks {
  private val PercentEscape = "%[0-9A-Fa-f]{2}".r

  /** Does the string still contain an unresolved `%XX` escape? Used to spot a
   *  doubly-encoded title (see [[FilmDeepLinks.film]]) without
   *  touching the normal, already-decoded path. */
  private def looksPercentEncoded(s: String): Boolean =
    PercentEscape.findFirstIn(s).isDefined
}
