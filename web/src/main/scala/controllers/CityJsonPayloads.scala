package controllers

import models.City
import play.api.libs.json.{JsValue, Json}

import java.time.LocalDate

/** A city's JSON payloads for the mobile apps — `/api/repertoire`, `/api/details` and
 *  `/api/cinemas` — as the bodies [[ConditionalResponse]] serves.
 *
 *  Each film's object in the per-film payloads is kept under the schedule it is
 *  serialized from. A payload is re-rendered whenever its city's stamp moves, and
 *  building every film's play-json tree again was ~26 MB and ~20 ms a Warsaw
 *  `/api/repertoire`, for the handful of films that had moved. The key holds everything
 *  an object is a function of — the payload and its day window, the city (its
 *  language), the day, the schedule — so a changed film asks for another key and a
 *  superseded entry only ages out of the byte bound — every entry weighing at least
 *  `FilmJsonEntryBytes`, a film left out (`None`) too: Caffeine never evicts a
 *  zero-weight entry, and each pins its key's whole schedule. */
final class CityJsonPayloads {
  import CityJsonPayloads._

  private val filmJson: com.github.benmanes.caffeine.cache.Cache[FilmJsonKey, Option[String]] =
    tools.BoundedCache.ofWeight[FilmJsonKey, Option[String]](FilmJsonMaxBytes, FilmJsonEntryBytes)(
      (_, json) => json.fold(0)(CaffeineFilmCardFragments.bytesHeld))
      .build[FilmJsonKey, Option[String]]()

  /** How many films' API JSON this is holding — for the spec. */
  private[controllers] def filmJsonHeld: Long = { filmJson.cleanUp(); filmJson.estimatedSize() }
  /** What the held films' API JSON weighs against [[CityJsonPayloads.FilmJsonMaxBytes]] — for the spec. */
  private[controllers] def filmJsonWeighed: Long = filmJson.policy().eviction().get().weightedSize().getAsLong

  /** Lean listing — everything the grid + filters need, no heavy detail text —
   *  cut to `window` days from `today` ([[CityJsonPayloads.withinWindow]]). */
  def repertoire(city: City, today: LocalDate, films: Seq[FilmSchedule], window: Option[Int]): ResponseBody =
    filmJsonArray("repertoire" + windowCacheKey(window), city, today, films) { film =>
      withinWindow(film, today, window).map(windowed => Json.toJson(ApiFilm.from(windowed, city.country.language)))
    }

  /** Detail-only payload (synopsis + trailers), one object per film that has any. */
  def details(city: City, today: LocalDate, films: Seq[FilmSchedule]): ResponseBody =
    filmJsonArray("details", city, today, films) { film =>
      Some(ApiFilmDetails.from(film)).filter(ApiFilmDetails.hasContent).map(Json.toJson(_))
    }

  /** A city's JSON payload as an array of one object per film, each film's object
   *  serialized once and kept (see `filmJson`); a film for which `json` answers `None`
   *  is left out. `payload` names what the objects are, so two payloads never share one. */
  private def filmJsonArray(payload: String, city: City, today: LocalDate, films: Seq[FilmSchedule])
                           (json: FilmSchedule => Option[JsValue]): ResponseBody =
    ResponseBody.jsonArray(films.iterator.flatMap { film =>
      filmJson.get(FilmJsonKey(payload, city.slug, today, film), _ => json(film).map(Json.stringify))
    })
}

object CityJsonPayloads {

  /** The city's cinema universe + area grouping (static) — see [[ApiCityCinemas]]. */
  def cinemas(city: City, today: LocalDate): ResponseBody =
    ResponseBody.text(Json.stringify(Json.toJson(ApiCityCinemas.from(city, today))))

  /** How many days from today a listing request wants, or `None` for everything.
   *
   *  Clamped rather than trusted: the parameter reaches us from a URL, and an
   *  unbounded one is a way to ask for arbitrary work. `<= 0` is meaningless, so
   *  it reads as "no window" only when the parameter is absent -- an explicit
   *  `days=0` is clamped to one day rather than silently returning everything,
   *  because an empty answer is easier to notice than a 700 KB one. */
  def dayWindow(days: Option[Int]): Option[Int] = days.map(n => math.max(1, math.min(n, MaxDayWindow)))

  /** The ceiling on `?days=`. The corpus reaches ~10 months ahead (London's last
   *  date was 2027-07-03 when this was written), so anything past a year is the
   *  whole payload by another name. */
  val MaxDayWindow: Int = 400

  /** Part of the gzip cache key, so two windows cannot share one entry. Spelled
   *  out rather than derived from the raw query so `?days=07` and `?days=7` land
   *  on the same entry instead of two identical ones. */
  def windowCacheKey(window: Option[Int]): String = window.fold("")(n => s"|days=$n")

  /** The film, if it has at least one showing inside the window, carrying only the
   *  showings inside it.
   *
   *  CALENDAR DAYS FROM TODAY, not "the first N dates that have showings": a film
   *  whose only screening is in December must not appear in `days=7` just because
   *  it happens to be the next date on its own list. A film left with nothing in
   *  the window is dropped entirely rather than emitted with an empty
   *  `showings` -- an empty film is a card the client would have to render and
   *  then hide. */
  def withinWindow(film: FilmSchedule, today: java.time.LocalDate, window: Option[Int]): Option[FilmSchedule] = window match {
    case None => Some(film)
    case Some(n) =>
      val limit = today.plusDays(n.toLong)
      val kept  = film.showings.filter { case (date, _) => !date.isBefore(today) && date.isBefore(limit) }
      if (kept.isEmpty) None else Some(film.copy(showings = kept))
  }

  /** What `filmJson` keys a film's serialized object by: which
   *  payload (and day window) it belongs to, the city, the day and the schedule. */
  private[controllers] final case class FilmJsonKey(payload: String, city: String, today: java.time.LocalDate, film: FilmSchedule)

  /** The per-film API JSON kept: a few cities' payloads several times over
   *  (a Warsaw `/api/repertoire` is ~1.5 MB of JSON before gzip). */
  val FilmJsonMaxBytes: Long = 16L * 1024 * 1024

  /** The least a kept film's API JSON weighs: its entry and the schedule its key pins, which
   *  outlive the schedule's own rebuild until evicted. A film left out of a payload holds no
   *  JSON but still pins its schedule — weighed as nothing, Caffeine would never evict it. */
  val FilmJsonEntryBytes: Int = 1024
}
