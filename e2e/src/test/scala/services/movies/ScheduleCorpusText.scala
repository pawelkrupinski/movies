package services.movies

import controllers.{FilmSchedule, MovieControllerService}
import models.{City, MovieRecord}
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.FixtureTestWiring

import java.time.LocalDateTime
import java.util.Locale

/** The whole-corpus text `expected-schedules.txt` holds — shared by `FilmScheduleEndToEndSpec`,
 *  which pins it, and `ObservationCaptureEndToEndSpec`, which proves capture leaves it
 *  byte-identical. */
object ScheduleCorpusText {

  /** The whole corpus of a booted fixture wiring, as the `/` view for `city` at `now` renders it. */
  def of(wiring: FixtureTestWiring, city: City, now: LocalDateTime): String = {
    val records = recordsByFilmId(wiring)
    render(new MovieControllerService(wiring.webReadModel, wiring.clock).toSchedules(city, now), s => records.get(s.resolved._id))
  }

  /** Every card's source record, by film id. A record can project to SEVERAL cards (one per
   *  shown title — the read-model split), so EVERY variant film id maps back to its record.
   *  Read from the REPOSITORY (its showtimes are authoritative — stitched from `screenings`
   *  under the read-split), NOT `movieCache.snapshot()`, whose resident records are stripped
   *  of showtime lists (index-only cache). */
  def recordsByFilmId(wiring: FixtureTestWiring): Map[String, MovieRecord] =
    wiring.movieRepository.findAll().flatMap { r =>
      services.readmodel.ReadModelProjection.filmIds(r, titleNormalizer).map(_ -> r.record)
    }.toMap

  /** Render every FilmSchedule into a deterministic multi-line block. One
   *  block per film, separated by blank lines; films sorted alphabetically
   *  by display title. Each block lists every field a viewer of the `/`
   *  card would see (title, runtime, year, poster, synopsis size, cast,
   *  director, per-cinema deep-links) plus every enrichment value (tmdbId,
   *  imdbId, ratings, MC/RT/FW URLs, per-cinema slot provenance) plus the
   *  full per-(date, cinema) showtime list with room + format tokens. */
  def render(schedules: Seq[FilmSchedule], recordFor: FilmSchedule => Option[MovieRecord]): String =
    schedules.sortBy(s => (s.movie.title.toLowerCase(Locale.ROOT), s.movie.releaseYear)).map(renderOne(_, recordFor)).mkString("\n\n")

  private def renderOne(s: FilmSchedule, recordFor: FilmSchedule => Option[MovieRecord]): String = {
    val e = recordFor(s)
    val cinemaUrls = s.cinemaFilmUrls.sortBy(_._1.displayName)
      .map { case (c, u) => s"${c.displayName} = $u" }
    val scrapes = e.map(_.cinemaData.toSeq
      .sortBy { case (c, sd) => (c.displayName, sd.title.getOrElse(""), sd.releaseYear.getOrElse(Int.MinValue)) }
      .map { case (c, sd) =>
        s"${c.displayName} / ${sd.title.getOrElse("—")} / ${sd.releaseYear.map(_.toString).getOrElse("—")}"
      })
      .getOrElse(Nil)
    // `cinemaTitles` is the set of raw spellings each cinema reported.
    // `displayTitle` is the picker's choice across that set — the
    // canonical form that `MovieController.toSchedules` writes into
    // `Movie.title` (and therefore the `=== TITLE ===` header above).
    // Showing both makes the picker's behaviour explicit per film.
    val cinemaTitles = e.map(_.evidence.titles.toSeq.sorted).getOrElse(Nil)
    val showings = s.showings.sortBy(_._1).flatMap { case (date, byCinema) =>
      byCinema.sortBy(_.cinema.displayName).map { sht =>
        val slots = sht.showtimes.sortBy(_.dateTime).map { st =>
          val room   = st.room.fold("")(r => s" $r")
          val format = if (st.format.isEmpty) "" else s" ${st.format.mkString("/")}"
          s"${st.dateTime.toLocalTime}$room$format"
        }.mkString(" · ")
        f"  $date  ${sht.cinema.displayName}%-28s  $slots"
      }
    }
    val lines = Seq(
      s"=== ${s.movie.title} ===",
      s"displayTitle:      ${s.movie.title}",
      s"cinemaTitles:      ${if (cinemaTitles.isEmpty) "—" else cinemaTitles.mkString(" | ")}",
      s"runtimeMinutes:    ${s.movie.runtimeMinutes.map(_.toString).getOrElse("—")}",
      s"releaseYear:       ${s.movie.releaseYear.map(_.toString).getOrElse("—")}",
      s"countries:         ${if (s.movie.countries.isEmpty) "—" else s.movie.countries.mkString(", ")}",
      s"posterUrl:         ${s.posterUrl.getOrElse("—")}",
      s"synopsis.length:   ${s.synopsis.map(_.length.toString).getOrElse("—")}",
      s"cast:              ${if (s.cast.nonEmpty) s.cast.mkString(", ") else "—"}",
      s"director:          ${if (s.director.nonEmpty) s.director.mkString(", ") else "—"}",
      s"tmdbId:            ${e.flatMap(_.tmdbId).map(_.toString).getOrElse("—")}",
      s"imdbId:            ${e.flatMap(_.imdbId).getOrElse("—")}",
      s"originalTitle:     ${e.flatMap(_.originalTitle).getOrElse("—")}",
      s"imdbRating:        ${e.flatMap(_.imdbRating).map(_.toString).getOrElse("—")}",
      s"metascore:         ${e.flatMap(_.metascore).map(_.toString).getOrElse("—")}",
      s"rottenTomatoes:    ${e.flatMap(_.rottenTomatoes).map(_.toString).getOrElse("—")}",
      s"filmwebRating:     ${e.flatMap(_.filmwebRating).map(_.toString).getOrElse("—")}",
      s"metacriticUrl:     ${e.flatMap(_.metacriticUrl).getOrElse("—")}",
      s"rottenTomatoesUrl: ${e.flatMap(_.rottenTomatoesUrl).getOrElse("—")}",
      s"filmwebUrl:        ${e.flatMap(_.filmwebUrl).getOrElse("—")}"
    ) ++
      (if (cinemaUrls.nonEmpty) Seq("cinemaFilmUrls:") ++ cinemaUrls.map("  " + _) else Seq("cinemaFilmUrls:    —")) ++
      (if (scrapes.nonEmpty) Seq("cinemaScrapes:") ++ scrapes.map("  " + _) else Seq("cinemaScrapes:     —")) ++
      Seq("showings:") ++ showings
    lines.mkString("\n")
  }
}
