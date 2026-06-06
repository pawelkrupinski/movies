package tools

import play.api.libs.json._

import java.time.LocalDateTime

/**
 * Deterministic JSON rendering for [[FilmwebDiff]]: a third output alongside the
 * text report and CSV summary, with a per-film discrepancy breakdown so two days'
 * runs diff cleanly.
 *
 * Pure (no clock, no network, no I/O): [[render]] takes already-computed per-cinema
 * results + run metadata and returns a `JsValue`. Everything is SORTED — cinemas by
 * (city, cinema), discrepancies by normalised film title, screening times
 * ascending — so re-running on identical data yields byte-identical JSON. The
 * `FilmwebDiff` tool builds the [[CinemaResult]]s from the SAME normalised matching
 * it uses for the text/CSV (`FilmwebDiffTitleNormalizer`), so all three agree.
 */
object FilmwebDiffJson {

  /** Run-level metadata for `meta`. */
  final case class Meta(
    date:        String, // the run's `today`, yyyy-MM-dd
    generatedAt: String, // Instant.now() ISO-8601
    windowDays:  Int,
    commit:      String  // GITHUB_SHA or "unknown"
  )

  /** One cinema's full comparison, rich enough to render the per-film breakdown.
   *
   *  `oursByFilm` / `fwByFilm` are normalised-title → screening times (the same
   *  maps FilmwebDiff computes for the text report). For cinemas with no Filmweb
   *  id or a failed fetch they're empty and the counts are 0; the cinema is still
   *  included so the set is stable across days. */
  final case class CinemaResult(
    city:        String,
    cinema:      String,
    filmwebId:   Option[Int],
    resolvedVia: String, // "fuzzy" | "override" | "none"
    verdict:     String,
    oursByFilm:  Map[String, Seq[LocalDateTime]],
    fwByFilm:    Map[String, Seq[LocalDateTime]]
  ) {
    def oursCount: Int = oursByFilm.valuesIterator.map(_.size).sum
    def fwCount:   Int = fwByFilm.valuesIterator.map(_.size).sum
  }

  def render(results: Seq[CinemaResult], meta: Meta): JsValue = {
    val sorted = results.sortBy(r => (r.city, r.cinema))
    Json.obj(
      "meta" -> Json.obj(
        "date"        -> meta.date,
        "generatedAt" -> meta.generatedAt,
        "windowDays"  -> meta.windowDays,
        "commit"      -> meta.commit
      ),
      "summary" -> summary(sorted),
      "cinemas" -> JsArray(sorted.map(cinemaJson))
    )
  }

  private def summary(results: Seq[CinemaResult]): JsObject = {
    def count(v: String): Int = results.count(_.verdict == v)
    Json.obj(
      "cinemas"     -> results.size,
      "same"        -> count("SAME"),
      "oursExtra"   -> count("OURS_EXTRA"),
      "fwExtra"     -> count("FW_EXTRA"),
      "bothDiffer"  -> count("BOTH_DIFFER"),
      "fetchFailed" -> (count("OUR_FETCH_FAILED") + count("FW_FETCH_FAILED")),
      "noFilmwebId" -> results.count(_.verdict == "NO_FILMWEB_ID")
    )
  }

  private def cinemaJson(r: CinemaResult): JsObject = {
    val oursTimes = r.oursByFilm.valuesIterator.flatten.toSeq
    val fwTimes   = r.fwByFilm.valuesIterator.flatten.toSeq
    Json.obj(
      "city"          -> r.city,
      "cinema"        -> r.cinema,
      "filmwebId"     -> r.filmwebId,
      "resolvedVia"   -> r.resolvedVia,
      "ours"          -> r.oursCount,
      "fw"            -> r.fwCount,
      "shared"        -> multisetIntersect(oursTimes, fwTimes).size,
      "oursOnly"      -> multisetDiff(oursTimes, fwTimes).size,
      "fwOnly"        -> multisetDiff(fwTimes, oursTimes).size,
      "verdict"       -> r.verdict,
      "discrepancies" -> JsArray(discrepancies(r))
    )
  }

  /** Per-film breakdown — ONLY films whose ours/fw screening sets differ. Sorted
   *  by normalised film title. Exact-match films are omitted. */
  private def discrepancies(r: CinemaResult): Seq[JsObject] = {
    val films = (r.oursByFilm.keySet | r.fwByFilm.keySet).toSeq.sorted
    films.flatMap { film =>
      val ours = r.oursByFilm.getOrElse(film, Nil)
      val fw   = r.fwByFilm.getOrElse(film, Nil)
      val oursOnly = multisetDiff(ours, fw)
      val fwOnly   = multisetDiff(fw, ours)
      if (oursOnly.isEmpty && fwOnly.isEmpty) None
      else Some(Json.obj(
        "film"     -> film,
        "ours"     -> ours.size,
        "fw"       -> fw.size,
        "shared"   -> multisetIntersect(ours, fw).size,
        "oursOnly" -> isoTimes(oursOnly),
        "fwOnly"   -> isoTimes(fwOnly)
      ))
    }
  }

  /** ISO `yyyy-MM-dd'T'HH:mm` strings, sorted ascending. */
  private def isoTimes(times: Seq[LocalDateTime]): Seq[String] =
    times.sorted.map(t => f"${t.toLocalDate}T${t.getHour}%02d:${t.getMinute}%02d")

  // Multiset ops over LocalDateTime: count duplicates correctly. Shared with
  // FilmwebDiff's text/CSV path so all three outputs agree — a duplicate slot on
  // one side is a diff.
  private[tools] def multisetIntersect(a: Seq[LocalDateTime], b: Seq[LocalDateTime]): Seq[LocalDateTime] = {
    val bCounts = scala.collection.mutable.HashMap[LocalDateTime, Int]()
    b.foreach(x => bCounts.update(x, bCounts.getOrElse(x, 0) + 1))
    a.flatMap { x =>
      val c = bCounts.getOrElse(x, 0)
      if (c > 0) { bCounts.update(x, c - 1); Some(x) } else None
    }
  }

  private[tools] def multisetDiff(a: Seq[LocalDateTime], b: Seq[LocalDateTime]): Seq[LocalDateTime] = {
    val bCounts = scala.collection.mutable.HashMap[LocalDateTime, Int]()
    b.foreach(x => bCounts.update(x, bCounts.getOrElse(x, 0) + 1))
    a.flatMap { x =>
      val c = bCounts.getOrElse(x, 0)
      if (c > 0) { bCounts.update(x, c - 1); None } else Some(x)
    }
  }
}
