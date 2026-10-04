package services.movies

import models.{CinemaShowing, MovieRecord, Showtime, Source, SourceData, UsCinema}

import java.time.LocalDateTime

/** What a projection patch of a film at 2,500 venues costs to diff when one venue moved and every other is the object the
 *  cache holds: `sbt "common/Test/runMain services.movies.PatchDiffBench"`. Not a spec — a measuring tool. */
object PatchDiffBench {
  def main(args: Array[String]): Unit = {
    val normalizer = SingleCountryNormalizer.titleNormalizer
    val start = LocalDateTime.of(2026, 10, 5, 12, 0)
    def slot(v: Int, shift: Int): SourceData = ShowtimesDigest.stripSlot(SourceData(title = Some("Wide Film"), rawTitle = Some("Wide Film"),
      releaseYear = Some(2026), director = Seq("Some Director"), cast = Seq("A One", "B Two", "C Three", "D Four", "E Five"),
      synopsis = Some("A long synopsis " * 20), filmUrl = Some(s"https://v$v/wide-film"),
      showtimes = (0 until 14).map(h => Showtime(start.plusHours((h * 7 + shift).toLong), None))))
    val venues: Seq[Source] = (0 until 2500).map(v => CinemaShowing.keyFor(new UsCinema(s"Venue $v Theatre", s"Venue $v"), "Wide Film", normalizer))
    val before = MovieRecord(tmdbId = Some(1), data = venues.zipWithIndex.map { case (s, v) => s -> slot(v, 0) }.toMap)
    val moved  = venues.head
    val after  = before.copy(data = before.data + (moved -> SourceData(title = Some("Wide Film"), rawTitle = Some("Wide Film"),
      releaseYear = Some(2026), director = Seq("Some Director"), filmUrl = Some("https://v0/wide-film"),
      showtimes = Seq(Showtime(start.plusHours(99), None)))))
    def time(label: String)(body: => Any): Unit = {
      (1 to 30).foreach(_ => body)
      val n = 200; val t = System.nanoTime(); (1 to n).foreach(_ => body)
      println(f"$label%-28s ${(System.nanoTime() - t) / 1e6 / n}%.3f ms")
    }
    time("SlotsRepository.slotOps")(SlotsRepository.slotOps(before.data, after.data))
    time("ScreeningsSplit.writesFor")(ScreeningsSplit.writesFor(before.data, after.data))
    time("MovieRecordPatch.diff")(MovieRecordPatch.diff(before, after))
    time("ShowtimesDigest.leanEqual")(ShowtimesDigest.leanEqual(after, before))
    // As a patch of the moved venue alone (`ProjectedFilm.touched`): every diff over the sources that may differ.
    val touched = Set(moved)
    time("touched: all three diffs") {
      val (b, a) = (LeanRecords.only(before, touched), LeanRecords.only(after, touched))
      SlotsRepository.slotOps(b.data, a.data); ScreeningsSplit.writesFor(b.data, a.data); MovieRecordPatch.diff(b, a)
    }
    time("touched: LeanRecords.equalAt")(LeanRecords.equalAt(after, before, touched))
  }
}
