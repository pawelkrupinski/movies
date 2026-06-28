package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import models.{CinemaShowing, Multikino, MovieRecord, Showtime, Source, SourceData}
import services.movies.StoredMovieRecord

/**
 * The `movies` collection now keys every cinema slot by `CinemaShowing(cinema,
 * sanitize(title))` — a venue holds one slot per genuinely-distinct SHOWN title
 * (the same film listed under its Polish title and its original/transliterated
 * title at once), each with its own showtimes/synopsis. The read-model card
 * split keys on that per-title `titleKey`. The debug views must reflect that
 * production shape:
 *
 *  - the details breakdown iterates the actual cinema SLOTS, so a slot keyed by
 *    `CinemaShowing` still renders (a `Cinema.all` walk that looks up a BARE
 *    `Cinema` key misses every production slot), and a venue with two title
 *    slots shows BOTH;
 *  - the table's cinema-count column surfaces the slot count when it exceeds the
 *    distinct-cinema count, so the split is visible at a glance.
 *
 * (Decoration variants like "… - dubbing" canonicalise to the same `titleKey`
 * and so share a slot — the split is for shown titles that differ in WORDS.)
 */
class DebugViewPerTitleSlotSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan
  private val now = LocalDateTime.now()

  // One film, two genuinely-distinct shown titles at the same venue — its
  // original title and its Polish title — so the two slot keys differ.
  private val originalTitle = "The Substance"
  private val polishTitle   = "Substancja"
  private val original = CinemaShowing.keyFor(Multikino, originalTitle)
  private val polish   = CinemaShowing.keyFor(Multikino, polishTitle)

  private def slot(title: String): SourceData =
    SourceData(title = Some(title), showtimes = Seq(Showtime(now.plusHours(2), None, None, Nil)))

  private def detailsOf(data: Map[Source, SourceData]): String =
    views.html.debugDetails(polishTitle, Some(2024), MovieRecord(data = data)).body

  "debugDetails" should "render a cinema slot keyed by CinemaShowing (the production shape)" in {
    val html = detailsOf(Map[Source, SourceData](original -> slot(originalTitle)))

    html should include ("Multikino")
    html should include (originalTitle)
  }

  it should "render every per-title slot a single venue holds, each with its slot key" in {
    val html = detailsOf(Map[Source, SourceData](
      original -> slot(originalTitle),
      polish   -> slot(polishTitle),
    ))

    html should include (originalTitle)
    html should include (polishTitle)
    // Both per-title slot keys (`titleKey`) are surfaced so the split is legible.
    original.titleKey should not be polish.titleKey
    html should include (original.titleKey)
    html should include (polish.titleKey)
  }

  "debug table row" should "show the per-title slot count when a venue holds several" in {
    val row = StoredMovieRecord(
      title = polishTitle, year = Some(2024),
      record = MovieRecord(data = Map[Source, SourceData](
        original -> slot(originalTitle),
        polish   -> slot(polishTitle),
      )))
    val html = views.html.debug(Seq(row)).body

    // One distinct cinema (the sort key stays the cinema count) …
    html should include ("""data-cinemas="1"""")
    // … but two per-title slots, surfaced alongside.
    html should include ("(2)")
  }
}
