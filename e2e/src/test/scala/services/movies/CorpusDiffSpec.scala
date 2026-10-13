package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The clue a whole-corpus failure prints. Worth its own spec because its only job
 * is to be READ by someone diagnosing a divergence they cannot reproduce — a diff
 * that hides the difference costs a whole investigation, which is exactly what the
 * previous prefix-based rendering did.
 */
class CorpusDiffSpec extends AnyFlatSpec with Matchers {

  // The real shape: two rendered rows identical for hundreds of characters, then
  // differing in a poster URL. A prefix-based diff printed two identical-looking
  // windows and said they differed.
  private val shared = "FilmSchedule(Movie(Ghost in the Shell,Some(83),Some(1995)," + ("x" * 600)

  "the rendered-row diff" should "show the difference, not a common prefix that hides it" in {
    val a = shared + "poster=foto,20677,117444d5.jpg)"
    val b = shared + "poster=foto,20729,46b3c104.jpg)"

    val diff = CorpusDiff.rows(Seq(a), Seq(b), "pass0", "pass2")

    diff should include ("20677")
    diff should include ("20729")
  }

  it should "name the row index and where the two partway" in {
    val diff = CorpusDiff.rows(Seq("same", shared + "A"), Seq("same", shared + "B"), "pass0", "pass2")

    diff should include ("row 1 differs")
    diff should include (s"common prefix: ${shared.length} chars")
  }

  it should "report a size difference rather than pretending the rows line up" in {
    CorpusDiff.rows(Seq("a", "b"), Seq("a"), "pass0", "pass1") should include ("sizes differ: pass0=2 pass1=1")
  }

  // Equal-length, pairwise-equal, yet unequal as sequences is impossible — but an
  // ORDER-only difference between two multisets is not, and a diff that returned
  // an empty string there would read as "nothing differs".
  it should "say so plainly when no row differs pairwise" in {
    CorpusDiff.rows(Seq("a"), Seq("a"), "pass0", "pass1") should include ("no row differs pairwise")
  }

  it should "stay silent about a match on identical corpora" in {
    CorpusDiff.records(Seq.empty, Seq.empty) shouldBe ""
  }

  // The PL next-day leg printed `cast: first day=Vector(Fiona Shaw, Daisy Edgar-Jones, …)
  // next day=Vector(Daisy Edgar-Jones, Fiona Shaw, …)` — the same three people, re-billed —
  // beside the two fields that really moved, sending the reader after a non-difference.
  it should "not report a cast that was only re-billed, but still report one that grew" in {
    import models.{Kinoteka, MovieRecord, Source, SourceData}
    def row(cast: Seq[String], runtime: Int) = StoredMovieRecord("Rozważna i romantyczna", Some(2026),
      MovieRecord(data = Map[Source, SourceData](Kinoteka -> SourceData(cast = cast, runtimeMinutes = Some(runtime)))),
      FilmId("rozwazna"))
    val billed = Seq("Fiona Shaw", "Daisy Edgar-Jones", "Esme Creed-Miles")

    val rebilled = CorpusDiff.records(Seq(row(billed, 131)), Seq(row(billed.reverse, 112)))
    rebilled should include ("runtimeMinutes")
    rebilled should not include "cast"

    CorpusDiff.records(Seq(row(billed, 131)), Seq(row(billed :+ "George MacKay", 112))) should include ("cast")
  }

  private def film(title: String, venue: models.Cinema, raw: String, tmdb: Option[Int] = None) = {
    import models.{MovieRecord, Source, SourceData}
    StoredMovieRecord(title, Some(2026), MovieRecord(tmdbId = tmdb,
      data = Map[Source, SourceData](venue -> SourceData(title = Some(title), rawTitle = Some(raw)))), FilmId(title))
  }

  // A film the two orders folded differently is the divergence's whole story, and the diff used to stop at it:
  // `record keys differ: only0=Set((Wicked,Some(2026)))` and nothing about the venue, the listing, or the
  // films both orders hold but disagree on.
  it should "name a film only one order made by its listings, and still diff the films both made" in {
    import models.{Kinoteka, KinoPionier}
    val a = Seq(film("Anora", Kinoteka, "ANORA", tmdb = Some(1)), film("Wicked", KinoPionier, "Wicked: Sing-Along"))
    val b = Seq(film("Anora", Kinoteka, "ANORA", tmdb = Some(2)))

    val diff = CorpusDiff.records(a, b, "pass0", "pass1")
    diff should include ("1 film(s) only in pass0")
    diff should include (s"'Wicked' (Some(2026)) ← ${KinoPionier.displayName}: 'Wicked: Sing-Along'")
    diff should include ("scalar tmdbId: pass0=Some(1) pass1=Some(2)")
  }

  it should "lead with which fields differ and how often, then show the first films, capped" in {
    import models.Kinoteka
    val a = (1 to 5).map(i => film(s"Film $i", Kinoteka, s"Film $i", tmdb = Some(i)))
    val b = (1 to 5).map(i => film(s"Film $i", Kinoteka, s"Film $i", tmdb = Some(100 + i)))

    val diff = CorpusDiff.records(a, b, "pass0", "pass1", limit = 2)
    diff.linesIterator.next() shouldBe "5 film(s) differ — tmdbId ×5"
    diff.linesIterator.count(_.startsWith("record ")) shouldBe 2
    diff should include ("…and 3 more film(s)")
  }
}
