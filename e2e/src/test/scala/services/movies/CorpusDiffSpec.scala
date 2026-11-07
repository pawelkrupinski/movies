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

  // PL convergence #798: two orders' films differed only in `tmdbProvisional` (one pass ended with the take Pending), and
  // the FILMS axis printed its header and nothing else -- the diff named a fixed list of fields that left it out.
  it should "name every field two films differ in, not only the ones it lists" in {
    import models.Kinoteka
    val a = film("Tajemnica", Kinoteka, "Tajemnica", tmdb = Some(1))
    val b = a.copy(record = a.record.copy(tmdbProvisional = Some(true)))

    val diff = CorpusDiff.records(Seq(a), Seq(b), "pass1", "pass2")
    diff should include ("tmdbProvisional: pass1=None pass2=Some(true)")
    diff.linesIterator.next() shouldBe "1 film(s) differ — tmdbProvisional ×1"
  }

  "the pending takes a pass ended with" should "name each take's listings and what it awaits, capped, and nothing decided" in {
    val lines = CorpusDiff.awaiting(Seq(Seq("Kino B", "Kino A") -> Set("q2", "q1"), Seq("Kino C") -> Set.empty, Seq("Kino D") -> Set("q3")), limit = 1)
    lines shouldBe Seq("Kino A | Kino B awaits q1; q2", "…and 1 more take(s)")
  }

  // Germany's order passes in two JVMs: 6 films printed "source 'TMDB' differs:" and no field -- the TMDB slot's
  // titleSearches, a field the slot diff did not list.
  "the slot diff" should "name every field two slots differ in" in {
    import models.{MovieRecord, Source, SourceData, TitleSearch, Tmdb}
    def row(rank: Option[Int]) = StoredMovieRecord("Der Fremde", Some(2025), MovieRecord(data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Der Fremde"), titleSearches = Seq(TitleSearch("derfremde", rank, 2))))), FilmId("fremde"))

    val diff = CorpusDiff.records(Seq(row(Some(4))), Seq(row(Some(5))), "pass0", "pass1")
    diff should include ("titleSearches: pass0=List(TitleSearch(derfremde,Some(4),2)) pass1=List(TitleSearch(derfremde,Some(5),2))")
  }

  "the film diff" should "lead with which fields differ and how often, then show the first films, capped" in {
    import models.Kinoteka
    val a = (1 to 5).map(i => film(s"Film $i", Kinoteka, s"Film $i", tmdb = Some(i)))
    val b = (1 to 5).map(i => film(s"Film $i", Kinoteka, s"Film $i", tmdb = Some(100 + i)))

    val diff = CorpusDiff.records(a, b, "pass0", "pass1", limit = 2)
    diff.linesIterator.next() shouldBe "5 film(s) differ — tmdbId ×5"
    diff.linesIterator.count(_.startsWith("record ")) shouldBe 2
    diff should include ("…and 3 more film(s)")
  }
  // Verify scrape recording #14, Germany: an unchanged listing moved film on the next day, and nothing said whether the
  // first day's model already took that film (the corpus lagged the model) or did not (the model's own state lagged a
  // whole resolve). Each listing whose stored film is not the day-one model's is named, with what the model takes.
  "the day-one lag" should "name each listing whose stored film is not the model's, and nothing that agrees" in {
    val stored = Seq(("BTS World Tour", "Olympia | BTS WORLD TOUR", None), ("Anora", "Kinoteka | ANORA", Some(1)))
    val model  = Map("Olympia | BTS WORLD TOUR" -> (Some(1820088), "tmdb 1820088 (0.0%, OwnMatch)"),
                     "Kinoteka | ANORA" -> (Some(1), "tmdb 1 (99.0%, OwnMatch)"))
    CorpusDiff.modelAhead(stored, model) shouldBe
      Seq("'BTS World Tour' ← Olympia | BTS WORLD TOUR: stored no film, the day-one model takes tmdb 1820088 (0.0%, OwnMatch)")
  }

  it should "say when the model holds no decision for a stored listing, and cap its lines" in {
    val stored = (1 to 3).map(i => (s"Film $i", s"Kino | F$i", Some(i)))
    CorpusDiff.modelAhead(stored, Map.empty, limit = 2) shouldBe Seq(
      "'Film 1' ← Kino | F1: stored tmdb 1, the day-one model takes no decision", "'Film 2' ← Kino | F2: stored tmdb 2, the day-one model takes no decision",
      "…and 1 more listing(s)")
  }
}
