package services.movies

import models.{Filmweb, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.RetriggerKind._

class MergeRetriggerSpec extends AnyFlatSpec with Matchers {

  private def rec(
    tmdbId:      Option[Int]    = None,
    imdbId:      Option[String] = None,
    searchTitle: Option[String] = None,
    tmdbNoMatch: Boolean        = false,
    original:    Option[String] = None
  ): MovieRecord =
    MovieRecord(tmdbId = tmdbId, imdbId = imdbId, searchTitle = searchTitle, tmdbNoMatch = tmdbNoMatch,
      data = original.map(o => Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some(o)))).getOrElse(Map.empty))

  private def k(title: String, year: Option[Int] = Some(2026)) = CacheKey(title, year)

  private def decide(before: MovieRecord, bk: CacheKey, after: MovieRecord, ak: CacheKey) =
    MergeRetrigger.changedEnrichments(before, bk, after, ak)

  "changedEnrichments" should "be empty when nothing an enrichment reads changed" in {
    val r = rec(tmdbId = Some(1), imdbId = Some("tt1"), searchTitle = Some("Foo"))
    decide(r, k("Foo"), r, k("Foo")) shouldBe empty
  }

  it should "re-kick the title-driven ratings when a resolved row is RE-KEYED to a new title" in {
    val before = rec(tmdbId = Some(1), imdbId = Some("tt1"))
    val after  = rec(tmdbId = Some(1), imdbId = Some("tt1"))
    val kinds  = decide(before, k("Fatherland"), after, k("Ojczyzna"))
    kinds should contain allOf (FilmwebRating, RtRating, McRating)
    kinds should not contain ResolveTmdb   // already resolved — never re-resolve (re-point risk)
    kinds should not contain ImdbRating    // imdbId unchanged
  }

  it should "re-kick the IMDb rating when the imdbId changed" in {
    val before = rec(tmdbId = Some(1), imdbId = None)
    val after  = rec(tmdbId = Some(1), imdbId = Some("tt9"))
    decide(before, k("Foo"), after, k("Foo")) should contain (ImdbRating)
  }

  it should "re-kick IMDb-id resolution when the tmdbId firmed up but the id is still missing" in {
    val before = rec(tmdbId = None, imdbId = None, searchTitle = Some("Foo"))
    val after  = rec(tmdbId = Some(7), imdbId = None, searchTitle = Some("Foo"))
    val kinds  = decide(before, k("Foo"), after, k("Foo"))
    kinds should contain (ResolveImdbId)
    kinds should contain allOf (FilmwebRating, RtRating, McRating)  // tmdbId changed
  }

  it should "re-resolve TMDB only for an UNRESOLVED row whose title/year changed" in {
    val before = rec(tmdbId = None)
    val after  = rec(tmdbId = None)
    val kinds  = decide(before, k("Foo", Some(2025)), after, k("Foo", Some(2026)))
    kinds should contain (ResolveTmdb)
    kinds should not contain FilmwebRating   // unresolved → no firm film to rate yet
  }

  it should "NOT re-resolve TMDB for a tmdbNoMatch row even when its title changed" in {
    val before = rec(tmdbId = None, tmdbNoMatch = true)
    val after  = rec(tmdbId = None, tmdbNoMatch = true)
    decide(before, k("Foo"), after, k("Bar")) should not contain ResolveTmdb
  }

  it should "re-resolve TMDB for a tmdbNoMatch row when a NEW originalTitle hint arrives (Filmweb crack)" in {
    // The whole point of Filmweb-driven re-resolution: a film TMDB missed gains a
    // Filmweb original title, which is a new search term that might now resolve it.
    val before = rec(tmdbId = None, tmdbNoMatch = true)
    val after  = before.copy(data = Map[Source, SourceData](Filmweb -> SourceData(originalTitle = Some("Der letzte Concierge"))))
    val kinds  = decide(before, k("Ostatni konsjerż"), after, k("Ostatni konsjerż"))
    kinds should contain (ResolveTmdb)
    kinds should contain (ResolveImdbId)   // no imdbId + a new hint
  }

  it should "re-resolve TMDB for a tmdbNoMatch row when a director arrives from Filmweb" in {
    val before = rec(tmdbId = None, tmdbNoMatch = true)
    val after  = before.copy(data = Map[Source, SourceData](Filmweb -> SourceData(director = Seq("Gastón Solnicki"))))
    decide(before, k("Ostatni konsjerż"), after, k("Ostatni konsjerż")) should contain (ResolveTmdb)
  }

  it should "NOT re-resolve TMDB for a tmdbNoMatch row on a Filmweb write that adds no new hint (rating/genres only)" in {
    // A bare rating/genres refresh isn't a disambiguator — must not churn a re-resolve.
    val before = rec(tmdbId = None, tmdbNoMatch = true)
    val after  = before.copy(data = Map[Source, SourceData](Filmweb -> SourceData(genres = Seq("Dramat"))))
    decide(before, k("Ostatni konsjerż"), after, k("Ostatni konsjerż")) should not contain ResolveTmdb
  }

  it should "re-resolve TMDB when an unresolved row gains an originalTitle hint from the merge" in {
    val before = rec(tmdbId = None, original = None)
    val after  = rec(tmdbId = None, original = Some("The Original"))
    decide(before, k("Foo"), after, k("Foo")) should contain (ResolveTmdb)
  }

  it should "re-kick IMDb-id resolution when director data arrives on a TMDB-resolved but imdbId-less row" in {
    // `director` is derived from sourceData, so inject via data map
    val beforeRec = rec(tmdbId = Some(7), imdbId = None)
    val afterRec  = beforeRec.copy(data = Map(Tmdb -> SourceData(director = Seq("Jan Nowak"))))
    decide(beforeRec, k("Foo"), afterRec, k("Foo")) should contain (ResolveImdbId)
  }

  it should "re-kick IMDb-id resolution when originalTitle changes on a TMDB-resolved but imdbId-less row" in {
    val before = rec(tmdbId = Some(7), imdbId = None, original = None)
    val after  = rec(tmdbId = Some(7), imdbId = None, original = Some("The Original Title"))
    decide(before, k("Foo"), after, k("Foo")) should contain (ResolveImdbId)
  }

  it should "NOT re-kick IMDb-id resolution when director changes but the row already has an imdbId" in {
    val beforeRec = rec(tmdbId = Some(7), imdbId = Some("tt1"))
    val afterRec  = beforeRec.copy(data = Map(Tmdb -> SourceData(director = Seq("Jan Nowak"))))
    decide(beforeRec, k("Foo"), afterRec, k("Foo")) should not contain ResolveImdbId
  }

  it should "re-kick IMDb-id resolution when tmdbNoMatch flips to true (film not on TMDB but may be on IMDb)" in {
    val before = rec(tmdbId = None, tmdbNoMatch = false, imdbId = None)
    val after  = rec(tmdbId = None, tmdbNoMatch = true,  imdbId = None)
    decide(before, k("Nomadland"), after, k("Nomadland")) should contain (ResolveImdbId)
  }

  it should "re-kick IMDb-id resolution when a tmdbNoMatch row gains an originalTitle hint" in {
    val before = rec(tmdbId = None, tmdbNoMatch = true, imdbId = None, original = None)
    val after  = rec(tmdbId = None, tmdbNoMatch = true, imdbId = None, original = Some("Past Lives"))
    decide(before, k("Poprzednie życie"), after, k("Poprzednie życie")) should contain (ResolveImdbId)
  }

  it should "NOT re-kick IMDb-id resolution for a tmdbNoMatch row that already has an imdbId" in {
    val before = rec(tmdbId = None, tmdbNoMatch = true, imdbId = Some("tt13238346"))
    val after  = rec(tmdbId = None, tmdbNoMatch = true, imdbId = Some("tt13238346"), original = Some("Past Lives"))
    decide(before, k("Poprzednie życie"), after, k("Poprzednie życie")) should not contain ResolveImdbId
  }
}
