package services.movies

import tools.SpecClock.given

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Filmweb, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.RetriggerKind._

class MergeRetriggerSpec extends AnyFlatSpec with Matchers {

  private def rec(
    tmdbId:      Option[Int],
    imdbId:      Option[String] = None,
    searchTitle: Option[String] = None,
    tmdbAttempt: Option[services.resolution.TmdbAttempt] = None,
    original:    Option[String] = None
  ): MovieRecord =
    MovieRecord(tmdbId = tmdbId, imdbId = imdbId, searchTitle = searchTitle, tmdbAttempt = tmdbAttempt,
      data = original.map(o => Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some(o)))).getOrElse(Map.empty))

  private def k(title: String, year: Option[Int] = Some(2026)) = CacheKey(title, year, titleNormalizer)

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

  it should "not re-kick the title-driven ratings for an UNRESOLVED row whose title/year changed" in {
    val kinds = decide(rec(tmdbId = None), k("Foo", Some(2025)), rec(tmdbId = None), k("Foo", Some(2026)))
    kinds should not contain FilmwebRating   // unresolved → no firm film to rate yet
  }

  it should "re-kick IMDb-id resolution for a tmdbNoMatch row when a NEW originalTitle hint arrives" in {
    val before = rec(tmdbId = None, tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy))
    val after  = before.copy(data = Map[Source, SourceData](Filmweb -> SourceData(originalTitle = Some("Der letzte Concierge"))))
    decide(before, k("Ostatni konsjerż"), after, k("Ostatni konsjerż")) should contain (ResolveImdbId)
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
    val before = rec(tmdbId = None, tmdbAttempt = None, imdbId = None)
    val after  = rec(tmdbId = None, tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy),  imdbId = None)
    decide(before, k("Nomadland"), after, k("Nomadland")) should contain (ResolveImdbId)
  }

  it should "re-kick IMDb-id resolution when a tmdbNoMatch row gains an originalTitle hint" in {
    val before = rec(tmdbId = None, tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy), imdbId = None, original = None)
    val after  = rec(tmdbId = None, tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy), imdbId = None, original = Some("Past Lives"))
    decide(before, k("Poprzednie życie"), after, k("Poprzednie życie")) should contain (ResolveImdbId)
  }

  it should "re-kick IMDb-id resolution for a tmdbNoMatch row that already has an imdbId when a fact it searches by arrives" in {
    // US "Volcanoes" (no TMDB record): looked up before "Volcanoes 3D" {Michael Dalton-Smith, 2018} merged, it
    // took IMDb's first "Volcanoes" (2009); after, the 2018 film — whichever listing arrived first won, and
    // Identity model convergence's order-independence leg saw both. With no TMDB id the IMDb id is only as
    // good as the facts it was searched by, so it is searched again when they grow.
    val noMatch = Some(services.resolution.TmdbAttempt.Legacy)
    val before  = rec(tmdbId = None, tmdbAttempt = noMatch, imdbId = Some("tt1477111"))
    decide(before, k("Volcanoes", None), before.copy(data = before.data + (Tmdb -> SourceData(director = Seq("Michael Dalton-Smith")))),
      k("Volcanoes", None)) should contain (ResolveImdbId)
    decide(before, k("Volcanoes", None), rec(tmdbId = None, tmdbAttempt = noMatch, imdbId = Some("tt1477111"), original = Some("Volcanoes: The Fires of Creation")),
      k("Volcanoes", None)) should contain (ResolveImdbId)
    // the same facts: nothing to search again
    decide(before, k("Volcanoes", None), before, k("Volcanoes", None)) should not contain ResolveImdbId
  }

  it should "re-ask Filmweb when a cinema's director or the original title reaches a resolved row, its search reading both" in {
    // Verify scrape recording #23: "Cienie zapomnianych przodków" (TMDB 1965, Filmweb 1964) was asked under TMDB's
    // "Sergei Parajanov" alone and missed; a cinema's "Siergiej Paradżanow", Filmweb's own spelling, came in after, and
    // only the next day's pass asked again and found the page.
    val before    = rec(tmdbId = Some(26782), imdbId = Some("tt0058642"), original = Some("Тіні забутих предків"))
    val credited  = before.copy(data = before.data + (models.Kinoteka -> SourceData(title = Some("Cienie zapomnianych przodków"), director = Seq("Siergiej Paradżanow"))))
    val kinds     = decide(before, k("Cienie zapomnianych przodków", Some(1965)), credited, k("Cienie zapomnianych przodków", Some(1965)))
    kinds should contain (FilmwebRating)
    kinds should not contain ImdbRating   // its id stands
    decide(before, k("Foo"), rec(tmdbId = Some(26782), imdbId = Some("tt0058642"), original = Some("Shadows")), k("Foo")) should contain (FilmwebRating)
    // a row no film is resolved for has nothing to rate yet
    decide(rec(tmdbId = None), k("Foo"), rec(tmdbId = None, original = Some("Shadows")), k("Foo")) should not contain FilmwebRating
  }

  it should "re-ask every rating site for a film TMDB has no record of when what its cinemas publish of it changes" in {
    // Verify scrape recording #26: "Afrykanska Przygoda" (2007), no TMDB film, searched on Filmweb by its cinemas' facts
    // before they were all in, missed; only the next day's pass asked again and found its page.
    val noMatch = Some(services.resolution.TmdbAttempt.Legacy)
    def listed(director: Seq[String], original: Option[String]) = rec(tmdbId = None, tmdbAttempt = noMatch).copy(data =
      Map(models.Kinoteka -> SourceData(title = Some("Afrykanska Przygoda"), releaseYear = Some(2007), director = director, originalTitle = original)))
    val before = listed(Nil, None)
    decide(before, k("Afrykanska Przygoda", Some(2007)), listed(Seq("Rob Bausch"), None), k("Afrykanska Przygoda", Some(2007))) should
      contain allOf (FilmwebRating, RtRating, McRating)
    decide(before, k("Afrykanska Przygoda", Some(2007)), listed(Nil, Some("African Adventure: Safari in the Okavango")),
      k("Afrykanska Przygoda", Some(2007))) should contain allOf (FilmwebRating, RtRating, McRating)
    // the same facts: nothing to ask again
    decide(before, k("Afrykanska Przygoda", Some(2007)), before, k("Afrykanska Przygoda", Some(2007))) shouldBe empty
    // a row TMDB was not asked about yet is no rating site's to search
    val unasked = rec(tmdbId = None).copy(data = before.data)
    decide(unasked, k("Afrykanska Przygoda", Some(2007)), unasked.copy(data = listed(Seq("Rob Bausch"), None).data),
      k("Afrykanska Przygoda", Some(2007))) should not contain FilmwebRating
  }
}
