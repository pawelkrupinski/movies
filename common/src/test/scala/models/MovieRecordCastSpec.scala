package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.cast` / `director` — the longest (resp. highest-priority) list
 * decides WHICH names are shown, but TMDB decides how the ones it knows are
 * SPELLED.
 *
 * The pairing matters: TMDB caps its credits at a top-N, so a scraped nine-name
 * list wins the length contest against TMDB's five, and before this the display
 * showed the scraper's `Leonardo Dicaprio` even though the TMDB slot on the same
 * record carried `Leonardo DiCaprio`. `tools.PersonName` cannot fix that — no
 * rule over the letters alone knows about a capital inside a word.
 */
class MovieRecordCastSpec extends AnyFlatSpec with Matchers {

  /** What a scraper that ships lowercase names leaves behind after
   *  `PersonName` has cased it at the parse boundary: word-starts right,
   *  internal capitals lost. */
  private val scrapedCast = Seq(
    "Leonardo Dicaprio", "Sean Penn", "Danny Devito", "Frances McDormand", "Shia Labeouf",
    "Seth Macfarlane", "Jan Kowalski", "Anna Nowak", "Kate Winslet"
  )

  /** TMDB's top-N credits — shorter, and correctly spelled. */
  private val tmdbCast = Seq(
    "Leonardo DiCaprio", "Kate Winslet", "Danny DeVito", "Shia LaBeouf", "Seth MacFarlane"
  )

  "cast" should "keep the longest list but take TMDB's spelling for the names it knows" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(cast = scrapedCast),
        Tmdb      -> SourceData(cast = tmdbCast)
      )
    )
    record.cast shouldBe Seq(
      "Leonardo DiCaprio", "Sean Penn", "Danny DeVito", "Frances McDormand", "Shia LaBeouf",
      "Seth MacFarlane", "Jan Kowalski", "Anna Nowak", "Kate Winslet"
    )
    record.cast should have size scrapedCast.size
  }

  it should "leave the list untouched when the record has no TMDB slot" in {
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(cast = scrapedCast)))
    record.cast shouldBe scrapedCast
  }

  it should "return a list that already agrees with TMDB byte-identical" in {
    val agreed = Seq("Kate Winslet", "Danny DeVito", "Sean Penn")
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(cast = agreed),
        Tmdb      -> SourceData(cast = Seq("Danny DeVito", "Kate Winslet"))
      )
    )
    record.cast shouldBe agreed
  }

  it should "keep both entries when a source lists one TMDB name twice" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(cast = Seq("Danny Devito", "Sean Penn", "danny devito")),
        Tmdb      -> SourceData(cast = Seq("Danny DeVito"))
      )
    )
    record.cast shouldBe Seq("Danny DeVito", "Sean Penn", "Danny DeVito")
    record.cast should have size 3
  }

  it should "still pick the longest list, not TMDB's, when TMDB's is shorter" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(cast = scrapedCast),
        Tmdb      -> SourceData(cast = tmdbCast)
      )
    )
    record.cast should have size 9
    record.cast.head shouldBe "Leonardo DiCaprio"
  }

  "director" should "take TMDB's spelling for a name the winning cinema list shares" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(director = Seq("Cecil B. Demille", "Jan Kowalski")),
        Tmdb      -> SourceData(director = Seq("Cecil B. DeMille"))
      )
    )
    record.director shouldBe Seq("Cecil B. DeMille", "Jan Kowalski")
  }

  it should "leave the director list untouched when the record has no TMDB slot" in {
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(director = Seq("Cecil B. Demille"))))
    record.director shouldBe Seq("Cecil B. Demille")
  }
}
