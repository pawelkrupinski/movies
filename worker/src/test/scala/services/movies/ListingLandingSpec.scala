package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/** The landing decision on its own — an index of resolved rows and a listing, no cache. */
class ListingLandingSpec extends AnyFlatSpec with Matchers {

  private def resolved(title: String, tmdbId: Int, original: String = "", year: Int = 2026, runtime: Option[Int] = None): MovieRecord =
    MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some(title), originalTitle = Some(if (original.isEmpty) title else original), releaseYear = Some(year), runtimeMinutes = runtime),
      (KinoMuza: Source) -> SourceData(title = Some(title), originalTitle = Some(if (original.isEmpty) title else original), releaseYear = Some(year), runtimeMinutes = runtime)))

  private def corpus(rows: (String, MovieRecord)*): (CorpusIndex, Map[CacheKey, MovieRecord]) = {
    val index = new CorpusIndex(titleNormalizer, (k, r) => r.tmdbConcluded && FilmCanonicalizer.isBareFilmTitle((k, r), titleNormalizer))
    val byKey = rows.map { case (t, r) => CacheKey(t, Some(2026), titleNormalizer) -> r }.toMap
    byKey.foreach { case (k, r) => index.put(k, r, FilmId.legacy(k)) }
    (index, byKey)
  }

  private def listing(title: String, cinema: Cinema = Helios, year: Option[Int] = None, original: Option[String] = None,
                      runtime: Option[Int] = None, director: Seq[String] = Nil) =
    ListingLanding.Listing(title, cinema, original, runtime, year, director)

  private def ask(index: CorpusIndex, rows: Map[CacheKey, MovieRecord], l: ListingLanding.Listing) =
    ListingLanding.ask(index, rows.get, l, titleNormalizer, diverting = true)

  "a listing keyed by a known title" should "not divert" in {
    val (index, rows) = corpus("Vaiana" -> resolved("Vaiana", 1))
    val a = ask(index, rows, listing("Vaiana"))
    a.holdsTitle shouldBe true
    a.divert(diverting = true) shouldBe false
    a.fallbackKey shouldBe None
  }

  "a banner variant of a known film" should "land on the film through decoration" in {
    val (index, rows) = corpus("Toy Story" -> resolved("Toy Story", 862))
    val a = ask(index, rows, listing("Toddler Club: Toy Story"))
    a.decorationOf shouldBe Set(CacheKey("Toy Story", Some(2026), titleNormalizer))
    a.divert(diverting = true) shouldBe false
    a.fallbackKey shouldBe Some(CacheKey("Toy Story", Some(2026), titleNormalizer))
  }

  it should "never treat a listing as a decoration of a ONE-word film" in {
    // The veto compares words of four letters and more, and a one-word title has none
    // to compare — whatever evidence the listing carries, "It Ends With Us" is not a
    // screening of "It". It resolves on its own.
    val (index, rows) = corpus("It" -> resolved("It", 346364, year = 2017))
    ask(index, rows, listing("It Ends With Us")).decorationOf shouldBe empty
    ask(index, rows, listing("It Ends With Us")).divert(diverting = true) shouldBe true
    ask(index, rows, listing("It Ends With Us", original = Some("It Ends with Us"), year = Some(2024), runtime = Some(130))).decorationOf shouldBe empty
  }

  it should "refuse when the venue describes a different film" in {
    // The veto: a differing original title (on the words of four letters and more),
    // no shared director, corroborated by runtime and year.
    val (index, rows) = corpus("Toy Story" -> resolved("Toy Story", 862, runtime = Some(100)))
    val a = ask(index, rows, listing("Toddler Club: Toy Story", original = Some("Alien Romulus"), runtime = Some(150), year = Some(2011)))
    a.decorationOf shouldBe empty
    a.divert(diverting = true) shouldBe true
  }

  "a Cyrillic listing" should "land on the Latin row it romanises to" in {
    val (index, rows) = corpus("Vaiana" -> resolved("Vaiana", 1))
    val a = ask(index, rows, listing("Ваяна"))
    a.sameSearchAs shouldBe Set(CacheKey("Vaiana", Some(2026), titleNormalizer))
    a.divert(diverting = true) shouldBe false
  }

  "a listing whose title matches two different films' decorations" should "refuse the ambiguity and divert" in {
    val (index, rows) = corpus("Dune" -> resolved("Dune", 438631, year = 2021), "Dune Two" -> resolved("Dune Two", 693134, year = 2024))
    val a = ask(index, rows, listing("IMAX: Dune Two Dune"))
    a.decorationOf shouldBe empty
  }

  "a same-titled row that this venue describes as another film" should "divert" in {
    val (index, rows) = corpus("Joanna d'Arc" -> resolved("Joanna d'Arc", 1, original = "Jeanne d'Arc", year = 1999, runtime = Some(158)))
    val a = ask(index, rows, listing("Joanna d'Arc", original = Some("Maid Warrior"), year = Some(2025), runtime = Some(90)))
    a.aDifferentFilm shouldBe true
    a.divert(diverting = true) shouldBe true
    a.divert(diverting = false) shouldBe false     // nowhere to divert to: the cache lands it regardless
  }
}
