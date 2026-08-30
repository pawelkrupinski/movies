package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * A film's poster / film-page / trailer URL — and its TITLE — are the SAME
 * string at every cinema showing it, so the per-cinema slots must share ONE
 * instance rather than each holding a byte-identical copy, exactly as
 * [[StringPool]] already does for synopsis, cast, director, countries and
 * genres.
 *
 * Measured from the 2026-07-27 UK OOM heap dump: 656,202 URL strings occupying
 * 53.1 MB collapsed to 216,850 distinct values (16.9 MB) — a 3x duplication
 * factor worth ~36 MB. It is concentrated in exactly these fields, because a
 * popular film's poster and film page repeat once per cinema slot:
 *   - poster URLs      136,064 occurrences ->  1,896 distinct (71.8x)
 *   - flicks /movie/   138,199 occurrences ->  2,004 distinct (69.0x)
 * whereas `Showtime.bookingUrl` is per-screening and only 1.6x duplicated
 * (182,719 -> 116,571), so it is deliberately NOT interned: it would evict the
 * whole low-cardinality pool for almost no saving.
 *
 * The TITLE fields are the same shape and were missed by that pass. Measured
 * 2026-08-30 over `kinowo_uk.movie_slots` (32,552 cinema slots):
 *   - title          32,552 carriers ->  1,683 distinct (19.3x)
 *   - rawTitle       32,552 carriers ->  1,684 distinct (19.3x)
 *   - originalTitle   3,386 carriers ->    120 distinct (28.2x)
 *   - the slot key's `titleKey`
 *                    32,552 carriers ->  1,594 distinct (20.4x)
 * All four sit far below the pool's cap (the whole interned vocabulary is
 * ~21k entries against a 131,072 limit), so they belong in it on the same
 * low-cardinality grounds as the URLs.
 */
class SlotInterningSpec extends AnyFlatSpec with Matchers {

  private val poster   = "https://d32qys9a6wm9no.cloudfront.net/images/movies/poster/zjZ3UhmU49oNd8WHNCH"
  private val film     = "https://www.flicks.co.uk/movie/spider-man-brand-new-day/"
  private val trailer  = "https://www.youtube.com/watch?v=aBcDeFgHiJk"
  private val title    = "Spider-Man: Brand New Day"
  private val raw      = "SPIDER-MAN: BRAND NEW DAY (2D)"
  private val original = "Spider-Man: Brand New Day"

  /** Fresh (non-interned) instances, as a scraper's parser would produce them —
   *  `new String` defeats the compile-time literal pool the way a real parse does.
   *  Every string a slot retains has to be built this way or the assertions below
   *  pass for the wrong reason: two slots would share the javac constant, not the
   *  pooled instance. */
  private def showing(cinema: Cinema) = CinemaMovie(
    movie     = Movie(
      title         = new String(title),
      releaseYear   = Some(2026),
      originalTitle = Some(new String(original)),
      rawTitle      = Some(new String(raw))
    ),
    cinema    = cinema,
    posterUrl = Some(new String(poster)),
    filmUrl   = Some(new String(film)),
    synopsis  = None,
    cast      = Seq.empty,
    director  = Seq.empty,
    showtimes = Seq(Showtime(LocalDateTime.of(2026, 7, 27, 20, 0), Some("https://book.example/1"))),
    trailerUrl = Some(new String(trailer))
  )

  private def recordForOneFilmAtTwoCinemas(): MovieRecord = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq.empty), normalizer = titleNormalizer)
    val a = OdeonNorwich
    val b = BfiLondonSouthbank
    cache.recordCinemaScrape(a, Seq(showing(a)))
    cache.recordCinemaScrape(b, Seq(showing(b)))
    val record = cache.entries.map(_._2).find(_.data.size >= 2)
    withClue("expected ONE film row carrying a slot per cinema") { record.isDefined shouldBe true }
    record.get
  }

  private def slotsForOneFilmAtTwoCinemas(): Seq[SourceData] =
    recordForOneFilmAtTwoCinemas().data.values.toSeq

  "the cinema slot builder" should "share one poster-URL instance across a film's cinema slots" in {
    val urls = slotsForOneFilmAtTwoCinemas().flatMap(_.posterUrl)
    urls should have size 2
    urls.head shouldBe poster
    (urls.head eq urls(1)) shouldBe true
  }

  it should "share one film-URL instance across a film's cinema slots" in {
    val urls = slotsForOneFilmAtTwoCinemas().flatMap(_.filmUrl)
    urls should have size 2
    urls.head shouldBe film
    (urls.head eq urls(1)) shouldBe true
  }

  it should "share one trailer-URL instance across a film's cinema slots" in {
    val urls = slotsForOneFilmAtTwoCinemas().flatMap(_.trailerUrl)
    urls should have size 2
    urls.head shouldBe trailer
    (urls.head eq urls(1)) shouldBe true
  }

  it should "share one title instance across a film's cinema slots" in {
    val titles = slotsForOneFilmAtTwoCinemas().flatMap(_.title)
    titles should have size 2
    titles.head shouldBe title
    (titles.head eq titles(1)) shouldBe true
  }

  it should "share one rawTitle instance across a film's cinema slots" in {
    val raws = slotsForOneFilmAtTwoCinemas().flatMap(_.rawTitle)
    raws should have size 2
    raws.head shouldBe raw
    (raws.head eq raws(1)) shouldBe true
  }

  it should "share one originalTitle instance across a film's cinema slots" in {
    val originals = slotsForOneFilmAtTwoCinemas().flatMap(_.originalTitle)
    originals should have size 2
    originals.head shouldBe original
    (originals.head eq originals(1)) shouldBe true
  }

  "a decoded cinema slot key" should "share one titleKey instance across a film's cinema slots" in {
    // The Mongo DECODE path, which is where the resident slot keys come from: rehydrate
    // reads the whole corpus and every key is rebuilt by `byWireKey`, whose `substring`
    // yields a fresh instance every time (32,552 of them for 1,594 distinct values in
    // the UK corpus). The scrape path needs no interning — `sanitize` memoises — so this
    // is the only place the saving exists.
    val titleKeys = Seq(OdeonNorwich, BfiLondonSouthbank)
      .map(c => new String(s"${c.displayName}${CinemaShowing.Separator}spidermanbrandnewday"))
      .flatMap(Source.byWireKey)
      .collect { case cs: CinemaShowing => cs.titleKey }
    titleKeys should have size 2
    titleKeys.head shouldBe "spidermanbrandnewday"
    (titleKeys.head eq titleKeys(1)) shouldBe true
  }

  it should "not retain a materialised displayName per slot" in {
    // `displayName` is the Mongo wire spelling, needed only at the storage
    // boundary (`MovieCodecs` encode, `MovieRepository` slot updates) — yet as an
    // eager `val` every resident slot key held its own concatenated copy, and it
    // is ~unique per slot (32,273 distinct across 35,620 keys) so interning it
    // would only burn pool space. Deriving it on demand is what keeps it off the
    // heap, and a fresh instance per call is precisely the evidence of that.
    val key = CinemaShowing.keyFor(OdeonNorwich, new String(title), titleNormalizer)
    key.displayName shouldBe s"${OdeonNorwich.displayName}${CinemaShowing.Separator}${key.titleKey}"
    (key.displayName eq key.displayName) shouldBe false
  }
}
