package tools

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The recording sweep issues the identity resolver's query set: a function of the listing SET,
 * never of the order the corpus arrives in, with every listing's own evidence resolved — the
 * rows the per-title fold would erase included.
 */
class IdentityLookupSweepSpec extends AnyFlatSpec with Matchers {

  private def listing(cinema: Cinema, title: String, year: Option[Int] = None, director: Seq[String] = Nil,
                      page: Option[String] = None): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = year), cinema, None, page, None, Nil, director, Nil)

  private final class CountingEnricher(val cinema: Cinema) extends DetailEnricher {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    override def detailGroup: String = cinema.displayName
    override def fetchFilmDetail(ref: String): Option[FilmDetail] = {
      asked += ref
      if (ref.endsWith("/boom")) throw new RuntimeException("upstream down")
      Some(FilmDetail(releaseYear = Some(1995), director = Seq("Ang Lee"), runtimeMinutes = Some(136)))
    }
  }

  private val corpus: Map[Cinema, Seq[CinemaMovie]] = Map(
    // One venue listing two films under one title — the rows `ScrapeListing.prepare` folds.
    Multikino -> Seq(listing(Multikino, "Sinn und Sinnlichkeit", Some(1995), Seq("Ang Lee")),
                     listing(Multikino, "Sinn und Sinnlichkeit", Some(2026), Seq("Georgia Oakley")),
                     listing(Multikino, "Sinn und Sinnlichkeit", Some(2026), Seq("Georgia Oakley"))),
    // A page at a venue with an enricher: its detail fills the year and director.
    Kinoteka  -> Seq(listing(Kinoteka, "Rozważna i romantyczna", page = Some("https://kinoteka.pl/film/1")),
                     listing(Kinoteka, "Rozważna i romantyczna 2D", page = Some("https://kinoteka.pl/film/1")),
                     listing(Kinoteka, "Coś", page = Some("https://kinoteka.pl/boom"))),
    // Same evidence as Multikino's 1995 listing, another venue: ONE question.
    Helios    -> Seq(listing(Helios, "Sinn und Sinnlichkeit", Some(1995), Seq("Ang Lee"))))

  private def sweep(archived: Map[Cinema, Seq[CinemaMovie]]) = {
    val enricher = new CountingEnricher(Kinoteka)
    val resolved = scala.collection.mutable.ArrayBuffer.empty[(String, Option[Int], Set[Source])]
    val summary  = IdentityLookupSweep.run(archived, Seq(enricher), (title, year, record) => {
      resolved += ((title, year, record.data.keySet))
      if (year.contains(1995)) Some(record.copy(tmdbId = Some(4584))) else None
    }, slotCinema = CinemaCity, titleNormalizer)
    (summary, enricher.asked.toSeq, resolved.toSeq)
  }

  "the sweep" should "ask every listing's own evidence once, the same questions whatever order the corpus comes in" in {
    val (summary, details, resolves) = sweep(corpus)
    val shuffled = scala.collection.immutable.ListMap(corpus.toSeq.reverse.map { case (c, ls) => c -> ls.reverse }*)
    sweep(shuffled) shouldBe ((summary, details, resolves))

    resolves.map { case (t, y, _) => (t, y) } should contain allOf (("Sinn und Sinnlichkeit", Some(1995)), ("Sinn und Sinnlichkeit", Some(2026)))
    resolves.count(_._1 == "Sinn und Sinnlichkeit") shouldBe 2
    summary shouldBe IdentityLookupSweep.Summary(listings = 7 - 1, detailLookups = 2, detailFailures = 1, resolves = 5,
      resolveFailures = 0, resolved = 3)
  }

  it should "fetch each page once, and merge what it says under what the listing says" in {
    val (_, details, resolves) = sweep(corpus)
    details.sorted shouldBe Seq("https://kinoteka.pl/boom", "https://kinoteka.pl/film/1")
    resolves.collect { case (t, y, _) if t.startsWith("Rozważna") => y }.distinct shouldBe Seq(Some(1995))
  }

  it should "put every resolve's slot on the one fixed venue, so an answer never depends on which venue listed it" in {
    val (_, _, resolves) = sweep(corpus)
    resolves.flatMap(_._3).flatMap(Source.cinemaOf).distinct shouldBe Seq(CinemaCity)
  }

  "a leg" should "run the sweep when asked, or when it replays a tree recorded with it — never on an unmarked tree unasked" in {
    val root = java.nio.file.Files.createTempDirectory("identity-sweep-tree")
    try {
      IdentityLookupSweep.runsIn(requested = false, hermetic = true, root) shouldBe false
      IdentityLookupSweep.runsIn(requested = true, hermetic = false, root) shouldBe true
      IdentityLookupSweep.markRecorded(root)
      IdentityLookupSweep.runsIn(requested = false, hermetic = true, root) shouldBe true
      // A RECORDING leg is never switched on by the mark: recording the sweep is asked for.
      IdentityLookupSweep.runsIn(requested = false, hermetic = false, root) shouldBe false
    } finally {
      java.nio.file.Files.deleteIfExists(root.resolve(IdentityLookupSweep.RecordedMarker))
      java.nio.file.Files.deleteIfExists(root)
    }
  }
}
