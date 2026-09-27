package tools

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The recording sweep issues EXACTLY the identity resolver's query set — every listing's detail
 * page, every `CandidateQueries` question and the identity record of every film any answer names —
 * because it IS a resolve; and the set is a function of the listing SET, never of arrival order.
 */
class IdentityLookupSweepSpec extends AnyFlatSpec with Matchers {

  private def listing(cinema: Cinema, title: String, year: Option[Int] = None, director: Seq[String] = Nil,
                      page: Option[String] = None): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = year), cinema, None, page, None, Nil, director, Nil)

  private val corpus: Map[Cinema, Seq[CinemaMovie]] = Map(
    // One venue listing two films under one title — the rows `ScrapeListing.prepare` folds.
    Multikino -> Seq(listing(Multikino, "Sinn und Sinnlichkeit", Some(1995), Seq("Ang Lee")),
                     listing(Multikino, "Sinn und Sinnlichkeit", Some(2026), Seq("Georgia Oakley"))),
    Kinoteka  -> Seq(listing(Kinoteka, "Oficjalna premiera: Rozważna i romantyczna", page = Some("https://kinoteka.pl/film/1")),
                     listing(Kinoteka, "Coś", page = Some("https://kinoteka.pl/boom"))),
    Helios    -> Seq(listing(Helios, "Sinn und Sinnlichkeit", Some(1995), Seq("Ang Lee"))))

  /** A source answering every question with a film per distinct string, recording what it was asked. */
  private final class Recording extends IdentityLookups {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    override def hasDetail(l: Listing): Boolean = l.page.isDefined
    override def detail(l: Listing): Answer[Option[DetailFacts]] = {
      asked += s"detail ${l.page.get}"
      if (l.page.exists(_.endsWith("/boom"))) Answer.Unknown else Answer.Known(Some(DetailFacts(Some(1995), Seq("Ang Lee"), Some(136), None)))
    }
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = {
      asked += q.sortKey
      Answer.Known(Seq(Hit(math.abs(q.sortKey.hashCode % 50), q.sortKey.drop(2), None, Some(1995), 1.0)))
    }
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = {
      asked += s"film $id"
      Answer.Known(Some(IdentityMeasures.Film(s"film $id", year = Some(1995))))
    }
  }

  private def sweep(archived: Map[Cinema, Seq[CinemaMovie]]) = {
    val source = new Recording
    val names  = scala.collection.mutable.ArrayBuffer.empty[String]
    val summary = IdentityLookupSweep.run(services.identity.Listing.corpus(archived, titleNormalizer), source, titleNormalizer, names += _)
    (summary, source.asked.toSeq, names.toSeq)
  }

  "the sweep" should "ask exactly the resolver's questions: every listing's CandidateQueries, its detail page, and every named film's record" in {
    val (summary, asked, names) = sweep(corpus)
    val listings = services.identity.Listing.corpus(corpus, titleNormalizer)
    // What the resolver's own definitions say it must ask, derived from the SAME functions.
    val source = new Recording
    val evidences = listings.map(l => Evidence.of(l, if (source.hasDetail(l)) source.detail(l).toOption.flatten else None))
    val queries   = evidences.flatMap(CandidateQueries.of).distinct
    val films     = queries.flatMap(q => source.candidates(q).toOption.getOrElse(Nil)).map(_.tmdbId).distinct

    asked.filter(_.startsWith("detail")).sorted shouldBe Seq("detail https://kinoteka.pl/boom", "detail https://kinoteka.pl/film/1")
    asked.filterNot(a => a.startsWith("detail") || a.startsWith("film")).toSet shouldBe queries.map(_.sortKey).toSet
    asked.filterNot(a => a.startsWith("detail") || a.startsWith("film")).size shouldBe queries.size
    asked.filter(_.startsWith("film")).toSet shouldBe films.map(id => s"film $id").toSet
    // The title searches are the calibration's own shapes, yearless: the banner segment too.
    queries should contain (CandidateQuery.Title("Rozważna i romantyczna"))
    // …and a director's filmography, from the detail page merged under the listing.
    queries should contain (CandidateQuery.Director("Ang Lee"))
    // …and the films IMDb lists under each listing's whole title, a path TMDB's search can miss.
    evidences.map(_.title).distinct.foreach(t => queries should contain (CandidateQuery.Imdb(t)))
    summary.detailsUnanswered shouldBe 1
    names.size shouldBe asked.size
  }

  it should "ask the same questions whatever order the corpus comes in" in {
    val shuffled = scala.collection.immutable.ListMap(corpus.toSeq.reverse.map { case (c, ls) => c -> ls.reverse }*)
    sweep(shuffled)._2.sorted shouldBe sweep(corpus)._2.sorted
    sweep(shuffled)._1 shouldBe sweep(corpus)._1
  }

  private def withTree[A](body: java.nio.file.Path => A): A = {
    val root = java.nio.file.Files.createTempDirectory("identity-sweep-tree")
    try body(root)
    finally {
      Seq(IdentityLookupSweep.RecordedMarker, ".identity-lookups-v2", ".identity-lookups").foreach(m => java.nio.file.Files.deleteIfExists(root.resolve(m)))
      java.nio.file.Files.deleteIfExists(root)
    }
  }

  "a leg" should "run the sweep when asked, or when it replays a tree marked with the lookups its recording asked — never on an older mark" in withTree { root =>
    IdentityLookupSweep.runsIn(requested = false, hermetic = true, root) shouldBe false
    IdentityLookupSweep.runsIn(requested = true, hermetic = false, root) shouldBe true
    // A tree marked before the mark named its lookups cannot say which of today's it answers.
    Seq(".identity-lookups", ".identity-lookups-v2").foreach(m => java.nio.file.Files.writeString(root.resolve(m), "old set\n"))
    IdentityLookupSweep.runsIn(requested = false, hermetic = true, root) shouldBe false
    IdentityLookupSweep.markRecorded(root, sweep(corpus)._3)
    IdentityLookupSweep.runsIn(requested = false, hermetic = true, root) shouldBe true
    // A RECORDING leg is never switched on by the mark: recording the sweep is asked for.
    IdentityLookupSweep.runsIn(requested = false, hermetic = false, root) shouldBe false
  }

  it should "keep every lookup each recording leg over the same tree asked" in withTree { root =>
    IdentityLookupSweep.markRecorded(root, Seq("film 1", "query b"))
    IdentityLookupSweep.markRecorded(root, Seq("film 2", "query b"))
    IdentityLookupSweep.recordedIn(root) shouldBe Some(Set("film 1", "film 2", "query b"))
  }

  // The hermetic legs went red on 2026-09-27 when the resolver learned season queries ("Manon 2026")
  // its pinned tree had never been asked: a replay must issue no request its recording did not.
  "a hermetic replay of a marked tree" should "ask only the lookups its recording asked, and count the resolver's newer ones unasked" in withTree { root =>
    val (_, recordedAsked, recordedNames) = sweep(corpus)
    IdentityLookupSweep.markRecorded(root, recordedNames)
    // The resolver's query set moves after the recording: a listing now brings questions of its own.
    val grown  = corpus.updated(Kinoteka, corpus(Kinoteka) :+ listing(Kinoteka, "RBO Cinema Season 2026-27: Manon"))
    val listed = services.identity.Listing.corpus(grown, titleNormalizer)
    val everything = new Recording
    IdentityLookupSweep.run(listed, everything, titleNormalizer)
    (everything.asked.toSet -- recordedAsked) should not be empty

    val replay  = new Recording
    val summary = IdentityLookupSweep.run(listed, replay, titleNormalizer, recorded = IdentityLookupSweep.recordedIn(root))
    (replay.asked.toSet -- recordedAsked) shouldBe empty
    // …and every lookup it WAS recorded with is still asked: the gate holds for those.
    replay.asked.toSet shouldBe recordedAsked.toSet
    summary.unrecorded should be > 0
    summary.toString should include (s"${summary.unrecorded} lookup(s) the tree was recorded without")
  }
}
