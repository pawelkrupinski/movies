package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which questions a change wakes: those that read the key before it changed. The question whose own ask filed it has
 *  read what it filed (`StoredFirstLookups`), and one asked after the change read the new value -- neither is asked
 *  again. A change no question filed (a venue page read, a sweep's re-fetch) wakes every reader, as it always did. */
class ObservationReadsSpec extends AnyFlatSpec with Matchers {

  private val (earlier, filer, later) = (CandidateQuery.Title("Before"), CandidateQuery.Title("Filer"), CandidateQuery.Title("After"))
  private def asked(reads: ObservationReads, query: CandidateQuery)(body: => Unit = ()) =
    reads.asking(Question.Candidate(query)) { reads.read("k"); body }

  "a change a question filed" should "wake every earlier reader but its filer, and no later one" in {
    val reads = new ObservationReads
    asked(reads, earlier)()
    var change = Option.empty[ObservationReads.Change]
    asked(reads, filer) { change = Some(reads.changed()) }
    asked(reads, later)()
    reads.changedBy(Map("k" -> change.get)).queries shouldBe Set(earlier)
  }

  // Two filings of one key in one drain window merge into one change (IdentityModelService's queue). Only the newest
  // filer has read what the key now holds: the earlier one read content the later filing replaced, so it is asked
  // again. Excusing both left Odeon's "Catching Fire (2026)" holding a search read from before the fill's record of
  // 101299 replaced it, on Sunrise on the Reaping (Main #2716, HardClusterConvergence's outage pass, 1 run in 8).
  it should "wake, of two filers of one key merged, the one whose filing the other replaced" in {
    val reads = new ObservationReads
    var first, second = Option.empty[ObservationReads.Change]
    asked(reads, earlier) { first = Some(reads.changed()) }
    asked(reads, filer) { second = Some(reads.changed()) }
    reads.changedBy(Map("k" -> first.get.merge(second.get))).queries shouldBe Set(earlier)
    reads.changedBy(Map("k" -> second.get.merge(first.get))).queries shouldBe Set(earlier)
  }

  "a change no question filed" should "wake every question that reads the key" in {
    val reads = new ObservationReads
    asked(reads, earlier)()
    val change = reads.changed()
    asked(reads, later)()
    reads.changedBy(Map("k" -> change)).queries shouldBe Set(earlier, later)
    reads.changedBy(Seq("k")).queries shouldBe Set(earlier, later)
  }

  "a key every question reads" should "be held as one string, whichever copy of it each read named" in {
    val reads = new ObservationReads
    // each store read builds its key afresh: three questions, three equal strings
    Seq(earlier, filer, later).foreach(query => reads.asking(Question.Candidate(query))(reads.read(new String("tmdb_films:603"))))
    reads.keyObjects shouldBe 1
    reads.changedBy(Seq("tmdb_films:603")).queries shouldBe Set(earlier, filer, later)
    reads.forget(Question.Candidate(earlier))
    reads.changedBy(Seq("tmdb_films:603")).queries shouldBe Set(filer, later)
    Seq(filer, later).foreach(query => reads.forget(Question.Candidate(query)))
    reads.keys shouldBe 0
  }
}
