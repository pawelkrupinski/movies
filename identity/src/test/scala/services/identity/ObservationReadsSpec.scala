package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which questions a change wakes: those that read the key before it changed. The question whose own ask filed it has
 *  read what it filed (`StoredFirstLookups`), and one asked after the change read the new value -- neither is asked
 *  again. A change no question filed (a venue page read, a sweep's re-fetch) wakes every reader, as it always did. */
class ObservationReadsSpec extends AnyFlatSpec with Matchers {

  private val (earlier, filer, later) = (CandidateQuery.Title("Before"), CandidateQuery.Title("Filer"), CandidateQuery.Title("After"))
  private def asked(reads: ObservationReads, query: CandidateQuery)(body: => Unit = ()) =
    reads.asking(ObservationReads.Question.Query(query)) { reads.read("k"); body }

  "a change a question filed" should "wake every earlier reader but its filer, and no later one" in {
    val reads = new ObservationReads
    asked(reads, earlier)()
    var change = Option.empty[ObservationReads.Change]
    asked(reads, filer) { change = Some(reads.changed()) }
    asked(reads, later)()
    reads.changedBy(Map("k" -> change.get)).queries shouldBe Set(earlier)
  }

  it should "wake two filers of one key neither, merged" in {
    val reads = new ObservationReads
    var first, second = Option.empty[ObservationReads.Change]
    asked(reads, earlier) { first = Some(reads.changed()) }
    asked(reads, filer) { second = Some(reads.changed()) }
    reads.changedBy(Map("k" -> first.get.merge(second.get))).queries shouldBe empty
  }

  "a change no question filed" should "wake every question that reads the key" in {
    val reads = new ObservationReads
    asked(reads, earlier)()
    val change = reads.changed()
    asked(reads, later)()
    reads.changedBy(Map("k" -> change)).queries shouldBe Set(earlier, later)
    reads.changedBy(Seq("k")).queries shouldBe Set(earlier, later)
  }
}
