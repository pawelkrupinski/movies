package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.SliceHttp

import java.nio.file.{Files, Path}

/** SLICE ANSWER COVERAGE (scripts/identity-slice-coverage): a pinned slice whose code asks a search its fixture does not
 *  hold is named with that search -- every one, all slices at once -- and `--repin` tops the slice up from its recording
 *  until it lacks nothing. Unmatched C's banner shapes asked new searches two pinned cases lacked, found one push at a time
 *  (main #2180: Halstead, then Keswick). */
class IdentitySliceCoverageSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {

  /** The smallest pinned case, copied where the spec may change it. */
  private def copyOfACase(): Path = {
    val cases = IdentitySliceCoverage.slicesUnder(Seq(IdentitySliceCoverage.Roots.head)).filter(d => Files.exists(d.resolve("expected.tsv")))
    val small = cases.minBy(d => Files.size(d.resolve("http.jsonl.gz")))
    val copy  = tempDirectory("slice-coverage").resolve(small.getFileName.toString)
    Files.createDirectories(copy)
    Files.list(small).forEach(f => Files.copy(f, copy.resolve(f.getFileName)))
    copy
  }

  /** Every request `slice` asks at this code, with the outcome its fixture holds: decided from no answer at all, the fixture
   *  standing in for its recording. */
  private def askedBy(slice: IdentitySlice.Slice): Map[SliceHttp.Request, SliceHttp.Outcome] =
    IdentitySlice.decidePinned(slice.copy(http = Map.empty), Some(fromNotes(slice.http)(slice)))._3.widened

  /** A recording holding `notes`: what a re-pin reads the lacking answers from. */
  private def fromNotes(notes: Map[SliceHttp.Request, SliceHttp.Outcome]): IdentitySliceCoverage.RecordingOf = _ => {
    val recording = new SliceHttp.Replaying(notes)
    new SliceHttp.Noting(recording, () => recording.metOnThread)
  }

  "the slice answer coverage" should "name every search a case asks that its fixture lacks, and --repin should pin them from its recording" in {
    val dir      = copyOfACase()
    val pinned   = IdentitySlice.readSlice(dir)
    IdentitySliceCoverage.coverageOf(dir).covered shouldBe true
    val asked    = askedBy(pinned)
    // the head asks searches the case was never pinned with: here, its fixture without two of the searches it asks
    val searches = asked.keys.filter(r => r.method == "GET" && r.url.toLowerCase.contains("search")).toSeq.sortBy(_.render).take(2)
    searches should not be empty
    SliceHttp.write(dir.resolve("http.jsonl.gz"), pinned.http -- searches)

    val found = IdentitySliceCoverage.coverageOf(dir)
    found.covered shouldBe false
    found.lacking should contain allElementsOf searches.map(_.render)
    val report = IdentitySliceCoverage.report(Seq(found)).mkString("\n")
    report should include(s"$dir (recording ")
    searches.foreach(s => report should include(s.render))

    IdentitySliceCoverage.repin(dir, fromNotes(pinned.http)) shouldBe empty
    IdentitySliceCoverage.coverageOf(dir).covered shouldBe true
    IdentitySlice.readSlice(dir).http.keySet should contain allElementsOf searches
    // only the answers moved: the case's expected takes and listings as pinned
    Files.readString(dir.resolve("expected.tsv")) shouldBe Files.readString(IdentitySliceCoverage.Roots.head.resolve(dir.getFileName).resolve("expected.tsv"))
    IdentitySlice.readSlice(dir).core.map(_.key) shouldBe pinned.core.map(_.key)
  }

  "a slice's recording" should "be named by its cache key wherever the cache was" in {
    IdentitySliceCoverage.recordingKey("/Users/x/.cache/kinowo-identity/recordings/38713274617/pair/test/resources/fixtures") shouldBe Some("38713274617")
    IdentitySliceCoverage.recordingKey("/home/runner/cache/recordings/573-fills-1403118718/pair/test/resources/fixtures/") shouldBe Some("573-fills-1403118718")
    IdentitySliceCoverage.recordingKey("/tmp/some/other/dir") shouldBe None
  }
}
