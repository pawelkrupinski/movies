package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._

import java.nio.file.Files

/** A capture taken without the casts its candidates' evidence asked (through a [[MemoLookups]] that passed none on) or the
 *  records' original languages, filled from a recording's TMDB tree ([[UnmatchedClusters.withRecorded]]), and both kept
 *  through the capture's file and read back by its replay as the store answers them. */
class UnmatchedClustersRecordedSpec extends AnyFlatSpec with Matchers {

  private val pl = Country.all.find(_.code == "pl").get
  // Kino CK Lublin's "Lalka": Kawalski's 2026 film and Has's 1968 one
  private val kawalski = 1321666
  private val has      = 85008
  private val unknown  = 4242
  private val castOf   = Map(kawalski -> Seq("Marcin Dorociński", "Kamila Urzędowska", "Andrzej Seweryn"), has -> Seq("Mariusz Dmochowski", "Beata Tyszkiewicz"))
  private val languages = Map(kawalski -> "pl", has -> "pl")
  private val taken = UnmatchedClusters.Capture(pl, Nil, Nil, Map.empty,
    films = Map(kawalski -> Some(IdentityMeasures.Film("Lalka", year = Some(2026))), has -> Some(IdentityMeasures.Film("Lalka", year = Some(1968))),
      unknown -> None),
    details = Map.empty, withDetail = Set.empty, families = Map.empty, finds = Map.empty,
    casts = Map(kawalski -> None, has -> None))

  "a capture taken without casts or original languages" should "gain them from the recording, and keep them through its file" in {
    val filled = UnmatchedClusters.withRecorded(taken, Seq(unknown), castOf.get, languages.get)
    filled.casts shouldBe Map(kawalski -> Some(castOf(kawalski)), has -> Some(castOf(has)))
    filled.films(kawalski).flatMap(_.originalLanguage) shouldBe Some("pl")
    filled.films(unknown) shouldBe None

    val file = Files.createTempFile("capture", ".json.gz")
    try {
      UnmatchedClusters.write(file, filled)
      val replay = new UnmatchedClusters.Replay(UnmatchedClusters.read(file))
      replay.cast(kawalski) shouldBe Answer.Known(Some(castOf(kawalski)))
      replay.film(has).toOption.flatten.flatMap(_.originalLanguage) shouldBe Some("pl")
    } finally Files.deleteIfExists(file)
  }

  "its replay" should "note a cast it was asked and holds none of, for a fill to answer" in {
    val replay = new UnmatchedClusters.Replay(taken)
    replay.cast(unknown) shouldBe Answer.Known(None)
    replay.cast(kawalski) shouldBe Answer.Known(None)
    replay.casts.contains(unknown) shouldBe true
    replay.casts.contains(kawalski) shouldBe false
  }
}
