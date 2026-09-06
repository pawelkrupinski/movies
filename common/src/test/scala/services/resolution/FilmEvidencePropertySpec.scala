package services.resolution

import models.Source
import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators._

/** `FilmEvidence.of` is a pure function of the CINEMA slots, whatever order
 *  they sit in the record's map. */
class FilmEvidencePropertySpec extends IdentityPropertySpec {

  "FilmEvidence.of" should "not depend on the order of the record's data entries" in {
    forAll(genMovieRecord(maxSlots = 6).flatMap(r => withPermutation(Gen.const(r.data.toSeq)).map(r -> _))) {
      case (record, (_, permuted)) =>
        // Up to four entries a `Map` keeps insertion order, so this is a real reorder.
        FilmEvidence.of(record.copy(data = permuted.toMap)) shouldBe FilmEvidence.of(record)
    }
  }

  it should "read nothing from a Tmdb / Imdb / Filmweb slot" in {
    forAll(genMovieRecord(maxSlots = 6)) { record =>
      val cinemaOnly = record.copy(data = record.data.filter { case (source, _) => Source.cinemaOf(source).isDefined })
      FilmEvidence.of(record) shouldBe FilmEvidence.of(cinemaOnly)
    }
  }

  it should "report exactly the titles the cinema slots carry" in {
    forAll(genMovieRecord(maxSlots = 6)) { record =>
      FilmEvidence.of(record).titles shouldBe record.cinemaSlots.flatMap(_._2.title).toSet
    }
  }
}
