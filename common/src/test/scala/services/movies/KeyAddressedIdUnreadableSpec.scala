package services.movies

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A title-addressed upsert with no row under its key creates one under the key's legacy
 *  id, or a fresh one — whichever no document holds. An unreadable answer to "does one?"
 *  used to count as "no", so the write could replace the film holding that id. */
class KeyAddressedIdUnreadableSpec extends AnyFlatSpec with Matchers {

  "A title-addressed upsert" should "decline, not write, when it cannot read whether its id is taken" in {
    val repo = new StoredRowsRepository(Seq.empty) {
      override def findByIdChecked(id: FilmId): (Option[StoredMovieRecord], Boolean) = (None, false)
    }
    repo.upsert("Kumotry", Some(2026), MovieRecord(tmdbId = Some(7))) shouldBe WriteOutcome.Declined("id-unreadable")
    repo.upserts shouldBe empty
  }

  "MovieRepository.holdsId" should "throw on an unreadable row, never answer 'free'" in {
    val repo = new StoredRowsRepository(Seq.empty) {
      override def findByIdChecked(id: FilmId): (Option[StoredMovieRecord], Boolean) = (None, false)
    }
    an[IllegalStateException] should be thrownBy repo.holdsId(FilmId("f1"))
    new StoredRowsRepository(Seq.empty).holdsId(FilmId("f1")) shouldBe false
  }

  // An absent key is a delete already done; a key that could not be read is not — reported as
  // Written, the caller took a row it never looked for as gone.
  "A title-addressed delete" should "decline, not report a delete, when it cannot read the key" in {
    val repo = new StoredRowsRepository(Seq.empty) {
      override def findByKeyChecked(key: CacheKey): (Option[StoredMovieRecord], Boolean) = (None, false)
    }
    repo.delete("Kumotry", Some(2026)) shouldBe WriteOutcome.Declined("key-unreadable")
    new StoredRowsRepository(Seq.empty).delete("Kumotry", Some(2026)) shouldBe WriteOutcome.Written
  }
}
