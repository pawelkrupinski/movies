package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.{MongoReadModelRepository, ReadModelContentAudit, StoredCard}
import tools.{IntegrationCorpusDatabase, ReadModelSnapshot}

/**
 * The content audit's two reads by `_id`, against a real Mongo and the whole projected fixture
 * corpus — and the audit's comparison over each card as it comes BACK from Mongo.
 *
 * The second half is what makes the audit's alert safe to be strict. The audit compares a fresh
 * projection with the stored document field by field, so any field the codec does not round-trip
 * exactly (a dropped default, a reordered Seq, a precision change) would read as a difference on
 * every card it touches and page for a codec quirk. Written and read back, every card of the
 * corpus must compare EQUAL to what was written.
 */
class ReadModelFindCardIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  "findCard" should "read back every card of the corpus, its screenings and nothing else, and compare equal to what was written" in {
    IntegrationCorpusDatabase.withDatabase(mongoTarget, "readmodel-findcard") { db =>
      val rm       = new MongoReadModelRepository(Some(db))
      val snapshot = ReadModelSnapshot.parse(ReadModelSnapshot.read())
      ReadModelSnapshot.loadInto(rm, ReadModelSnapshot.read())
      val rowsByCard = snapshot.screenings.groupBy(_.filmId)
      snapshot.movies.size should be > 100
      withClue("the corpus must hold a display-title variant card, whose rows sort right after its anchor's: ") {
        snapshot.movies.exists(_._id.contains("~")) shouldBe true
      }

      val wrong = snapshot.movies.flatMap { movie =>
        val expectedRows = rowsByCard.getOrElse(movie._id, Nil)
        rm.findCard(movie._id) match {
          case None => Seq(s"${movie._id}: read failed")
          case Some(StoredCard(stored, rows)) =>
            val shape =
              if (stored.isEmpty) Seq(s"${movie._id}: no document")
              else if (rows.map(_._id).sorted != expectedRows.map(_._id).sorted)
                Seq(s"${movie._id}: rows ${rows.map(_._id).sorted} != ${expectedRows.map(_._id).sorted}")
              else Nil
            shape ++ stored.toSeq.flatMap(doc =>
              ReadModelContentAudit.differences((movie, expectedRows), (doc, rows)).map(f => s"${movie._id}: $f"))
        }
      }
      wrong shouldBe empty
      rm.findCard("__no-such-card__") shouldBe Some(StoredCard(None, Nil))
      rm.close()
    }
  }
}
