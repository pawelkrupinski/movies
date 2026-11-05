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
      val rm       = new MongoReadModelRepository(Some(db), stopwatch = _root_.tools.Stopwatch.System)
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
          case tools.ReadOutcome.Failed(cause) => Seq(s"${movie._id}: read failed: ${cause.explain}")
          case tools.ReadOutcome.Absent(reason) => Seq(s"${movie._id}: read absent: ${reason.explain}")
          case tools.ReadOutcome.Answered(StoredCard(stored, rows)) =>
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
      rm.findCard("__no-such-card__") shouldBe tools.ReadOutcome.Answered(StoredCard(None, Nil))
      rm.close()
    }
  }

  // A Mongo that never answers (nothing listens on port 1; the driver keeps selecting a server for a minute), so the read
  // cannot finish inside its timeout: over the suite's own fast mongod a 1 ns timeout raced the reply, and a loaded
  // runner's Await found the read already answered (precommit-check run 42005125331, "no such card").
  it should "answer an unread outcome carrying its cause, when a read runs out of time -- never the fact that there is no such card" in {
    val silent = org.mongodb.scala.MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=60000")
    try {
      val rm = new MongoReadModelRepository(Some(silent.getDatabase(tools.RunScopedDatabaseName.fresh("kinowo_it_findcard_timeout"))), stopwatch = _root_.tools.Stopwatch.System,
        findCardMovieTimeout = scala.concurrent.duration.Duration.fromNanos(1))
      val outcome = rm.findCard("__no-such-card__")
      withClue(outcome.explain + ": ") { outcome.isFailed shouldBe true }
    } finally silent.close()
  }
}
