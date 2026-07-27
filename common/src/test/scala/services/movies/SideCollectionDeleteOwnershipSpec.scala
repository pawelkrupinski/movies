package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

/**
 * A film's cinemas live in two side collections keyed by `filmId` — `screenings`
 * (showtimes) and `movie_slots` (the SourceData metadata). Deleting rows from those
 * belongs to `MovieRepository` / the two repositories, and to NOBODY else.
 *
 * The rule exists because the tempting place to break it looks correct. Any code holding a
 * `movies` `_id` that is going away can reach the side collections directly and "tidy up",
 * and that reads as obviously right — it is, after all, what `MovieRepository.delete`
 * does. But a `movies` row disappearing almost never means the film left: it means the
 * film was RE-KEYED (`foo|` collapsing onto `foo|2026` once TMDB concludes the year), and
 * at that instant the film's showtimes are still stored under the OLD id while the
 * winner's rows do not exist yet. `MongoStagingFolder` did exactly this on 2026-07-27 and
 * took prod PL from 39,413 upcoming showtimes to 18,161 and UK from 22,250 to 7,226 within
 * twenty minutes of deploy (@8033e39c6, reverted @926027438).
 *
 * The unit specs could not catch it — the fold needs a replica set — and the integration
 * spec that could was written around the MERGE case and passed throughout. So the guard is
 * structural: it is cheap, it runs in the ordinary unit job, and it fires on the SHAPE of
 * the mistake rather than on one caller's behaviour. A deliberate exception adds itself to
 * [[Owners]] with a reason, which is the review conversation this needs to trigger.
 *
 * Scope is `src/main` — production code. `scripts.ReapOrphanedFilmRows` legitimately
 * deletes orphaned side rows and lives under `worker/src/test/scala/scripts`; it is
 * out-of-band and cannot race a re-key, which is the whole reason it is a script and not
 * a cascade.
 */
class SideCollectionDeleteOwnershipSpec extends AnyFlatSpec with Matchers {

  /** The files allowed to delete from `screenings` / `movie_slots`: the repositories that
   *  own those collections, and `MovieRepository`, which decides a film is gone and
   *  delegates the cascade to them. */
  private val Owners = Set(
    "ScreeningsRepository.scala",
    "SlotsRepository.scala",
    "MovieRepository.scala"
  )

  private val DeleteCalls = Seq("deleteMany(", "deleteOne(", "deleteFilm(", "deleteSlot(")

  /** How a caller actually REACHES a side collection — the collection constants, the
   *  shared per-film predicate, or a direct `getCollection` on the raw name. Deliberately
   *  NOT the bare `"screenings"` literal: that also names the resume-token stream in
   *  `ChangeStreamResumeToken`, which deletes its own token doc and has nothing to do with
   *  a film's rows. Matching the reach rather than the word keeps the guard pointed at the
   *  mistake instead of at the vocabulary. */
  private val SideMentions = Seq(
    "SlotsRepository.Collection", "ScreeningsRepository.Collection", "SlotKeyed.filmFilter",
    "getCollection(\"movie_slots\")", "getCollection(\"screenings\")",
    "getCollection[Document](\"movie_slots\")", "getCollection[Document](\"screenings\")")

  private def mainScalaFiles(dir: File): Seq[File] =
    if (!dir.isDirectory) Seq.empty
    else Option(dir.listFiles()).toSeq.flatten.flatMap { f =>
      if (f.isDirectory) mainScalaFiles(f)
      else if (f.getName.endsWith(".scala")) Seq(f)
      else Seq.empty
    }

  private val modules = Seq("common", "worker", "web").map(m => new File(s"$m/src/main/scala"))

  "deleting a film's side-collection rows" should "stay with the repositories that own them" in {
    // Premise: the scan actually reached the source tree. Without this the spec would pass
    // vacuously from any working directory that isn't the build root.
    val files = modules.flatMap(mainScalaFiles)
    withClue("no production sources found — the scan is looking in the wrong place: ") {
      files.size should be > 100
    }

    val offenders = files.filterNot(f => Owners.contains(f.getName)).flatMap { f =>
      val body = scala.io.Source.fromFile(f).mkString
      val deletes  = DeleteCalls.exists(body.contains)
      val sideRefs = SideMentions.filter(body.contains)
      if (deletes && sideRefs.nonEmpty) Some(f.getPath -> sideRefs) else None
    }

    withClue(
      "these files delete rows AND name a side collection. A `movies` row going away is " +
      "usually a RE-KEY, not a film leaving, and its showtimes are still under the old id — " +
      "cascading a delete there destroys them (prod, 2026-07-27). Either drop the cascade, " +
      "or MIGRATE the rows onto the winning id, or add the file to `Owners` with a reason:\n" +
      offenders.map { case (p, refs) => s"  $p  (${refs.mkString(", ")})" }.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  // The guard is only worth having if it would have fired. Same predicate, run over a
  // reconstruction of the reverted code, so a future refactor of the matching cannot
  // quietly turn the whole spec into a no-op.
  it should "have caught the fold cascade that emptied the boards" in {
    val revertedFoldBody =
      """plan.moviesDeletes.foreach { k =>
        |  val loserId = StoredMovieRecord.idFor(k.cleanTitle, k.year)
        |  await(movies.deleteOne(session, Filters.eq("_id", loserId)).toFuture())
        |  slotsColl.foreach(c      => await(c.deleteMany(session, SlotKeyed.filmFilter(loserId)).toFuture()))
        |  screeningsColl.foreach(c => await(c.deleteMany(session, SlotKeyed.filmFilter(loserId)).toFuture()))
        |}""".stripMargin

    DeleteCalls.exists(revertedFoldBody.contains)  shouldBe true
    SideMentions.exists(revertedFoldBody.contains) shouldBe true
  }
}
