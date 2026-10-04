package controllers

import play.api.Logging
import services.MirrorFreshness

import java.io.{InputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamClass}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.{Clock, Duration => JDuration, Instant}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try, Using}

/** One `/debug` read: the value, the newest `updatedAt` the mirror held when it was
 *  read, and WHEN it was read — `None` for a read made for this very request. The
 *  navbar renders the mirror's lag as of the read, and, for a snapshot old enough to
 *  matter, how old it is. */
final case class DebugSnapshot[A](value: A, mirrorNewest: Option[Instant], takenAt: Option[Instant] = None)

object DebugSnapshot {
  /** Read it now, on the caller's thread — a stack whose read is cheap enough to do per
   *  request (an in-memory model, every spec's in-memory repository). */
  def readNow[A](freshness: MirrorFreshness)(read: => A): () => DebugSnapshot[A] =
    () => DebugSnapshot(read, freshness.newestUpdate())
}

/** Where a [[RefreshingSnapshot]] keeps its last read, so a restarted or dev-reloaded
 *  app can serve it at once instead of starting cold. */
trait SnapshotStore {
  def load[A](key: String): Option[DebugSnapshot[A]]
  def save[A](key: String, snapshot: DebugSnapshot[A]): Unit
}

object SnapshotStore {
  /** Keeps nothing — prod, where /debug 404s, and every spec. */
  val none: SnapshotStore = new SnapshotStore {
    def load[A](key: String): Option[DebugSnapshot[A]] = None
    def save[A](key: String, snapshot: DebugSnapshot[A]): Unit = ()
  }
}

/** Java-serialised snapshots under `dir`, one file per key. A file that no longer
 *  decodes — the classes changed since it was written — is skipped, and the next
 *  read overwrites it: this is a cache, never a source of truth. */
final class FileSnapshotStore(dir: Path) extends SnapshotStore with Logging {

  def load[A](key: String): Option[DebugSnapshot[A]] = {
    val file = fileFor(key)
    if (!Files.exists(file)) None
    else Try(Using.resource(new ClassLoaderObjectInputStream(Files.newInputStream(file), getClass.getClassLoader))(
      _.readObject().asInstanceOf[DebugSnapshot[A]])) match {
      case Success(snapshot)  => Some(snapshot)
      case Failure(exception) =>
        logger.info(s"$key: stored snapshot unreadable, reading fresh: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
        None
    }
  }

  def save[A](key: String, snapshot: DebugSnapshot[A]): Unit =
    Try {
      Files.createDirectories(dir)
      val file = fileFor(key)
      val tmp  = Files.createTempFile(dir, file.getFileName.toString, ".tmp")
      Using.resource(new ObjectOutputStream(Files.newOutputStream(tmp)))(_.writeObject(snapshot))
      Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    }.failed.foreach(exception =>
      logger.warn(s"$key: could not store snapshot: ${exception.getClass.getSimpleName}: ${exception.getMessage}"))

  private def fileFor(key: String): Path = dir.resolve(key.replaceAll("[^A-Za-z0-9]+", "-").stripPrefix("-") + ".ser")

  /** Resolves classes through the APP's loader: under Play dev mode the app lives in
   *  its own reloadable classloader, which the default (caller's latest user-defined)
   *  lookup can miss. */
  private final class ClassLoaderObjectInputStream(in: InputStream, loader: ClassLoader) extends ObjectInputStream(in) {
    override def resolveClass(desc: ObjectStreamClass): Class[?] =
      Try(Class.forName(desc.getName, false, loader)).getOrElse(super.resolveClass(desc))
  }
}

/**
 * A whole-collection `/debug` read served from memory instead of re-read per request.
 *
 * Why: the corpus listing stitches every film's `movie_slots` back in, and that read
 * scales with the corpus, not the page. Measured against the LOCAL mirror
 * (2026-10-03): US is 105k slots (~1.3 s for the scan alone) and 100k read-model
 * screenings (~0.8 s just to count them server-side), so every country switch cost
 * 4–10 s even with no network hop. A snapshot turns a switch into a map lookup.
 *
 * `get()` answers from the last snapshot at once; when that is older than
 * `refreshAfter` it also starts ONE background re-read (never two at once), so the
 * next load is current. [[refreshIfOlderThan]] is the periodic warm-up's entry point,
 * which keeps every country's snapshot within a minute or so of the mirror whether
 * or not anyone is looking at it.
 *
 * Every read is also written to `store`, and the first `get()` of a fresh app starts
 * from what is stored there — so neither a dev reload nor a server restart makes the
 * first load of a country wait on a cold read. Only when nothing is stored does that
 * first `get()` wait. A restored snapshot is shown with its age (the navbar's
 * "snapshot … old" chip) and re-read at once.
 *
 * A failed background re-read keeps the previous snapshot and logs. A failed FIRST
 * read throws, so the page shows the error instead of an empty table that would
 * read as an empty corpus.
 */
final class RefreshingSnapshot[A](
  label:        String,
  read:         () => A,
  freshness:    MirrorFreshness,
  refreshAfter: FiniteDuration,
  clock:        Clock,
  store:        SnapshotStore = SnapshotStore.none,
)(using ec: ExecutionContext) extends Logging {

  private val current  = new AtomicReference[Option[DebugSnapshot[A]]](None)
  private val inFlight = new AtomicReference[Option[Future[DebugSnapshot[A]]]](None)
  private val restored = new AtomicBoolean(false)

  def get(): DebugSnapshot[A] = {
    restore()
    current.get() match {
      case Some(snapshot) =>
        if (olderThan(snapshot, refreshAfter)) refresh()
        snapshot
      // The 70 s sits above the reads' own 60 s timeouts, so an inner timeout fires (and logs) first.
      case None => Await.result(refresh(), 70.seconds)
    }
  }

  /** Start a re-read unless the snapshot is younger than `threshold`; completes when
   *  that re-read does (at once when none was needed), so a caller warming several
   *  snapshots can do them one at a time rather than all contending at once. */
  def refreshIfOlderThan(threshold: FiniteDuration): Future[Unit] = {
    restore()
    if (current.get().forall(olderThan(_, threshold))) refresh().map(_ => ()).recover { case _ => () }
    else Future.unit
  }

  private def restore(): Unit =
    if (restored.compareAndSet(false, true))
      store.load[A](label).foreach(snapshot => current.compareAndSet(None, Some(snapshot)))

  private def olderThan(snapshot: DebugSnapshot[A], threshold: FiniteDuration): Boolean =
    snapshot.takenAt.forall(at => JDuration.between(at, clock.instant()).toMillis >= threshold.toMillis)

  // When the snapshot last stored was taken. The in-flight marker is cleared before a read's save
  // callback runs, so the next read can finish and save first; without this the older save landed
  // last and a restart came up on it.
  private var lastSaved: Option[Instant] = None

  private def saveIfNewest(snapshot: DebugSnapshot[A]): Unit = synchronized {
    if (lastSaved.forall(saved => snapshot.takenAt.exists(_.isAfter(saved)))) {
      store.save(label, snapshot)
      lastSaved = snapshot.takenAt
    }
  }

  private def refresh(): Future[DebugSnapshot[A]] = {
    val promise = Promise[DebugSnapshot[A]]()
    val marker  = Some(promise.future)
    if (inFlight.compareAndSet(None, marker)) {
      promise.completeWith(Future {
        // Over before the future completes, on the read's own thread: a callback queued after
        // completion left a window in which a stale ask joined the FINISHED read and started none.
        try {
          // Read the mirror's newest stamp and the clock BEFORE the data: the badge may
          // then overstate the data's age by the read's duration, but never understate it.
          val newest   = freshness.newestUpdate()
          val started  = clock.instant()
          val snapshot = DebugSnapshot(read(), newest, Some(started))
          // Published before the future completes, so a caller woken by it already sees it.
          current.set(Some(snapshot))
          logger.info(s"$label: read in ${JDuration.between(started, clock.instant()).toMillis} ms")
          snapshot
        } finally inFlight.set(None)
      })
      // A read the executor refused never ran the body that clears the marker: cleared here too, or
      // every later ask joined that failed read and none was ever started again.
      promise.future.onComplete(_ => { inFlight.compareAndSet(marker, None); () })(using ExecutionContext.parasitic)
      promise.future.onComplete {
        // Stored after the waiting caller is released, not on its time.
        case Success(snapshot)  => saveIfNewest(snapshot)
        case Failure(exception) =>
          logger.warn(s"$label: re-read failed, keeping the previous snapshot: " +
            s"${exception.getClass.getSimpleName}: ${exception.getMessage}")
      }
      promise.future
    } else inFlight.get().getOrElse(refresh())
  }
}
