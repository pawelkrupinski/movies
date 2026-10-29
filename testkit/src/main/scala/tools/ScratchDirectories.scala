package tools

import org.scalatest.{Args, Status, Suite, SuiteMixin}

import java.nio.file.Path
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
 * A suite's temp directories and files, deleted when the suite ends — passed or failed — and, should the JVM stop
 * first, when it exits ([[TempDirectories]]). The one way a spec takes a temp path: `SharedMachineStateLintSpec`
 * refuses a bare `Files.createTempDirectory` / `createTempFile` in tests, which never deleted theirs (~270,000 entries,
 * ~40 GB in the per-user temp directory by 2026-10-29).
 *
 * A path lives for the whole suite, so a directory made in the constructor and shared by its tests stays until the
 * last one is done. A spec that makes many large ones in one test can drop each sooner with
 * [[TempDirectories.delete]] or [[TempDirectories.within]].
 */
trait ScratchDirectories extends SuiteMixin { this: Suite =>

  private lazy val owned = new ConcurrentLinkedQueue[Path]()

  /** A fresh directory `<temp>/<prefix><random>`, gone when this suite ends. */
  protected def tempDirectory(prefix: String): Path = own(TempDirectories.create(prefix))

  /** A fresh file `<temp>/<prefix><random><suffix>`, gone when this suite ends. */
  protected def tempFile(prefix: String, suffix: String): Path = own(TempDirectories.createFile(prefix, suffix))

  private def own(path: Path): Path = {
    owned.add(path)
    path
  }

  abstract override def run(testName: Option[String], args: Args): Status = {
    val status =
      try super.run(testName, args)
      catch {
        case failure: Throwable =>
          deleteOwned()
          throw failure
      }
    status.whenCompleted(_ => deleteOwned())
    status
  }

  private def deleteOwned(): Unit = owned.asScala.toList.foreach { path =>
    owned.remove(path)
    TempDirectories.delete(path)
  }
}
