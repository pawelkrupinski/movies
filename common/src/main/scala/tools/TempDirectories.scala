package tools

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.util.concurrent.ConcurrentHashMap

/**
 * Temp directories (and files) that do not outlive their owner. `Files.createTempDirectory` alone leaves its directory
 * in the per-user temp directory for good: by 2026-10-29 the test suites had left ~270,000 entries and ~40 GB there
 * (`scratch-git*`, `share-cards*`, `enrichment-tree*`, ...), one or more per test run.
 *
 * Every path made here is remembered until [[delete]] removes it, and whatever is still remembered when the JVM exits
 * is deleted by a shutdown hook — the fallback for an owner that never got to delete its own (a test that threw, a
 * helper object with no end of its own, a JVM stopped mid-suite). Tests take theirs through testkit's
 * `ScratchDirectories`, which deletes a suite's paths when the suite ends; `SharedMachineStateLintSpec` holds every
 * test to that.
 */
object TempDirectories {

  private val live = ConcurrentHashMap.newKeySet[Path]()

  Runtime.getRuntime.addShutdownHook(new Thread(() => live.forEach(deleteTree), "temp-directories-cleanup"))

  /** A fresh directory `<temp>/<prefix><random>`, deleted by [[delete]] or at the latest when the JVM exits. */
  def create(prefix: String): Path = remember(Files.createTempDirectory(prefix))

  /** A fresh file `<temp>/<prefix><random><suffix>`, deleted by [[delete]] or at the latest when the JVM exits. */
  def createFile(prefix: String, suffix: String): Path = remember(Files.createTempFile(prefix, suffix))

  /** `path` and everything under it gone, quietly: a path already gone, or a file that cannot be removed, is no
   *  failure of the caller's. */
  def delete(path: Path): Unit = {
    deleteTree(path)
    live.remove(path)
  }

  /** `body` over a fresh directory, deleted when `body` returns or throws. */
  def within[A](prefix: String)(body: Path => A): A = {
    val directory = create(prefix)
    try body(directory)
    finally delete(directory)
  }

  private def remember(path: Path): Path = {
    live.add(path)
    path
  }

  private def deleteTree(path: Path): Unit =
    try
      if (Files.isDirectory(path) && !Files.isSymbolicLink(path))
        Files.walkFileTree(
          path,
          new SimpleFileVisitor[Path] {
            // A spec that took a directory's permissions away (to make a write fail) does not keep it from going.
            override def preVisitDirectory(directory: Path, attributes: BasicFileAttributes): FileVisitResult = {
              grantOwner(directory)
              FileVisitResult.CONTINUE
            }
            override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
              quietly(Files.deleteIfExists(file))
              FileVisitResult.CONTINUE
            }
            // An unreadable directory fails to open before its pre-visit: granted, then walked on its own.
            override def visitFileFailed(file: Path, failure: IOException): FileVisitResult = {
              if (Files.isDirectory(file) && !Files.isSymbolicLink(file) && grantOwner(file)) deleteTree(file)
              else quietly(Files.deleteIfExists(file))
              FileVisitResult.CONTINUE
            }
            override def postVisitDirectory(directory: Path, failure: IOException): FileVisitResult = {
              quietly(Files.deleteIfExists(directory))
              FileVisitResult.CONTINUE
            }
          }
        )
      else quietly(Files.deleteIfExists(path))
    catch { case _: IOException => () }

  /** Whether `directory` now lets its owner list, enter and change it (false when it already did, or cannot). */
  private def grantOwner(directory: Path): Boolean = {
    val file   = directory.toFile
    val before = file.canRead && file.canWrite && file.canExecute
    file.setReadable(true); file.setWritable(true); file.setExecutable(true)
    !before && file.canRead && file.canWrite && file.canExecute
  }

  private def quietly(delete: => Boolean): Unit =
    try delete
    catch { case _: IOException => () }
}
