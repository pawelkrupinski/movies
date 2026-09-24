package services.sharecards

import java.io.IOException
import java.lang.management.ManagementFactory
import java.nio.channels.FileChannel
import java.nio.file.{AtomicMoveNotSupportedException, Files, NoSuchFileException, Path, StandardCopyOption, StandardOpenOption}
import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

/**
 * One country's share-card directory — the rendered cards (`<file>.jpg`, served by Caddy at
 * `/share-cards/<cc>/<file>`) and, under [[PosterDir]], the poster cache (never served).
 *
 * SEVERAL WRITERS SHARE IT. Every render thread of every worker replica of the country writes
 * here, so nothing in this class relies on being the only one:
 *
 *  - A write goes to a temp file UNIQUE TO THE WRITER (`<name>.<host>-<pid>-<random>.tmp`), is
 *    fsynced, then renamed onto its final name in one atomic step. A reader (Caddy) sees either
 *    no file or the whole file. Two writers racing on one content-addressed name write the same
 *    bytes, so whichever rename lands last changes nothing.
 *  - A delete of a file that is already gone is not an error: two janitors (or a janitor and a
 *    replica's own prune) may pick the same file.
 *  - What the janitor may delete is decided from the directory ITSELF (sizes, mtimes), never from
 *    per-process counters, so every replica sees the same budget.
 */
class ShareCardStore(val root: Path) {
  import ShareCardStore.*

  private val posters = root.resolve(PosterDir)

  /** True when this process can write the directory — false in a test or dev JVM with no mount,
   *  which then runs without share cards. */
  def usable: Boolean = Try {
    Files.createDirectories(posters)
    Files.isDirectory(root) && Files.isWritable(root) && Files.isWritable(posters)
  }.getOrElse(false)

  def cardPath(name: String): Path  = root.resolve(name)
  def posterPath(key: String): Path = posters.resolve(s"$key.$PosterExtension")

  def cardExists(name: String): Boolean = Files.isRegularFile(cardPath(name))

  /** Every file in the directory and the poster cache, with the facts the janitor decides on. A
   *  file deleted by someone else mid-listing is skipped. */
  def list(): Seq[StoredFile] =
    Seq(root -> Kind.Card, posters -> Kind.Poster).flatMap { case (dir, kind) =>
      if (!Files.isDirectory(dir)) Nil
      else Using.resource(Files.list(dir))(_.iterator.asScala.toList).flatMap { path =>
        val name = path.getFileName.toString
        Try {
          if (!Files.isRegularFile(path)) None
          else Some(StoredFile(path, name, kind, temp = name.endsWith(TempSuffix),
            bytes = Files.size(path), modified = Files.getLastModifiedTime(path).toInstant))
        }.toOption.flatten
      }
    }

  /** Write `bytes` to `target` atomically (see the class doc). */
  def writeAtomically(target: Path, bytes: Array[Byte]): Unit = {
    val temp = target.resolveSibling(s"${target.getFileName}.$writerId-${UUID.randomUUID().toString.take(8)}$TempSuffix")
    try {
      Using.resource(FileChannel.open(temp, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)) { channel =>
        val buffer = java.nio.ByteBuffer.wrap(bytes)
        while (buffer.hasRemaining) channel.write(buffer)
        channel.force(true)
      }
      try Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch { case _: AtomicMoveNotSupportedException => Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING) }
    } finally Files.deleteIfExists(temp)
    ()
  }

  /** Delete `file`; false when it was already gone. */
  def delete(file: StoredFile): Boolean =
    try Files.deleteIfExists(file.path)
    catch { case _: NoSuchFileException => false; case _: IOException => false }
}

object ShareCardStore {
  /** The poster cache's subdirectory. A dot directory, which Caddy must not serve. */
  val PosterDir = ".posters"
  val PosterExtension = "jpg"
  val TempSuffix = ".tmp"

  object Kind {
    val Card   = "card"
    val Poster = "poster"
    val all: Seq[String] = Seq(Card, Poster)
  }

  /** Host and pid, so a temp file names the process that wrote it. */
  private[sharecards] val writerId: String = {
    val runtime = ManagementFactory.getRuntimeMXBean.getName // "<pid>@<host>"
    val (pid, host) = runtime.split('@') match {
      case Array(p, h) => (p, h)
      case _           => (ProcessHandle.current().pid().toString, "host")
    }
    s"${host.filter(_.isLetterOrDigit).take(24)}-$pid"
  }
}

/** One file in the store. `kind` is where it lives (cards or posters); `temp` marks a write in
 *  progress (or abandoned by a writer that died). */
final case class StoredFile(path: Path, name: String, kind: String, temp: Boolean, bytes: Long, modified: Instant)
