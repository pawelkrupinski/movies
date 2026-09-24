package services.sharecards

import java.io.IOException
import java.lang.management.ManagementFactory
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, NoSuchFileException, Path, StandardCopyOption, StandardOpenOption}
import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

/**
 * One country's share-card directory. Each film has AT MOST THREE FILES, every one at a stable name
 * and overwritten in place, so nothing old is ever left behind:
 *
 *  - its card, `<film>.jpg` — served by Caddy at `/share-cards/<cc>/<film>.jpg`, always the latest
 *    render; the web adds `?v=<version>` so a changed card is a new URL to preview caches;
 *  - its card BASE, [[BaseDir]]`/<film>.jpg` — the card without its rating badges;
 *  - its poster, [[PosterDir]]`/<film>.jpg` — shrunk to the card's slot.
 *
 * WHAT A FILE WAS DRAWN FROM TRAVELS INSIDE IT: every file carries a JPEG comment
 * (`kinowo:v=<version>[;p=<epoch seconds>]`) written in the same atomic rename as its pixels, so
 * the version can never disagree with the bytes. `v` is the file's input hash; `p`, on a card, is
 * when the film was first published (see [[ShareCardService]]); `a`, on a card, is when the inputs
 * it was drawn from were ASKED for — what orders two replicas' renders of one film.
 *
 * SEVERAL WRITERS SHARE IT. Every render thread of every worker replica of the country writes here:
 *
 *  - A write goes to a temp file UNIQUE TO THE WRITER (`<name>.<host>-<pid>-<random>.tmp`), is
 *    fsynced, then renamed over the final name in one atomic step. A reader (Caddy) sees the old
 *    file or the new one, never a part. Two replicas rendering one film race only on the rename,
 *    and a card write never replaces a card whose inputs were asked for LATER (its `a` stamp): a
 *    replica that never saw the newer ask would otherwise put the older picture under the URL the
 *    document names for the newer one. (The check and the rename are two steps, so two renders
 *    of one film landing inside that window can still cross.)
 *  - A delete of a file that is already gone is not an error.
 *  - What the janitor may delete is decided from the directory ITSELF, never from per-process
 *    counters, so every replica sees the same budget.
 */
class ShareCardStore(val root: Path) {
  import ShareCardStore.*

  private val posters = root.resolve(PosterDir)
  private val bases   = root.resolve(BaseDir)

  /** True when this process can write the directory — false in a test or dev JVM with no mount,
   *  which then runs without share cards. */
  def usable: Boolean = Try {
    Files.createDirectories(posters)
    Files.createDirectories(bases)
    Files.isDirectory(root) && Files.isWritable(root) && Files.isWritable(posters) && Files.isWritable(bases)
  }.getOrElse(false)

  def cardPath(filmId: String): Path   = root.resolve(ShareCardFile.name(filmId))
  def basePath(filmId: String): Path   = bases.resolve(ShareCardFile.name(filmId))
  def posterPath(filmId: String): Path = posters.resolve(ShareCardFile.name(filmId))

  /** The version a file was written with ([[writeAtomically]]), or None when it is absent. */
  def version(path: Path): Option[String] = stamp(path).get("v")

  /** When the inputs the card at `path` was drawn from were asked for, if it records it. */
  def asked(path: Path): Option[Instant] = stamp(path).get("a").flatMap(_.toLongOption).map(Instant.ofEpochMilli)

  /** When the film whose card is at `path` was first published, if the card records it. */
  def published(path: Path): Option[Instant] = stamp(path).get("p").flatMap(_.toLongOption).map(Instant.ofEpochSecond)

  private def stamp(path: Path): Map[String, String] =
    Try(Using.resource(Files.newInputStream(path))(_.readNBytes(StampReadBytes))).toOption.fold(Map.empty[String, String]) { head =>
      if (head.length < 6 || (head(2) & 0xff) != 0xff || (head(3) & 0xff) != 0xfe) Map.empty
      else {
        val length = ((head(4) & 0xff) << 8) | (head(5) & 0xff)
        val text   = new String(head.slice(6, 4 + length), StandardCharsets.US_ASCII)
        if (!text.startsWith(StampPrefix)) Map.empty
        else text.stripPrefix(StampPrefix).split(';').toSeq.flatMap(_.split("=", 2) match {
          case Array(key, value) => Some(key -> value)
          case _                 => None
        }).toMap
      }
    }

  /** Every file in the directory, its poster cache and its bases, with the facts the janitor decides
   *  on. A file deleted by someone else mid-listing is skipped. */
  def list(): Seq[StoredFile] =
    Seq(root -> Kind.Card, posters -> Kind.Poster, bases -> Kind.Base).flatMap { case (dir, kind) =>
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

  /** Write the JPEG `bytes` over `target` atomically (see the class doc), stamped with `version`
   *  and, when given, `published` and `asked`. THROWS [[NewerAskOnDisk]], writing nothing, when
   *  `target` holds a card asked for after `asked`. */
  def writeAtomically(target: Path, bytes: Array[Byte], version: String, published: Option[Instant] = None,
                      asked: Option[Instant] = None): Unit = {
    val temp = target.resolveSibling(s"${target.getFileName}.$writerId-${UUID.randomUUID().toString.take(8)}$TempSuffix")
    try {
      Using.resource(FileChannel.open(temp, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)) { channel =>
        val buffer = java.nio.ByteBuffer.wrap(stamped(bytes, version, published, asked))
        while (buffer.hasRemaining) channel.write(buffer)
        channel.force(true)
      }
      for (mine <- asked; theirs <- this.asked(target) if theirs.isAfter(mine)) throw new NewerAskOnDisk(target, theirs, mine)
      try Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch { case _: AtomicMoveNotSupportedException => Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING) }
    } finally Files.deleteIfExists(temp)
    ()
  }

  /** `jpeg` with a comment segment carrying the stamp inserted right after its start-of-image. */
  private def stamped(jpeg: Array[Byte], version: String, published: Option[Instant], asked: Option[Instant]): Array[Byte] = {
    require(jpeg.length > 2 && (jpeg(0) & 0xff) == 0xff && (jpeg(1) & 0xff) == 0xd8, "not a JPEG")
    val text   = (StampPrefix + s"v=$version" + published.fold("")(at => s";p=${at.getEpochSecond}") +
      asked.fold("")(at => s";a=${at.toEpochMilli}")).getBytes(StandardCharsets.US_ASCII)
    val length = text.length + 2
    Array[Byte](0xff.toByte, 0xd8.toByte, 0xff.toByte, 0xfe.toByte, (length >> 8).toByte, length.toByte) ++ text ++ jpeg.drop(2)
  }

  /** Delete `file`; false when it was already gone. */
  def delete(file: StoredFile): Boolean = deletePath(file.path)

  /** Delete the film's card, base and poster — each only if it was last written before `olderThan`
   *  (a younger one may be another replica's render of a film that is coming back). Returns the kinds
   *  deleted. */
  def deleteFilm(filmId: String, olderThan: Instant): Seq[String] =
    Seq(cardPath(filmId) -> Kind.Card, basePath(filmId) -> Kind.Base, posterPath(filmId) -> Kind.Poster).collect {
      case (path, kind) if Try(Files.getLastModifiedTime(path).toInstant.isBefore(olderThan)).getOrElse(false) && deletePath(path) => kind
    }

  private def deletePath(path: Path): Boolean =
    try Files.deleteIfExists(path)
    catch { case _: NoSuchFileException => false; case _: IOException => false }
}

/** A card write refused because the card on disk was drawn from inputs asked for later. */
final class NewerAskOnDisk(target: Path, onDisk: Instant, mine: Instant)
  extends RuntimeException(s"$target holds a card asked for at $onDisk, after this render's $mine")

object ShareCardStore {
  /** The poster cache's subdirectory. A dot directory, which Caddy must not serve. */
  val PosterDir = ".posters"
  /** The card BASES' subdirectory (a card without its rating badges) — a dot directory too. */
  val BaseDir = ".base"
  val TempSuffix = ".tmp"

  private val StampPrefix    = "kinowo:"
  private val StampReadBytes = 256

  object Kind {
    val Card   = "card"
    val Poster = "poster"
    val Base   = "base"
    val all: Seq[String] = Seq(Card, Poster, Base)
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

/** One file in the store. `kind` is where it lives (cards, posters or bases); `temp` marks a write
 *  in progress (or abandoned by a writer that died). */
final case class StoredFile(path: Path, name: String, kind: String, temp: Boolean, bytes: Long, modified: Instant) {
  /** The film token its name carries (`<token>.jpg`), for a finished file. */
  def token: Option[String] = Option.when(!temp && name.endsWith(".jpg"))(name.stripSuffix(".jpg"))
}
