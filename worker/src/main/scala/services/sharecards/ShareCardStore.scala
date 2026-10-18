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
 * And each of the country's cities has ONE: its card, `city-<slug>.jpg` ([[cityCardPath]]), served
 * beside the film cards at `/share-cards/<cc>/city-<slug>.jpg` and re-rendered weekly
 * ([[CityCardService]]). All of it sits under one budget and one free-space floor
 * ([[ShareCardRoom]]).
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
 *    document names for the newer one. The check and the rename run under the film's lock
 *    file ([[ShareCardStore.LockDir]]), so no other writer lands between them.
 *  - A delete of a file that is already gone is not an error.
 *  - What the janitor may delete is decided from the directory ITSELF, never from per-process
 *    counters, so every replica sees the same budget.
 */
class ShareCardStore(val root: Path, val room: ShareCardRoom = ShareCardRoom.Unlimited) {
  import ShareCardStore.*

  // The in-JVM half of a film's lock (see `holding`): per store, because a directory has ONE store
  // per process — the wiring builds one per country, a worker runs one country (WorkerMain refuses
  // more), and a card directory is a country's own (its janitor prunes whatever that country's
  // read model does not name).
  private val jvmLocks = Array.fill(LockStripes)(new AnyRef)

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
  /** A city's card, `city-<slug>.jpg` beside the film cards ([[models.City.shareImage]]). */
  def cityCardPath(city: models.City): Path = root.resolve(city.shareImage)

  /** When the file at `path` was last written, or None when it is absent. */
  def written(path: Path): Option[Instant] = Try(Files.getLastModifiedTime(path).toInstant).toOption

  /** Mark the file at `path` written now without rewriting it — a re-render that would draw the
   *  same picture. False when it is gone. */
  def touch(path: Path, at: Instant): Boolean =
    Try(Files.setLastModifiedTime(path, java.nio.file.attribute.FileTime.from(at))).isSuccess
  /** Where a paid poster route remembers the posters that failed (see `RememberedFailurePosterDownload`):
   *  a dot directory under the poster cache, which the janitor's listing does not descend into. */
  def failedPosters: Path = posters.resolve(".failed")

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
          else Some(StoredFile(path, name, if (kind == Kind.Card && isCityCard(name)) Kind.City else kind, temp = name.endsWith(TempSuffix),
            bytes = Files.size(path), modified = Files.getLastModifiedTime(path).toInstant))
        }.toOption.flatten
      }
    }

  /** The lock file a card write to `target` holds (see [[writeAtomically]]). */
  def lockFor(target: Path): Path = root.resolve(LockDir).resolve(s"${Math.floorMod(target.getFileName.toString.hashCode, LockStripes)}.lock")

  /** THROWS [[NoRoomForShareCard]] when writing `target` would ADD a file the directory has no room
   *  for — the cap [[writeAtomically]] applies, asked before the work of drawing one. */
  def admitNew(target: Path): Unit = if (!Files.exists(target)) room.admit(kindOf(target))

  /** Write the JPEG `bytes` over `target` atomically (see the class doc), stamped with `version`
   *  and, when given, `published` and `asked`. THROWS [[NewerAskOnDisk]], writing nothing, when
   *  `target` holds a card asked for after `asked`. */
  def writeAtomically(target: Path, bytes: Array[Byte], version: String, published: Option[Instant] = None,
                      asked: Option[Instant] = None): Unit = {
    // THE CAP: a write that would ADD a file is refused while the directory is over its budget or
    // the volume is short of its free-space floor (see [[ShareCardRoom]]); overwriting a file in
    // place adds nothing and always goes ahead.
    admitNew(target)
    val temp = target.resolveSibling(s"${target.getFileName}.$writerId-${UUID.randomUUID().toString.take(8)}$TempSuffix")
    try {
      Using.resource(FileChannel.open(temp, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)) { channel =>
        val buffer = java.nio.ByteBuffer.wrap(stamped(bytes, version, published, asked))
        while (buffer.hasRemaining) channel.write(buffer)
        channel.force(true)
      }
      // A card write (`asked` given) holds the film's lock across the ask-order check and the
      // rename, so another writer can never land between the two.
      def commit(): Unit = {
        for (mine <- asked; theirs <- this.asked(target) if theirs.isAfter(mine)) throw new NewerAskOnDisk(target, theirs, mine)
        try Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch { case _: AtomicMoveNotSupportedException => Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING) }
        ()
      }
      if (asked.isDefined) holding(lockFor(target))(commit()) else commit()
    } finally Files.deleteIfExists(temp)
    ()
  }

  /** Run `body` holding `lock` against this process's threads (a JVM's file locks are its own:
   *  a second channel of the same process is refused, not queued) AND other processes (a POSIX
   *  record lock). Whether other HOSTS honour it is the filesystem's business: a local or NFSv4
   *  volume does; a mount without working locks would leave the check-then-rename window. */
  private def holding[A](lock: Path)(body: => A): A = {
    Files.createDirectories(lock.getParent)
    val stripe = jvmLocks(Math.floorMod(lock.toString.hashCode, jvmLocks.length))
    stripe.synchronized {
      Using.resource(FileChannel.open(lock, StandardOpenOption.CREATE, StandardOpenOption.WRITE)) { channel =>
        val held = channel.lock()
        try body finally held.release()
      }
    }
  }

  /** `jpeg` with a comment segment carrying the stamp inserted right after its start-of-image. */
  private def stamped(jpeg: Array[Byte], version: String, published: Option[Instant], asked: Option[Instant]): Array[Byte] = {
    require(jpeg.length > 2 && (jpeg(0) & 0xff) == 0xff && (jpeg(1) & 0xff) == 0xd8, "not a JPEG")
    val text   = (StampPrefix + s"v=$version" + published.fold("")(at => s";p=${at.getEpochSecond}") +
      asked.fold("")(at => s";a=${at.toEpochMilli}")).getBytes(StandardCharsets.US_ASCII)
    val length = text.length + 2
    Array[Byte](0xff.toByte, 0xd8.toByte, 0xff.toByte, 0xfe.toByte, (length >> 8).toByte, length.toByte) ++ text ++ jpeg.drop(2)
  }

  /** Which of the directory's kinds a file at `path` is. */
  private def kindOf(path: Path): String =
    if (path.getParent == posters) Kind.Poster
    else if (path.getParent == bases) Kind.Base
    else if (isCityCard(path.getFileName.toString)) Kind.City
    else Kind.Card

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
  /** Where card writes take their per-film lock: a dot directory Caddy must not serve, holding a
   *  FIXED set of stripe files (a lock file cannot safely be deleted while another may take it). */
  val LockDir     = ".locks"
  val LockStripes = 64

  private val StampPrefix    = "kinowo:"
  private val StampReadBytes = 256

  object Kind {
    val Card   = "card"
    val Poster = "poster"
    val Base   = "base"
    /** A city's card ([[models.City.shareImage]]), beside the film cards. */
    val City   = "city"
    val all: Seq[String] = Seq(Card, Poster, Base, City)
  }

  /** True for a city card's name (finished or a temp of one); a film card's never holds a `-`. */
  def isCityCard(name: String): Boolean = name.startsWith(models.City.ShareImagePrefix)

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
  def token: Option[String] = Option.when(!temp && kind != ShareCardStore.Kind.City && name.endsWith(".jpg"))(name.stripSuffix(".jpg"))

  /** The city slug a finished city card's name carries (`city-<slug>.jpg`). */
  def citySlug: Option[String] =
    Option.when(!temp && kind == ShareCardStore.Kind.City && name.endsWith(".jpg"))(name.stripPrefix(models.City.ShareImagePrefix).stripSuffix(".jpg"))
}

/** A new file refused by the directory's cap ([[ShareCardRoom]]): nothing written. */
final class NoRoomForShareCard(kind: String, why: String)
  extends RuntimeException(s"no room for a new share-card $kind file: $why")

/**
 * THE CAP ON THE SHARE-CARD DIRECTORY, as the writes see it. What happens at the limit:
 *
 *  - Over the BUDGET (`KINOWO_SHARE_CARD_BUDGET_MB`, the janitor's measure): the janitor first
 *    evicts the oldest files nothing references; when what IS referenced still exceeds it, it
 *    tells this room ([[measured]]), and from then until a pass finds the directory back under
 *    budget no NEW file is written.
 *  - Under the volume's FREE-SPACE FLOOR (`KINOWO_SHARE_CARD_MIN_FREE_MB`, read at each write): the
 *    same, so the cards never fill the node's disk, whatever else on it is growing.
 *
 * Refused is never broken: an existing file is still overwritten in place (a re-render adds no
 * bytes), and whatever has no card yet falls back — a film page to its city's card, a city page
 * to its country's home card. Every refusal is counted (`share_cards_refused_total{kind}`) and
 * alerted on (`ShareCardWritesRefused`).
 */
final class ShareCardRoom(val minimumFreeBytes: Long, freeBytes: () => Long, refused: String => Unit = _ => (),
                          honoursBudget: Boolean = true) {
  @volatile private var overBudget = false

  /** The janitor's latest measure: whether the directory stays over its budget after pruning. */
  def measured(overBudget: Boolean): Unit = if (honoursBudget) this.overBudget = overBudget

  /** The volume's free bytes now. */
  def free(): Long = freeBytes()

  /** THROWS [[NoRoomForShareCard]] when a new `kind` file may not be written. */
  def admit(kind: String): Unit = {
    val why =
      if (overBudget) Some("the directory is over its budget")
      else Some(free()).filter(_ < minimumFreeBytes).map(f => s"the volume has $f bytes free, under the $minimumFreeBytes-byte floor")
    why.foreach { reason => refused(kind); throw new NoRoomForShareCard(kind, reason) }
  }
}

object ShareCardRoom {
  /** No cap — for specs that are not about it. Shared, so it ignores the janitor's measure. */
  val Unlimited: ShareCardRoom = new ShareCardRoom(0L, () => Long.MaxValue, honoursBudget = false)

  /** The room on the volume holding `root`. A volume that cannot be asked reads as roomy: the
   *  budget still holds, and a stat failing must not stop every render. */
  def onVolumeOf(root: Path, minimumFree: settings.ShareCardMinimumFree, refused: String => Unit): ShareCardRoom =
    new ShareCardRoom(minimumFree.bytes, () => Try(Files.getFileStore(root).getUsableSpace).getOrElse(Long.MaxValue), refused)

  /** The volume's total size, for the gauge beside the free bytes. */
  def volumeSize(root: Path): Option[Long] = Try(Files.getFileStore(root).getTotalSpace).toOption
}
