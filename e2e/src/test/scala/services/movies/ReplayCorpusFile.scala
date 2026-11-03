package services.movies

import models.{Cinema, CinemaNetworks, Showtime}

import java.io.{BufferedInputStream, BufferedOutputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamClass}
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/** One order pass's [[ReplayCorpus]] on disk, so the passes can run in rows of their own and a comparison job compares
 *  them (country-convergence-leg.yml's `order-compare`): Java serialisation, gzipped, `pass<N>.bin.gz`.
 *
 *  Two things in a corpus do not survive plain serialisation, and both would change what `==` answers:
 *  - a data-driven venue (`UkCinema`, `GermanCinema`, ...) is a plain class with reference equality, so it is written as
 *    its key in the roster and read back as the roster's own instance;
 *  - a [[Showtime]] still awaiting its row's URL prefix holds the one sentinel prefix and is told apart by `eq`, so that
 *    sentinel is written as a marker and read back as this JVM's sentinel.
 *
 *  The writer reads the file straight back and requires the copy `==` the corpus it wrote (records, screenings and rows
 *  each way). Equality is an equivalence, so two passes' copies compare exactly as the passes did: the verdict is the
 *  in-row comparison's, computed on equals of what the row held. */
object ReplayCorpusFile {

  def of(dir: Path, pass: Int): Path = dir.resolve(s"pass$pass.bin.gz")

  def write(corpus: ReplayCorpus, file: Path): Unit = {
    Files.createDirectories(file.toAbsolutePath.getParent)
    val out = new CorpusOutput(new BufferedOutputStream(new GZIPOutputStream(Files.newOutputStream(file), 1 << 16), 1 << 16))
    try out.writeObject(corpus) finally out.close()
  }

  def read(file: Path): ReplayCorpus = {
    val in = new CorpusInput(new BufferedInputStream(new GZIPInputStream(Files.newInputStream(file), 1 << 16), 1 << 16))
    try in.readObject().asInstanceOf[ReplayCorpus] finally in.close()
  }

  /** Writes the corpus and proves the file reads back equal to it; the first axis that does not is the error. */
  def writeVerified(corpus: ReplayCorpus, file: Path): Unit = {
    write(corpus, file)
    val copy = read(file)
    def same(axis: String, a: Any, b: Any): Unit =
      if (a != b || b != a) throw new IllegalStateException(s"$file: the $axis read back unequal to the pass's own")
    same("records", corpus.records, copy.records)
    same("screenings", corpus.screenings, copy.screenings)
    same("rendered rows", corpus.rows, copy.rows)
  }

  /** A venue as written: its roster key. */
  private final case class CinemaRef(key: String)
  /** The awaiting-prefix sentinel as written: a string (a String field holds only a string), one no URL can be. */
  private val AwaitingPrefixRef = "\u0000awaiting-row-prefix\u0000"

  /** Class, names, and `toString` only where the class gives one: Object's own carries the identity hash, which differs
   *  between the JVM that wrote the file and the one that reads it. */
  private[movies] def keyOf(c: Cinema): String = {
    val own = c.getClass.getMethod("toString").getDeclaringClass != classOf[Object]
    s"${c.getClass.getName}|${c.displayName}|${c.pillName}" + (if (own) s"|$c" else "")
  }

  /** Every venue a corpus can name, by key; a key two venues share is ambiguous, and naming one fails the write. */
  private lazy val roster: Map[String, Seq[Cinema]] = {
    val seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Cinema, java.lang.Boolean])
    (Cinema.all ++ CinemaNetworks.all).filter(seen.add).groupBy(keyOf)
  }

  private[movies] def ambiguousKeys: Set[String] = roster.collect { case (k, cs) if cs.sizeIs > 1 => k }.toSet

  /** This JVM's awaiting-prefix sentinel, read off a showtime built through the public constructor that sets it. */
  private lazy val awaitingPrefix: String =
    Showtime.stored(java.time.LocalDateTime.of(2000, 1, 1, 0, 0), null, "", None, Nil).urlSplitPrefix
      .getOrElse(throw new IllegalStateException("Showtime.stored(prefix = null) carried no awaiting prefix"))

  private final class CorpusOutput(out: java.io.OutputStream) extends ObjectOutputStream(out) {
    enableReplaceObject(true)
    override protected def replaceObject(obj: AnyRef): AnyRef = obj match {
      case s: String if s eq awaitingPrefix => AwaitingPrefixRef
      case c: Cinema =>
        val key = keyOf(c)
        roster.get(key) match {
          case Some(Seq(only)) if only eq c => CinemaRef(key)
          case Some(Seq(_))                 => throw new IllegalStateException(s"venue $key is not the roster's own instance")
          case Some(_)                      => throw new IllegalStateException(s"venue $key: two venues share the key")
          case None                         => throw new IllegalStateException(s"venue $key is in no roster")
        }
      case other => other
    }
  }

  private final class CorpusInput(in: java.io.InputStream) extends ObjectInputStream(in) {
    enableResolveObject(true)
    override protected def resolveObject(obj: AnyRef): AnyRef = obj match {
      case s: String if s == AwaitingPrefixRef => awaitingPrefix
      case CinemaRef(key) =>
        roster.get(key).collect { case Seq(only) => only }
          .getOrElse(throw new IllegalStateException(s"venue $key is in no roster here, or two venues share the key"))
      case other => other
    }
    // The test classpath's own loader: sbt's layered loaders hold the corpus's classes, not the platform's.
    override protected def resolveClass(desc: ObjectStreamClass): Class[?] =
      try Class.forName(desc.getName, false, ReplayCorpusFile.getClass.getClassLoader)
      catch { case _: ClassNotFoundException => super.resolveClass(desc) }
  }
}
