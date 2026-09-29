package tools

import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The image build's training run for the JVM's AOT cache: it LOADS every class on the launcher's
 *  classpath, initialising none, so `-XX:AOTCacheOutput` archives them all. A class served from
 *  the cache lives in the mapped file instead of metaspace — loading all 31k of the worker's
 *  classes took 157 MB of metaspace and 37 MB of class space without it, 0.6 MB with it — and
 *  worker-pl died of `OutOfMemoryError: Metaspace` at its 128m cap before this. The Dockerfile runs
 *  it as `bin/$BIN -main tools.ClassArchiveTraining /app/lib`, so the classpath and the baked JVM
 *  options are the ones the app starts with; a cache trained on any other options does not map.
 *
 *  Only classes: a lambda's class is spun when its call site first runs, so lambdas still load
 *  into metaspace at run time.
 *
 *  WHICH classes: those a class list on the launcher's classpath names ([[ClassListResource]] — the
 *  worker's, generated from production heap dumps by scripts/aot-class-list.py), or every class the
 *  jars carry when there is none (the web). Archiving only what production loads mattered: in the
 *  worker image under PL's options, the fixture pipeline held 559 MB anonymous RSS and 17 MB of
 *  metaspace on a cache of the classes it used, against 638 MB and 35 MB on one of all 31k. A class
 *  the list lacks still loads, into metaspace, as before the cache. */
object ClassArchiveTraining {

  final case class Loaded(classes: Int, failed: Int)

  /** The class list a launcher's classpath may carry: one binary name a line, `#` comments. */
  val ClassListResource = "/aot-classes.txt"

  def classList(text: String): Seq[String] =
    text.linesIterator.map(_.trim).filterNot(line => line.isEmpty || line.startsWith("#")).toSeq

  /** What to load: the class list's names when there is one, every class the jars carry otherwise. */
  def trainingNames(jars: Seq[Path], listed: Option[String]): Seq[String] =
    listed.fold(jars.flatMap(classNames))(classList)

  /** Every class a jar carries, by binary name — not `module-info`, nor a multi-release jar's
   *  `META-INF/versions/` copies, which the loader resolves from the base name. */
  def classNames(jar: Path): Seq[String] =
    Using.resource(new JarFile(jar.toFile)) { file =>
      file.entries().asScala.map(_.getName)
        .filter(name => name.endsWith(".class") && !name.startsWith("META-INF/") && !name.endsWith("module-info.class"))
        .map(_.stripSuffix(".class").replace('/', '.'))
        .toVector
    }

  /** Load each class without running its static initialiser. A class whose dependency is not on
   *  the classpath (a driver's optional netty or zstd support) fails to load and is counted, not
   *  thrown: the cache then simply lacks it, as the running app never loads it either. */
  def load(names: Iterable[String], loader: ClassLoader): Loaded =
    names.foldLeft(Loaded(0, 0)) { (loaded, name) =>
      try { Class.forName(name, false, loader); loaded.copy(classes = loaded.classes + 1) }
      catch { case _: LinkageError | _: ClassNotFoundException => loaded.copy(failed = loaded.failed + 1) }
    }

  /** The jars of the directory the image's launcher builds its classpath from (`/app/lib`): which
   *  classes to load. The launcher's own classpath still decides where each one loads from. */
  def main(arguments: Array[String]): Unit = {
    val directory = Paths.get(arguments.headOption.getOrElse(sys.error("usage: ClassArchiveTraining <lib directory>")))
    val jars = Using.resource(Files.list(directory))(_.iterator.asScala.toVector)
      .filter(jar => jar.toString.endsWith(".jar") && Files.isRegularFile(jar)).sorted
    val listed = Option(getClass.getResourceAsStream(ClassListResource))
      .map(stream => try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally stream.close())
    val loaded = load(trainingNames(jars, listed), getClass.getClassLoader)
    println(s"class archive training: loaded ${loaded.classes} class(es) ${listed.fold(s"from ${jars.size} jar(s)")(_ => s"from $ClassListResource")}, ${loaded.failed} not loadable")
  }
}
