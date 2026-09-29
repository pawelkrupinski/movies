package tools

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The image build's training run for the JVM's AOT cache: it LOADS every class on the launcher's
 *  classpath, initialising none, so `-XX:AOTCacheOutput` archives them all. A class served from
 *  the cache lives in the mapped file instead of metaspace — loading all 31k of the worker's
 *  classes took 157 MB of metaspace and 37 MB of class space without it, 0.6 MB with it — and
 *  worker-pl died of `OutOfMemoryError: Metaspace` at its 128m cap before this. The Dockerfile runs
 *  it through `bin/$BIN -main`, so the classpath and the baked JVM options are the ones the app
 *  starts with; a cache trained on any other options does not map.
 *
 *  Only classes: a lambda's class is spun when its call site first runs, so lambdas still load
 *  into metaspace at run time. */
object ClassArchiveTraining {

  final case class Loaded(classes: Int, failed: Int)

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

  def main(arguments: Array[String]): Unit = {
    val jars = System.getProperty("java.class.path").split(File.pathSeparator).toSeq
      .filter(_.endsWith(".jar")).map(Paths.get(_)).filter(Files.isRegularFile(_))
    val loaded = load(jars.flatMap(classNames), getClass.getClassLoader)
    println(s"class archive training: loaded ${loaded.classes} class(es) from ${jars.size} jar(s), ${loaded.failed} not loadable")
  }
}
