package tools

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import scala.util.Try

/** Runs a `main` in a fresh JVM on this test run's classpath. For behaviour that depends on what
 *  ran FIRST in the process -- a JDK setting read once, a lazily created global -- which the specs'
 *  shared JVM cannot answer, since any earlier spec may already have settled it either way. */
object ChildJvm {

  /** Exit code and combined stdout+stderr. */
  def run(main: String, args: Seq[String] = Nil, jvmArgs: Seq[String] = Nil): (Int, String) = {
    val java    = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val process = new ProcessBuilder((Seq(java, "-cp", classpath) ++ jvmArgs ++ (main +: args))*)
      .redirectErrorStream(true).start()
    val output  = new String(process.getInputStream.readAllBytes(), UTF_8)
    if (!process.waitFor(120, TimeUnit.SECONDS)) { process.destroyForcibly(); (-1, output + "\n[timed out]") }
    else (process.exitValue(), output)
  }

  /** This test run's classpath, whether sbt forked it (`java.class.path`) or runs it in its own
   *  layered class loaders (their URLs). */
  private def classpath: String = {
    def urls(loader: ClassLoader): Seq[String] = loader match {
      case null              => Nil
      case u: URLClassLoader => u.getURLs.toSeq.flatMap(url => Try(Paths.get(url.toURI).toString).toOption) ++ urls(u.getParent)
      case other             => urls(other.getParent)
    }
    (urls(getClass.getClassLoader) ++ System.getProperty("java.class.path").split(java.io.File.pathSeparator))
      .distinct.mkString(java.io.File.pathSeparator)
  }
}
