package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.util.jar.{JarEntry, JarOutputStream}
import scala.util.Using

object ClassArchiveTrainingSpec {
  @volatile var tripped = false
  object Tripwire { tripped = true }
}

class ClassArchiveTrainingSpec extends AnyFlatSpec with Matchers {
  import ClassArchiveTrainingSpec.*

  "The class archive training" should "name every class a jar carries, and nothing else" in {
    val jar = Files.createTempFile("training", ".jar")
    Using.resource(new JarOutputStream(Files.newOutputStream(jar))) { out =>
      Seq("a/B.class", "a/B$C.class", "module-info.class", "META-INF/versions/21/a/B.class", "a/notes.txt")
        .foreach { name => out.putNextEntry(new JarEntry(name)); out.closeEntry() }
    }
    ClassArchiveTraining.classNames(jar) should contain theSameElementsAs Seq("a.B", "a.B$C")
  }

  it should "load a class without running its initialiser, and count what cannot load instead of failing" in {
    val loaded = ClassArchiveTraining.load(
      Seq(classOf[ClassArchiveTrainingSpec].getName + "$Tripwire$", "no.such.Klass"), getClass.getClassLoader)
    loaded shouldBe ClassArchiveTraining.Loaded(classes = 1, failed = 1)
    tripped shouldBe false
  }

  it should "read a class list's names, skipping its comments and blank lines" in {
    ClassArchiveTraining.classList("# generated\n\na.B\n  a.B$C  \n# note\n") shouldBe Seq("a.B", "a.B$C")
  }

  /** A launcher whose classpath carries a class list trains on the classes production loads; one
   *  without (the web) still trains on every class its jars carry. */
  it should "train on the class list when the classpath carries one, and on every jar's classes otherwise" in {
    val jar = Files.createTempFile("training", ".jar")
    Using.resource(new JarOutputStream(Files.newOutputStream(jar))) { out =>
      Seq("a/B.class", "a/C.class").foreach { name => out.putNextEntry(new JarEntry(name)); out.closeEntry() }
    }
    ClassArchiveTraining.trainingNames(Seq(jar), Some("x.Listed\n")) shouldBe Seq("x.Listed")
    ClassArchiveTraining.trainingNames(Seq(jar), None) should contain theSameElementsAs Seq("a.B", "a.C")
  }
}
