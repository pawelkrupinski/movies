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
}
