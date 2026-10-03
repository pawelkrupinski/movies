package modules

import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

/** application.conf configures Play and nothing else. The application's own settings are
 *  read in one place, `settings.ProcessConfiguration` (`ProcessAccessLintSpec` keeps every
 *  other file from reading one), so a key here outside `play.*` is one nothing reads — a
 *  value that looks live and silently is not. The `mongodb { uri, database }` block sat here
 *  long after the app stopped reading it, naming a `MONGODB_DATABASE` variable the
 *  connection never consulted (it reads `MONGODB_DB`). */
class ApplicationConfKeysSpec extends AnyFlatSpec with Matchers {

  "application.conf" should "set only Play's own keys" in {
    val roots = ConfigFactory.parseResources("application.conf").root.keySet.asScala.toSet
    roots shouldBe Set("play")
  }
}
