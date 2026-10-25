package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.DebugMirror

/** The local mirror's sync reads `debug-mirror-collections.json`; a collection marked mirrored in the registry but missing
 *  from the file reads as permanently EMPTY on a /debug page. */
class DebugMirrorListSpec extends AnyFlatSpec with Matchers {

  "debug-mirror-collections.json" should "be what DebugMirrorList generates from the registry today" in {
    withClue(s"stale: run sbt 'worker/Test/runMain deploy.DebugMirrorList' and commit ${DebugMirrorList.Path}\n") {
      RepoFile.read(DebugMirrorList.Path) shouldBe DebugMirror.rendered
    }
  }
}
