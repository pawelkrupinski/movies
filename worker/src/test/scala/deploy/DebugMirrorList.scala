package deploy

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import services.DebugMirror

/** Writes `debug-mirror-collections.json`, the list the local mirror's sync reads, from [[services.MongoCollections]]
 *  ([[DebugMirror.rendered]]); [[DebugMirrorListSpec]] fails when the file and the registry disagree. Regenerate:
 *  `sbt 'worker/Test/runMain deploy.DebugMirrorList'`. */
object DebugMirrorList {

  val Path = s"common/src/main/resources/${DebugMirror.ListResource}"

  def main(args: Array[String]): Unit = {
    Files.write(Paths.get(Path), DebugMirror.rendered.getBytes(StandardCharsets.UTF_8))
    println(s"wrote $Path (${DebugMirror.Collections.size} collections)")
  }
}
