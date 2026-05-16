package clients.tools

import tools.{GetOnlyHttpFetch, RealHttpFetch}

import java.io.File
import java.net.URI
import java.nio.file.Files

class RecordingHttpFetch(fixtureDir: String, realFetch: RealHttpFetch) extends GetOnlyHttpFetch {
  val fixtureRoot = "test/resources/fixtures/" + fixtureDir

  override def get(url: String): String = {
    val uri = new URI(url)
    val path = uri.getPath.stripPrefix("/")
    val base = s"$fixtureRoot/${uri.getHost}/$path"
    val file = new File(base)
    file.getParentFile.mkdirs()
    file.createNewFile()
    val content = realFetch.get(url)
    Files.write(file.toPath, content.getBytes("UTF-8"))
    content
  }
}
