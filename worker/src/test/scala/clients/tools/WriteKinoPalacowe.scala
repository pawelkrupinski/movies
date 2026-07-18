package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoPalacoweClient

object WriteKinoPalacowe {
  def main(args: Array[String]): Unit = {
    val client = new KinoPalacoweClient(new RecordingHttpFetch("kino-palacowe", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
