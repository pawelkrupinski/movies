package clients.tools

import services.cinemas.KinoPalacoweClient
import tools.RealHttpFetch

object WriteKinoPalacowe {
  def main(args: Array[String]): Unit = {
    val client = new KinoPalacoweClient(new RecordingHttpFetch("kino-palacowe", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
