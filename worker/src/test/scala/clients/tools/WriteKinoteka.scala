package clients.tools

import services.cinemas.KinotekaClient
import tools.RealHttpFetch

object WriteKinoteka {
  def main(args: Array[String]): Unit = {
    val client = new KinotekaClient(new RecordingHttpFetch("kinoteka", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
