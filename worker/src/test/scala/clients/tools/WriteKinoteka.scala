package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinotekaClient

object WriteKinoteka {
  def main(args: Array[String]): Unit = {
    val client = new KinotekaClient(new RecordingHttpFetch("kinoteka", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
