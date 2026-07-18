package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.DcfClient

object WriteDcf {
  def main(args: Array[String]): Unit = {
    val client = new DcfClient(new RecordingHttpFetch("dcf", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
