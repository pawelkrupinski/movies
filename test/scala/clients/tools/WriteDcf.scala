package clients.tools

import services.cinemas.DcfClient
import tools.RealHttpFetch

object WriteDcf {
  def main(args: Array[String]): Unit = {
    val client = new DcfClient(new RecordingHttpFetch("dcf", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
