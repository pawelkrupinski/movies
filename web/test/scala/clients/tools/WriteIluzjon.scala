package clients.tools

import services.cinemas.IluzjonClient
import tools.RealHttpFetch

object WriteIluzjon {
  def main(args: Array[String]): Unit = {
    val client = new IluzjonClient(new RecordingHttpFetch("iluzjon", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
