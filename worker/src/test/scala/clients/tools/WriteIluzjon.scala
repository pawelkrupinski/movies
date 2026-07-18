package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.IluzjonClient

object WriteIluzjon {
  def main(args: Array[String]): Unit = {
    val client = new IluzjonClient(new RecordingHttpFetch("iluzjon", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
