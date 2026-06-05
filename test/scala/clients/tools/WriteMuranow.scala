package clients.tools

import services.cinemas.MuranowClient
import tools.RealHttpFetch

object WriteMuranow {
  def main(args: Array[String]): Unit = {
    val client = new MuranowClient(new RecordingHttpFetch("kino-muranow", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
