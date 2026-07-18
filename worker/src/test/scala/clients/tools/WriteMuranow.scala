package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.MuranowClient

object WriteMuranow {
  def main(args: Array[String]): Unit = {
    val client = new MuranowClient(new RecordingHttpFetch("kino-muranow", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
