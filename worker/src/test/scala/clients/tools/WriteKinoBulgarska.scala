package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoBulgarskaClient

object WriteKinoBulgarska {
  def main(args: Array[String]): Unit = {
    val client = new KinoBulgarskaClient(new RecordingHttpFetch("kino-bulgarska", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
