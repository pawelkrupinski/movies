package clients.tools

import services.cinemas.KinoBulgarskaClient
import tools.RealHttpFetch

object WriteKinoBulgarska {
  def main(args: Array[String]): Unit = {
    val client = new KinoBulgarskaClient(new RecordingHttpFetch("kino-bulgarska", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
