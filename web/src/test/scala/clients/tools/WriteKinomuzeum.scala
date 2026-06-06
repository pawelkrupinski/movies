package clients.tools

import services.cinemas.KinomuzeumClient
import tools.RealHttpFetch

object WriteKinomuzeum {
  def main(args: Array[String]): Unit = {
    val client = new KinomuzeumClient(new RecordingHttpFetch("kinomuzeum", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
