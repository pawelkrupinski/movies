package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinomuzeumClient

object WriteKinomuzeum {
  def main(args: Array[String]): Unit = {
    val client = new KinomuzeumClient(new RecordingHttpFetch("kinomuzeum", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
