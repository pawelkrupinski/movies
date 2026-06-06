package clients.tools

import services.cinemas.KinoGramClient
import tools.RealHttpFetch

object WriteKinoGram {
  def main(args: Array[String]): Unit = {
    val client = new KinoGramClient(new RecordingHttpFetch("kinogram", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
