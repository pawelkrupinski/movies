package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoGramClient

object WriteKinoGram {
  def main(args: Array[String]): Unit = {
    val client = new KinoGramClient(new RecordingHttpFetch("kinogram", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
