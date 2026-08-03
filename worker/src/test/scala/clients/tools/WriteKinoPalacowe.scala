package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoPalacoweClient
import services.movies.SingleCountryNormalizer.titleNormalizer

object WriteKinoPalacowe {
  def main(args: Array[String]): Unit = {
    val client = new KinoPalacoweClient(new RecordingHttpFetch("kino-palacowe", new RealHttpFetch()), titles = titleNormalizer)
    client.fetch().foreach(println)
  }
}
