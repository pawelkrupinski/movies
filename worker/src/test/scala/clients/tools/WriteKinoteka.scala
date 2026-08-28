package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinotekaClient
import services.movies.SingleCountryNormalizer.titleNormalizer

object WriteKinoteka {
  def main(args: Array[String]): Unit = {
    val client = new KinotekaClient(new RecordingHttpFetch("kinoteka", new RealHttpFetch()), titles = titleNormalizer)
    client.fetch().foreach(println)
  }
}
