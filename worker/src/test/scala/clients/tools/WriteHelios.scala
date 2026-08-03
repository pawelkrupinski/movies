package clients.tools

import tools.HeliosFetch
import services.cinemas.pl.HeliosClient
import services.movies.SingleCountryNormalizer.titleNormalizer

object WriteHelios {
  def main(args: Array[String]): Unit = {
    val client = new HeliosClient(new RecordingHttpFetch("helios/missing-runtime", HeliosFetch), titles = titleNormalizer)
    client.fetch().foreach(println)
  }
}
