package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.UjazdowskiClient

object WriteUjazdowski {
  def main(args: Array[String]): Unit = {
    val client = new UjazdowskiClient(new RecordingHttpFetch("ujazdowski", new RealHttpFetch()))
    val movies = client.fetch()
    movies.foreach(println)
    // Also record each film's detail page so the spec's fetchFilmDetail
    // (synopsis + bracketed original title) tests have fixtures to replay.
    movies.flatMap(_.filmUrl).foreach(client.fetchFilmDetail)
  }
}
