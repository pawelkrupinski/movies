package clients.tools

import services.cinemas.KinoStudioClient
import tools.RealHttpFetch

object WriteKinoStudio {
  def main(args: Array[String]): Unit = {
    val client = new KinoStudioClient(new RecordingHttpFetch("kino-studio-opole", new RealHttpFetch()))
    val films  = client.fetch()
    films.foreach(f => println(s"${f.movie.title}: ${f.showtimes.map(_.dateTime).mkString(", ")}"))
    println(s"Total: ${films.size} film(s)")
  }
}
