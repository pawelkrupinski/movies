package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.{PromKepaClient, SwitClient}

object WriteSwitPromKepa {
  def main(args: Array[String]): Unit = {
    println("=== Świt ===")
    new SwitClient(new RecordingHttpFetch("kino-swit", new RealHttpFetch())).fetch().foreach(println)
    println("=== Kępa ===")
    new PromKepaClient(new RecordingHttpFetch("kino-kepa", new RealHttpFetch())).fetch().foreach(println)
  }
}
