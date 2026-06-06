package clients.tools

import services.cinemas.{PromKepaClient, SwitClient}
import tools.RealHttpFetch

object WriteSwitPromKepa {
  def main(args: Array[String]): Unit = {
    println("=== Świt ===")
    new SwitClient(new RecordingHttpFetch("kino-swit", new RealHttpFetch())).fetch().foreach(println)
    println("=== Kępa ===")
    new PromKepaClient(new RecordingHttpFetch("kino-kepa", new RealHttpFetch())).fetch().foreach(println)
  }
}
