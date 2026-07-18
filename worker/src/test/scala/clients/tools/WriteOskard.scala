package clients.tools

import models.KinoOskard
import tools.RealHttpFetch
import services.cinemas.pl.Bilety24Client

object WriteOskard {
  def main(args: Array[String]): Unit = {
    val oskard = new Bilety24Client(new RecordingHttpFetch("kino-oskard", new RealHttpFetch()), "https://ckis-konin.bilety24.pl", KinoOskard)
    println("=== Oskard ===")
    oskard.fetch().foreach(println)
  }
}
