package clients.tools

import models.{KinoElektronik, KinoLuna}
import services.cinemas.Bilety24Client
import tools.RealHttpFetch

object WriteBilety24 {
  def main(args: Array[String]): Unit = {
    val luna = new Bilety24Client(new RecordingHttpFetch("kino-luna", new RealHttpFetch()), "https://kinoluna.bilety24.pl", KinoLuna)
    println("=== Luna ===")
    luna.fetch().foreach(println)

    val elektronik = new Bilety24Client(new RecordingHttpFetch("kino-elektronik", new RealHttpFetch()), "https://kinoelektronik.pl", KinoElektronik, "/")
    println("=== Elektronik ===")
    elektronik.fetch().foreach(println)
  }
}
