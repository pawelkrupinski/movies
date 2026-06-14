package clients.tools

import models.{Cinema, KinoElektronik, KinoKosmos, KinoLuna, KinoSwiatowid}
import services.cinemas.{Bilety24Client, Bilety24OrganizerClient}
import tools.RealHttpFetch

object WriteBilety24 {
  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    def rec(directory: String) = new RecordingHttpFetch(directory, real)

    // Kino Luna is still on the legacy per-venue subdomain.
    val luna = new Bilety24Client(rec("kino-luna"), "https://kinoluna.bilety24.pl", KinoLuna)
    println("=== Luna ===")
    luna.fetch().foreach(println)

    // Kosmos / Światowid / Elektronik migrated to the main-domain organizer pages.
    val migrated: Seq[(String, String, Cinema)] = Seq(
      ("kino-kosmos",     "https://www.bilety24.pl/kino/organizator/kino-kosmos-1501",    KinoKosmos),
      ("kino-swiatowid",  "https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503", KinoSwiatowid),
      ("kino-elektronik", "https://www.bilety24.pl/kino/organizator/kino-elektronik-631", KinoElektronik)
    )
    migrated.foreach { case (directory, url, cinema) =>
      println(s"=== ${cinema.displayName} ===")
      new Bilety24OrganizerClient(rec(directory), url, cinema).fetch().foreach(println)
    }
  }
}
