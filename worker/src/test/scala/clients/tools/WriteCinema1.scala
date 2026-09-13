package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.Cinema1Client
import models.Cinema1Gdansk

import java.time.LocalDate

object WriteCinema1 {
  def main(args: Array[String]): Unit = {
    val today = LocalDate.of(2026, 9, 13)
    new Cinema1Client(new RecordingHttpFetch("cinema1-gdansk", new RealHttpFetch()), Cinema1Gdansk,
      cinemaId = "8d3b10d9-f892-4f57-bf74-9f86905ce3ea", today = today).fetch().foreach(println)
  }
}
