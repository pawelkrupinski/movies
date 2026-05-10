package clients

object MultikinoScraperRunner extends App {

  println("Fetching Multikino repertoire (Poznań Stary Browar)...")
  println()

  val showtimes = MultikinoScraper.scrape()

  if (showtimes.isEmpty) {
    println("No showtimes found.")
  } else {
    val byTitle = showtimes.groupBy(_.movieTitle)

    byTitle.toSeq.sortBy(_._1).foreach { case (title, entries) =>
      val bar = "─" * (60 - title.length - 4).max(2)
      println(s"── $title $bar")
      println()

      entries.sortBy(_.date).foreach { s =>
        println(s"  ${s.date}  ${s.times.mkString("  ")}")
      }
      println()
    }

    val totalScreenings = showtimes.map(_.times.size).sum
    println("─" * 60)
    println(s"${byTitle.size} title(s)  |  ${showtimes.size} showtime group(s)  |  $totalScreenings screening slot(s)")
  }
}
