package clients.tools

import clients.MultikinoClient

import java.nio.file.{Files, Paths}

object WriteMultikino extends App {
  private val fixtureRoot = "test/resources/fixtures/multikino"
  private val apiPath     = "www.multikino.pl/api/microservice/showings/cinemas/0011/films"

  private val json = MultikinoClient.DefaultFetch.get(MultikinoClient.ApiUrl)
  private val dest = Paths.get(s"$fixtureRoot/$apiPath")
  Files.createDirectories(dest.getParent)
  Files.write(dest, json.getBytes("UTF-8"))

  println(s"Recorded ${json.length} bytes → $dest")
  new MultikinoClient().fetch().foreach(m => println(s"${m.movie.title} (${m.showtimes.size} showtimes)"))
}
