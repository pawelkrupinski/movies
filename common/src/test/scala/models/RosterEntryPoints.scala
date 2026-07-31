package models

/**
 * The two entry points [[RosterInitOrderSpec]] forks a JVM for. Each touches exactly ONE of
 * the two mutually-initialising objects and nothing else, because the thing under test is
 * which of them the JVM reaches first — anything else in the process could initialise the
 * other one and quietly make the check vacuous.
 *
 * `def main` rather than `extends App`: an `App` body runs as delayed initialisation, so a
 * failure surfaces as an object-init error rather than the exception itself.
 */
object TouchGermanRosterFirst {
  def main(args: Array[String]): Unit = {
    require(GermanRoster.byCity.nonEmpty, "German roster came out empty")
    println("roster-ok")
  }
}

object TouchCinemaFirst {
  def main(args: Array[String]): Unit = {
    require(Cinema.all.nonEmpty, "Cinema roster came out empty")
    println("cinema-ok")
  }
}
