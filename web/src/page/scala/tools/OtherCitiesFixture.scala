package tools

/** Two sibling-city links for a `/movie` render of the fixture corpus, which only
 *  Poznań carries — so no corpus film is ever showing in another city — to drive
 *  the "other cities" popup in the page suites (`PageJsBehaviourSpec`, and the
 *  fixture server's `/{city}/movie-other-cities` for Playwright). */
object OtherCitiesFixture {
  val links: Seq[(models.City, String)] = Seq(
    models.Wroclaw  -> "/wroclaw/movie/example-slug",
    models.Warszawa -> "/warszawa/movie/example-slug",
  )
}
