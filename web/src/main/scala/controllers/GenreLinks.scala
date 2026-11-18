package controllers

import models.{City, CityPath}

object GenreLinks {

  /** A link minted when venues printed their own spelling of a genre ("?genre=przygoda") 301s onto the
   *  vocabulary's ("?genre=Przygodowy", [[models.GenreNames]]), query otherwise intact: the
   *  genre filter and the browse page match the stored names exactly, so the old spelling would
   *  narrow the listing to nothing. The address to 301 onto, or `None` when every genre in the query is already canonical. */
  def canonical(c: City, tail: String, queryString: Map[String, Seq[String]]): Option[String] = {
    val genreKeys = Set("genre", "gatunek")
    def canonical(value: String) = models.GenreNames.canonical(value, c.country.language)
    val changed = queryString.exists { case (key, values) => genreKeys(key) && values.exists(v => canonical(v) != v) }
    Option.when(changed) {
      def enc(s: String) = java.net.URLEncoder.encode(s, java.nio.charset.StandardCharsets.UTF_8)
      val query = queryString.toSeq.sortBy(_._1).flatMap { case (key, values) =>
        values.map(v => s"${enc(key)}=${enc(if (genreKeys(key)) canonical(v) else v)}")
      }.mkString("&")
      s"${CityPath(c)}/$tail?$query"
    }
  }
}
