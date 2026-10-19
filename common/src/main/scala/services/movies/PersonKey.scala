package services.movies

/** A person's name as an order-insensitive whole-name key: accents folded, lowercased, its
 *  words sorted, so "Makoto Shinkai" = "Shinkai Makoto". One definition for every place that
 *  asks whether two credits name the same person by name alone. */
object PersonKey {
  def of(name: String): String = TitleContainment.tokens(name).sorted.mkString(" ")
}

