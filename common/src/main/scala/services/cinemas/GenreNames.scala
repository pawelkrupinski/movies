package services.cinemas

import java.util.Locale

/**
 * The genre vocabulary of each deployment language — the names TMDB's `genres` field carries in
 * that language — and the one place a genre a venue printed is folded into it. A venue prints
 * "przygoda", "akcja", "dla dzieci", "film TV" where TMDB says "Przygodowy", "Akcja", "Familijny",
 * "Film TV": left alone, the same film shows lowercase pills beside its neighbours' capitalised
 * ones, and "Przygoda" and "Przygodowy" filter as two genres.
 *
 * [[canonical]] runs where a cinema slot is built (`SlotFields.genres`), so stored data, the
 * genre filter and its URLs, the page's pills and the JSON API all carry the same strings. A name
 * outside the vocabulary keeps its words with a capital first letter (all-capitals text is
 * lowercased first: "KONCERT" -> "Koncert"). Matching is case-insensitive.
 */
object GenreNames {

  private val Vocabulary: Map[String, Seq[String]] = Map(
    "pl" -> Seq("Akcja", "Animacja", "Dokumentalny", "Dramat", "Familijny", "Fantasy", "Film TV", "Historyczny", "Horror",
      "Komedia", "Kryminał", "Muzyczny", "Przygodowy", "Romans", "Sci-Fi", "Tajemnica", "Thriller", "Western", "Wojenny"),
    "en" -> Seq("Action", "Adventure", "Animation", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
      "Horror", "Music", "Mystery", "Romance", "Science Fiction", "Thriller", "TV Movie", "War", "Western"),
    "de" -> Seq("Abenteuer", "Action", "Animation", "Dokumentarfilm", "Drama", "Familie", "Fantasy", "Historie", "Horror",
      "Komödie", "Krimi", "Kriegsfilm", "Liebesfilm", "Musik", "Mystery", "Science Fiction", "Thriller", "TV-Film", "Western"),
    "es" -> Seq("Acción", "Animación", "Aventura", "Bélica", "Ciencia ficción", "Comedia", "Crimen", "Documental", "Drama",
      "Familia", "Fantasía", "Historia", "Misterio", "Música", "Película de TV", "Romance", "Suspense", "Terror", "Western")
  )

  /** Spellings venues use for a vocabulary name (lowercase key). */
  private val Aliases: Map[String, Map[String, String]] = Map(
    "pl" -> Map("przygoda" -> "Przygodowy", "dla dzieci" -> "Familijny", "dokument" -> "Dokumentalny", "animowany" -> "Animacja",
      "film telewizyjny" -> "Film TV", "science fiction" -> "Sci-Fi", "sci fi" -> "Sci-Fi"),
    "en" -> Map("kids & family" -> "Family", "sci-fi" -> "Science Fiction", "scifi" -> "Science Fiction", "tv film" -> "TV Movie"),
    "de" -> Map("dokumentation" -> "Dokumentarfilm", "sci-fi" -> "Science Fiction", "romanze" -> "Liebesfilm", "lovestory" -> "Liebesfilm",
      "trickfilm" -> "Animation", "zeichentrick" -> "Animation", "opera" -> "Oper", "film tv" -> "TV-Film", "fernsehfilm" -> "TV-Film"),
    "es" -> Map("del oeste" -> "Western", "opera" -> "Ópera", "película de televisión" -> "Película de TV", "sci-fi" -> "Ciencia ficción")
  )

  private val Lookup: Map[String, Map[String, String]] = Vocabulary.map { case (lang, names) =>
    lang -> (names.map(n => n.toLowerCase(Locale.ROOT) -> n).toMap ++ Aliases.getOrElse(lang, Map.empty))
  }

  /** `genre` as the deployment `language`'s vocabulary spells it, or — outside it — with a capital first letter. */
  def canonical(genre: String, language: Locale): String = {
    val trimmed = genre.trim
    val key     = trimmed.toLowerCase(Locale.ROOT)
    Lookup.get(language.getLanguage).flatMap(_.get(key)).getOrElse {
      val words = if (trimmed.exists(_.isLower)) trimmed else key
      if (words.isEmpty) words else words.head.toUpper.toString + words.tail
    }
  }
}
