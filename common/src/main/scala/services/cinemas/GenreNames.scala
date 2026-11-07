package services.cinemas

import java.util.Locale

/**
 * The ONE master genre set. Each [[Genre]] has a language-independent id and a display name per
 * deployment language (pl, en, de, es — TMDB's own names for the TMDB genres), and every spelling
 * a source prints for it — TMDB's and Filmweb's names in each language, every venue's strings —
 * is an alias of its id in [[Aliases]], matched case-insensitively and across languages.
 *
 * [[canonical]] runs wherever a genre enters the pipeline (a cinema slot in `SlotFields.genres`,
 * the TMDB and Filmweb slots) and again where a stored row's genres are read for the read model
 * (`MovieRecord.genres`: a slot stored before an alias existed), so stored data, the genre filter and its URLs, the page's pills and
 * the JSON API all carry the deployment language's name of the id; the API also serves the ids
 * ([[idOf]]). A string no genre claims keeps its words with a capital first letter (all-capitals
 * text is lowercased first: "KONCERT" -> "Koncert") and has no id.
 */
object GenreNames {

  final case class Genre(id: String, pl: String, en: String, de: String, es: String) {
    def name(language: Locale): String = language.getLanguage match {
      case "pl" => pl
      case "de" => de
      case "es" => es
      case _    => en
    }
  }

  /** TMDB's 19 movie genres, then what venues add. */
  val All: Seq[Genre] = Seq(
    Genre("action",      "Akcja",         "Action",          "Action",         "Acción"),
    Genre("adventure",   "Przygodowy",    "Adventure",       "Abenteuer",      "Aventura"),
    Genre("animation",   "Animacja",      "Animation",       "Animation",      "Animación"),
    Genre("comedy",      "Komedia",       "Comedy",          "Komödie",        "Comedia"),
    Genre("crime",       "Kryminał",      "Crime",           "Krimi",          "Crimen"),
    Genre("documentary", "Dokumentalny",  "Documentary",     "Dokumentarfilm", "Documental"),
    Genre("drama",       "Dramat",        "Drama",           "Drama",          "Drama"),
    Genre("family",      "Familijny",     "Family",          "Familie",        "Familia"),
    Genre("fantasy",     "Fantasy",       "Fantasy",         "Fantasy",        "Fantasía"),
    Genre("history",     "Historyczny",   "History",         "Historie",       "Historia"),
    Genre("horror",      "Horror",        "Horror",          "Horror",         "Terror"),
    Genre("music",       "Muzyczny",      "Music",           "Musik",          "Música"),
    Genre("mystery",     "Tajemnica",     "Mystery",         "Mystery",        "Misterio"),
    Genre("romance",     "Romans",        "Romance",         "Liebesfilm",     "Romance"),
    Genre("scifi",       "Sci-Fi",        "Science Fiction", "Science Fiction", "Ciencia ficción"),
    Genre("tvmovie",     "Film TV",       "TV Movie",        "TV-Film",        "Película de TV"),
    Genre("thriller",    "Thriller",      "Thriller",        "Thriller",       "Suspense"),
    Genre("war",         "Wojenny",       "War",             "Kriegsfilm",     "Bélica"),
    Genre("western",     "Western",       "Western",         "Western",        "Western"),
    Genre("opera",       "Opera",         "Opera",           "Oper",           "Ópera"),
    Genre("concert",     "Koncert",       "Concert",         "Konzert",        "Concierto"),
    Genre("ballet",      "Balet",         "Ballet",          "Ballett",        "Ballet"),
    Genre("theatre",     "Teatr",         "Theatre",         "Theater",        "Teatro"),
    Genre("musical",     "Musical",       "Musical",         "Musical",        "Musical"),
    Genre("biography",   "Biograficzny",  "Biography",       "Biographie",     "Biografía"),
    Genre("short",       "Krótkometrażowy", "Short",         "Kurzfilm",       "Cortometraje"),
    Genre("sport",       "Sportowy",      "Sport",           "Sport",          "Deporte"),
    Genre("live",        "Na żywo",       "Live performance", "Liveveranstaltung", "En directo"),
    Genre("event",       "Wydarzenie",    "Event",           "Special Event",  "Evento"),
    Genre("comedydrama", "Komediodramat", "Comedy-drama",    "Tragikomödie",   "Tragicomedia"),
    Genre("erotic",      "Erotyczny",     "Erotic",          "Erotik",         "Erótico"),
  )

  /** Spellings that are not a genre's display name in some language (lowercase) -> its id. */
  private val Aliases: Map[String, String] = Map(
    "przygoda" -> "adventure", "przygodowy" -> "adventure", "przygodowe" -> "adventure",
    "dla dzieci" -> "family", "kids & family" -> "family", "kids and family" -> "family", "familie" -> "family",
    "dokument" -> "documentary", "dokumentation" -> "documentary", "dokumentalny" -> "documentary", "doku" -> "documentary",
    "animowany" -> "animation", "trickfilm" -> "animation", "zeichentrick" -> "animation", "animowane" -> "animation",
    "film telewizyjny" -> "tvmovie", "film tv" -> "tvmovie", "tv film" -> "tvmovie", "fernsehfilm" -> "tvmovie", "película de televisión" -> "tvmovie",
    "science fiction" -> "scifi", "sci-fi" -> "scifi", "sci fi" -> "scifi", "scifi" -> "scifi", "science-fiction" -> "scifi", "ciencia ficción" -> "scifi",
    "romanze" -> "romance", "lovestory" -> "romance", "love story" -> "romance", "melodramat" -> "romance",
    "del oeste" -> "western", "suspense" -> "thriller", "sensacyjny" -> "thriller",
    "oper" -> "opera", "klassisches konzert" -> "concert", "konzert (u)" -> "concert", "live concert" -> "concert", "jazz konzert" -> "concert",
    "tanz" -> "ballet", "balet" -> "ballet", "spektakl teatralny" -> "theatre", "teatr improwizowany" -> "theatre",
    "spektakl muzyczny" -> "musical", "biograficzny" -> "biography", "biopic" -> "biography", "biographie" -> "biography",
    "krótkometrażowy" -> "short", "kurzfilm" -> "short", "sportowy" -> "sport", "sport event" -> "sport", "sport" -> "sport",
    "live performance" -> "live", "live show" -> "live", "liveveranstaltung" -> "live", "comedy live" -> "live",
    "tragikomödie" -> "comedydrama", "komediodramat" -> "comedydrama",
    "komödie" -> "comedy", "komedia" -> "comedy", "kriegsfilm" -> "war", "bélica" -> "war",
    "special event" -> "event", "festival" -> "event", "festiwal" -> "event", "pop" -> "concert", "rock" -> "concert", "zarzuela" -> "musical", "przyrodniczy" -> "documentary", "spotkanie" -> "event", "pokaz specjalny" -> "event", "retransmisja" -> "event",
    "alternativer content" -> "event", "erotyczny" -> "erotic", "erotik" -> "erotic",
  )

  /** What a feed prints where it has no genre: not a genre, so not served as one. */
  private val Placeholders = Set("unknown", "undefiniert", "default", "laufzeit: k. a.", "n/a", "-")

  def isPlaceholder(genre: String): Boolean = Placeholders(genre.trim.toLowerCase(Locale.ROOT))

  private val ById: Map[String, Genre] = All.map(g => g.id -> g).toMap

  private val IdOfSpelling: Map[String, String] =
    Aliases ++ All.flatMap(g => Seq(g.pl, g.en, g.de, g.es).map(_.toLowerCase(Locale.ROOT) -> g.id))

  /** The id of the master genre `genre` is a spelling of, if any. */
  def idOf(genre: String): Option[String] = IdOfSpelling.get(genre.trim.toLowerCase(Locale.ROOT))

  /** `genre` as the deployment `language` names its master genre, or — outside the set — with a capital first letter. */
  def canonical(genre: String, language: Locale): String = {
    val trimmed = genre.trim
    idOf(trimmed).map(ById).map(_.name(language)).getOrElse {
      val words = if (trimmed.exists(_.isLower)) trimmed else trimmed.toLowerCase(Locale.ROOT)
      if (words.isEmpty) words else words.head.toUpper.toString + words.tail
    }
  }
}
