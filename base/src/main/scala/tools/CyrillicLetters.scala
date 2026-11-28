package tools

/** The Cyrillic letters every transliteration scheme of the repository spells alike: [[TextNormalization]]'s Ukrainian-leaning
 *  search romanization and the identity measures' title-key romanization each add their own spelling of the rest (г, є, ё, ї, й,
 *  ю, я). One table of the shared letters, so the two cannot drift apart where they agree. */
object CyrillicLetters {

  val Shared: Map[Char, String] = Map(
    'а' -> "a", 'б' -> "b", 'в' -> "v", 'ґ' -> "g", 'д' -> "d", 'е' -> "e", 'ж' -> "zh", 'з' -> "z", 'и' -> "y", 'і' -> "i",
    'к' -> "k", 'л' -> "l", 'м' -> "m", 'н' -> "n", 'о' -> "o", 'п' -> "p", 'р' -> "r", 'с' -> "s", 'т' -> "t", 'у' -> "u",
    'ф' -> "f", 'х' -> "kh", 'ц' -> "ts", 'ч' -> "ch", 'ш' -> "sh", 'щ' -> "shch", 'ъ' -> "", 'ы' -> "y", 'ь' -> "", 'э' -> "e")
}
