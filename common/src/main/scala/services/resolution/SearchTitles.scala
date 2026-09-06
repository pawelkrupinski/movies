package services.resolution

/**
 * The title strings a film is searched for under, expanded from what the cinemas
 * printed. Pure — no normaliser, no lookups — so the same candidate set is built
 * for the movies path, the staging path and the IMDb id recovery alike.
 */
object SearchTitles {

  /** Search candidates for a row, in priority order: the row's title, the
   *  cinema-provided original title, then every other reported title (the row's
   *  cinema titles + per-slot original titles). Each is additionally expanded with
   *  its de-decorated forms — every side of a `" | "` festival/preview split
   *  ("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", "WTF Fest | Stolik kawowy"), every side
   *  of a spaced dash ("Ladies Night - Narodziny gwiazdy") and the
   *  trailing-parenthetical-stripped form ("Ojczyzna (pokaz przedpremierowy)" →
   *  "Ojczyzna"). Blanks and duplicates collapse. Callers verify each hit, so an
   *  extra candidate can't mis-resolve onto a same-title different film. */
  def candidates(title: String, originalTitle: Option[String], extraTitles: Iterable[String] = Nil): Seq[String] = {
    def deDecorate(t: String): Seq[String] = {
      val pipeParts       = if (t.contains(" | ")) t.split("""\s+\|\s+""").toIndexedSeq else Nil
      // A programme banner is joined with a DASH as often as a pipe or a colon, and
      // the film's own title is the part after it. Hyphen, en dash and em dash all
      // appear; the surrounding spaces are what mark it as a separator rather than
      // a hyphenated word ("Spider-Man" is untouched). Purely ADDITIVE — the
      // undivided title stays a candidate — and a candidate only ever becomes a
      // resolution by matching, so an over-eager split costs nothing.
      val dashParts       = if (t.matches(""".*\s[-–—]\s.*""")) t.split("""\s+[-–—]\s+""").toIndexedSeq else Nil
      val noTrailingParen = t.replaceAll("""\s*\([^)]*\)\s*$""", "").trim
      (Seq(t) ++ pipeParts ++ dashParts :+ noTrailingParen)
    }
    (Seq(title) ++ originalTitle.toSeq ++ extraTitles)
      .map(_.trim).filter(_.nonEmpty).distinct
      .flatMap(deDecorate)
      .map(_.trim).filter(_.nonEmpty).distinct
  }
}
