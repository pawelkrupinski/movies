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
   *  of a spaced dash ("Ladies Night - Narodziny gwiazdy"), the part after a
   *  `"Series: Film"` or `"Series. Film"` programme banner ("Akademia Kina
   *  Polskiego: Człowiek z żelaza (1981) 4K" → "Człowiek z żelaza (1981) 4K";
   *  double-decorated banners split on both separators independently, so
   *  "3 wieczory: kieślowski. Blizna" also yields "Blizna") and the
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
      // A programme banner is also introduced with a ": " or ". " prefix
      // ("Akademia Kina Polskiego: Człowiek z żelaza", "Modoteka: Tootsie",
      // "3 wieczory: kieślowski. Blizna" — colon AND period banners stacked).
      // Only the FIRST occurrence of each separator is used, so a colon further
      // into a legitimate title ("Kill Bill: Vol. 2: Redux") doesn't lose its
      // undivided form (already kept via `Seq(t)`) — this only ADDS the
      // post-banner half as an extra candidate. Both separators are tried
      // independently against the undivided `t`, not chained, so a
      // double-decorated banner yields the fully-stripped film title in one step.
      // A separator with nothing meaningful on either side (leading or trailing)
      // is skipped.
      def afterBanner(separator: String): Option[String] = t.indexOf(separator) match {
        case idx if idx > 0 =>
          val post = t.substring(idx + separator.length).trim
          if (post.nonEmpty) Some(post) else None
        case _ => None
      }
      val bannerParts     = Seq(afterBanner(": "), afterBanner(". ")).flatten
      val noTrailingParen = t.replaceAll("""\s*\([^)]*\)\s*$""", "").trim
      (Seq(t) ++ pipeParts ++ dashParts ++ bannerParts :+ noTrailingParen)
    }
    (Seq(title) ++ originalTitle.toSeq ++ extraTitles)
      .map(_.trim).filter(_.nonEmpty).distinct
      .flatMap(deDecorate)
      .map(_.trim).filter(_.nonEmpty).distinct
  }
}
