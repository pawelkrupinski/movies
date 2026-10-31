package services.resolution

/**
 * The title strings a film is searched for under, expanded from what the cinemas
 * printed. Pure — no normaliser, no lookups — so the same candidate set is built
 * for the movies path, the staging path and the IMDb id recovery alike.
 */
object SearchTitles {
  // Compiled once: `candidates` runs for every title of every record the identity model reads.
  private val PipeSeparator       = java.util.regex.Pattern.compile("""\s+\|\s+""")
  private val DashedTitle         = java.util.regex.Pattern.compile(""".*\s[-–—]\s.*""")
  // Not between two numbers: "1979 – 1989" is a range, and split there "1989" would be a piece of the title.
  private val DashSeparator       = java.util.regex.Pattern.compile("""(?<![\d\s])\s+[-–—]\s+|\s+[-–—]\s++(?!\d)""")
  private val TrailingParenthesis = java.util.regex.Pattern.compile("""\s*\([^)]*\)\s*$""")
  /** Search candidates for a row, in priority order: the row's title, the
   *  cinema-provided original title, then every other reported title (the row's
   *  cinema titles + per-slot original titles). Each is additionally expanded with
   *  its de-decorated forms — every side of a `" | "` festival/preview split
   *  ("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", "WTF Fest | Stolik kawowy"), every side
   *  of a spaced dash ("Ladies Night - Narodziny gwiazdy"; never a year range's,
   *  "1979 – 1989"), the part after a
   *  `"Series: Film"` programme-banner colon ("Akademia Kina Polskiego:
   *  Człowiek z żelaza (1981) 4K" → "Człowiek z żelaza (1981) 4K") and the
   *  trailing-parenthetical-stripped form ("Ojczyzna (pokaz przedpremierowy)" →
   *  "Ojczyzna"). Blanks and duplicates collapse. Callers must verify a hit found via one
   *  of these derived fragments — they are shorter, more generic strings than any
   *  title a cinema printed (see [[wholeCandidates]]). */
  def candidates(title: String, originalTitle: Option[String], extraTitles: Iterable[String] = Nil): Seq[String] = {
    def deDecorate(certified: String): Seq[String] = {
      // without the BBFC certificate a box office bills behind it ("Obsession (18)", "Digger - Cert 15"): no part of any
      // title, and searched whole it finds what the number names, "80 Day Obsession: Day 18 AAA" for "Obsession (18)"
      val t = services.billing.BillingParse(certified).uncertified
      val pipeParts       = if (t.contains(" | ")) PipeSeparator.split(t).toIndexedSeq else Nil
      // A programme banner is joined with a DASH as often as a pipe or a colon, and
      // the film's own title is the part after it. Hyphen, en dash and em dash all
      // appear; the surrounding spaces are what mark it as a separator rather than
      // a hyphenated word ("Spider-Man" is untouched). Purely ADDITIVE — the
      // undivided title stays a candidate — and a candidate only ever becomes a
      // resolution by matching, so an over-eager split costs nothing.
      val dashParts       = if (DashedTitle.matcher(t).matches()) DashSeparator.split(t).toIndexedSeq else Nil
      // A programme banner is also introduced with a ": " prefix ("Akademia
      // Kina Polskiego: Człowiek z żelaza", "Modoteka: Tootsie"). Only the
      // FIRST colon is used, so one further into a legitimate title ("Kill
      // Bill: Vol. 2: Redux") doesn't lose its undivided form (already kept
      // via `Seq(t)`) — this only ADDS the post-banner half as an extra
      // candidate. A colon with nothing meaningful on either side (leading
      // or trailing) is skipped.
      //
      // Deliberately COLON-ONLY, not period — a ". " split was tried and
      // reverted (2026-09-15): unlike a colon, a mid-title ". " is a real,
      // fairly common Polish film-title punctuation choice, not a reliable
      // banner signal. "Vincent. Legenda oceanu" is a film's OWN stylised
      // title (period included), not "Vincent" the banner + "Legenda oceanu"
      // the film — splitting it manufactured a spurious "Legenda oceanu…"
      // candidate that raced the ALREADY-AMBIGUOUS bare title (TMDB itself
      // returns different films for "Vincent. Legenda oceanu" depending on
      // which year happens to be attached at resolution time) and made a
      // `PolandConvergenceSpec` settle-on-a-settled-corpus run fold a row
      // that hadn't folded on the previous pass — exactly the order-dependence
      // this file's resolution architecture exists to prevent. A colon
      // essentially never appears mid-title in this corpus's real titles, so
      // it doesn't carry the same risk; a period does, so it's not worth it.
      def afterBanner(separator: String): Option[String] = t.indexOf(separator) match {
        case idx if idx > 0 =>
          val post = t.substring(idx + separator.length).trim
          if (post.nonEmpty) Some(post) else None
        case _ => None
      }
      val bannerParts     = afterBanner(": ").toSeq
      val noTrailingParen = withoutTrailingParenthesis(t)
      (Seq(t) ++ pipeParts ++ dashParts ++ bannerParts :+ noTrailingParen)
    }
    (Seq(title) ++ originalTitle.toSeq ++ extraTitles)
      .map(_.trim).filter(_.nonEmpty).distinct
      .flatMap(deDecorate)
      .map(_.trim).filter(_.nonEmpty).distinct
  }

  /** `title` without its trailing parenthetical: "Ojczyzna (pokaz przedpremierowy)" is "Ojczyzna", "Black Water (GLOBALE)"
   *  "Black Water" -- one of [[candidates]]' de-decorated forms. */
  def withoutTrailingParenthesis(title: String): String = TrailingParenthesis.matcher(title).replaceAll("").trim

  /** The subset of [[candidates]] that are the row's own titles exactly as
   *  reported — `title`, `originalTitle` and `extraTitles`, none of them split
   *  apart. Lets a caller whose acceptance check has no title-relevance
   *  verification of its own (the since-deleted `TmdbCandidateSearch.searchUnique`'s director-less
   *  branch) tell a de-decorated FRAGMENT (needs the extra check — see there)
   *  from one of the row's own complete titles (trusted outright: no text-based
   *  check could ever confirm a genuine cross-language TRANSLATION like
   *  "Посіпаки і Монстряки" for "Minionki i straszydła", which is exactly why one
   *  must not be asked of it). */
  def wholeCandidates(title: String, originalTitle: Option[String], extraTitles: Iterable[String] = Nil): Seq[String] =
    (Seq(title) ++ originalTitle.toSeq ++ extraTitles).map(_.trim).filter(_.nonEmpty).distinct
}
