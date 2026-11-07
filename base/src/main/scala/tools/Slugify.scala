package tools

import java.util.Locale
import java.util.regex.Pattern

/** Fold an arbitrary human-readable string into a URL-safe `a-z0-9-` slug.
 *
 *  Two callers today, which is why it lives here rather than staying private to
 *  either: [[services.titlerules.TitleRuleKey]] keys a standalone cinema's
 *  cleanup rules on a slug of its display name, and `controllers.FilmHref`
 *  builds the canonical `/{city}/film/{slug}` address of a film page. Both need
 *  the identical fold — the moment they drift, a rule key or a permalink
 *  silently stops resolving.
 *
 *  The fold is deliberately lossy and NOT reversible: punctuation, case, and
 *  diacritics all collapse. Callers that need to get back to the original
 *  string must resolve the slug against a candidate set (which is what the film
 *  page does — it re-slugs each title the city is showing and compares), never
 *  attempt to un-slug.
 *
 *  The two callers want the same hyphenation but NOT the same folding policy,
 *  so the shared mechanism is split from the two policies rather than
 *  duplicated:
 *
 *  - [[stable]] is a FROZEN key space. `TitleRuleKey` persists rules against
 *    its output, so a new fold here would re-key existing cinemas and orphan
 *    their rules. 15 German venues carry `ß` ("Kino Weißhaus", "Filmpalast
 *    Meißen", …) and would move under the URL policy's ß→ss. Never extend it.
 *  - [[apply]] is the URL policy: `stable` plus the folds that make a permalink
 *    readable. Nothing persists it — a film slug is resolved by re-slugging the
 *    titles a city is showing and comparing — so it stays free to improve.
 *
 *  Both are deliberately lossy and NOT reversible: punctuation, case, and
 *  diacritics all collapse. Callers must resolve a slug against a candidate
 *  set, never attempt to un-slug.
 */
object Slugify {

  private val NonAlphanumericRun = Pattern.compile("[^a-z0-9]+")
  private val EdgeDashes         = Pattern.compile("(^-|-$)")

  /** The frozen fold: deburr → lowercase → hyphenate → trim. Byte-stable by
   *  contract; see the class comment before adding anything to it. */
  def stable(s: String): String = {
    val folded = TextNormalization.deburr(s).toLowerCase(Locale.ROOT)
    EdgeDashes.matcher(NonAlphanumericRun.matcher(folded).replaceAll("-")).replaceAll("")
  }

  /** The URL fold. Cyrillic is romanized first so a Ukrainian-dubbed title
   *  ("Ваяна") yields a usable "vaiana" rather than folding away to the empty
   *  string; `ß` becomes `ss` because NFD leaves it intact and the hyphenation
   *  step would otherwise drop it, turning "Große Freiheit" into
   *  "groe-freiheit".
   *
   *  @return the slug, or `""` when `s` holds nothing that survives the fold
   *          (all punctuation, or a script neither Latin nor Cyrillic). Callers
   *          that need a non-empty URL segment must handle that case. */
  def apply(s: String): String =
    stable(TextNormalization.romanizeCyrillic(s).replace("ß", "ss").replace("ẞ", "ss"))
}
