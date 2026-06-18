package services.movies

import models.Cinema
import services.titlerules.{TitleRuleKey, TitleRuleSet}
import tools.TextNormalization

/** Pure computation of how a rule set groups cinema titles into merged rows —
 *  reused by the admin "what will merge" preview (run against a DRAFT rule set,
 *  no persistence) and by the backfill engine (run against the saved set, then
 *  applied). Given the raw per-cinema titles, it reproduces the cache's merge
 *  key exactly: `sanitize(perCinema(ruleKey, rawTitle))` paired with the year.
 *
 *  Working off the RAW titles + an arbitrary rule set is what makes the preview
 *  honest: it shows what WOULD happen under rules that aren't installed yet. */
object RuleMergePreview {

  /** One cinema's contribution: the raw scraped title, the cinema's rule key,
   *  and the row year. */
  final case class Entry(ruleKey: String, rawTitle: String, year: Option[Int])

  /** A set of distinct RAW cinema titles that collapse onto one merge key — the
   *  rows that become one. `display` is the per-cinema-cleaned SEARCH title (the
   *  merge anchor); `displayTitle` is what the merged row would actually RENDER —
   *  the cleaned spellings run through the shared display ladder (dominant
   *  identity → `preferredDisplay` → `recase`), i.e. after every replacement and
   *  the display capitalisation. TMDB isn't reachable in the preview, so the
   *  TMDB-Polish-title step of the live ladder is skipped. */
  final case class Group(key: String, year: Option[Int], titles: Seq[String],
                         display: String, displayTitle: String)

  /** The merge key a rule set assigns to a raw title — the `sanitize`-equivalent
   *  canonical string (year handled separately by the caller). Mirrors
   *  `MovieCache.keyOf` + `CacheKey` equality, but parameterised on the rule set
   *  so a draft can be evaluated without installing it. */
  def mergeKey(rules: TitleRuleSet, ruleKey: String, rawTitle: String): String = {
    val display = rules.perCinema(ruleKey, rawTitle)
    TextNormalization.deburr(rules.canonical(TitleNormalizer.normalize(display)))
      .toLowerCase
      .replaceAll("[^\\p{L}\\p{N}]+", "")
  }

  /** Search title (per-cinema cleanup) a rule set assigns to a raw title — the
   *  merge anchor, before the display ladder / casing. */
  def display(rules: TitleRuleSet, ruleKey: String, rawTitle: String): String =
    rules.perCinema(ruleKey, rawTitle)

  /** Every group of >1 distinct RAW title that share a (key, year) under `rules`
   *  — i.e. distinct cinema titles that become one row. Deterministic ordering. */
  def groups(rules: TitleRuleSet, entries: Seq[Entry]): Seq[Group] =
    entries
      .groupBy(e => (mergeKey(rules, e.ruleKey, e.rawTitle), e.year))
      .iterator
      .map { case ((key, year), es) =>
        val raws        = es.map(_.rawTitle).distinct.sorted
        // One cleaned vote per cinema slot — mirrors MovieRecord.displayTitle's
        // per-cinema vote pool — fed through the shared display ladder.
        val cleaned     = es.map(e => display(rules, e.ruleKey, e.rawTitle))
        val searchTitle = display(rules, es.head.ruleKey, es.head.rawTitle)
        Group(key, year, raws, searchTitle, TitleNormalizer.chooseDisplay(cleaned, searchTitle))
      }
      .filter(_.titles.sizeIs > 1)
      .toSeq
      .sortBy(g => (g.key, g.year.map(_.toString).getOrElse("")))

  /** Groups present under `draft` but NOT under `current` — the NEW merges an
   *  edit would cause (keyed by the same (key, year), with a wider title set). */
  def newMerges(current: TitleRuleSet, draft: TitleRuleSet, entries: Seq[Entry]): Seq[Group] = {
    val before = groups(current, entries).map(g => (g.key, g.year) -> g.titles.toSet).toMap
    groups(draft, entries).filter { g =>
      before.get((g.key, g.year)).forall(previous => g.titles.toSet != previous)
    }
  }

  /** Build preview entries from the live corpus: one per cinema slot that has a
   *  raw title. */
  def entriesFrom(records: Seq[StoredMovieRecord]): Seq[Entry] =
    records.flatMap { r =>
      r.record.cinemaData.collect {
        case (cinema: Cinema, slot) if slot.rawTitle.exists(_.nonEmpty) =>
          Entry(TitleRuleKey.of(cinema), slot.rawTitle.get, r.year)
      }
    }
}
