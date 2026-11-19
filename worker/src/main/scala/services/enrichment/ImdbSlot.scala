package services.enrichment

import models.SourceData

import java.util.Locale

/** How IMDb's details of a film become its `SourceData(Imdb)` slot. */
object ImdbSlot {

  /** Translate an `ImdbClient.Details` into a `SourceData` slot — only the content fields (director, cast, runtime,
   *  year, countries, poster, title, originalTitle). Synopsis is intentionally left empty: IMDb's plot is always
   *  English and a Polish audience shouldn't see it bleed through the merged-synopsis "longest wins" rule. Returns None
   *  when every content field is empty, so a rating-only GraphQL response (the legacy `lookup` shape that recorded
   *  fixtures replay) doesn't churn an empty slot onto the record.
   *
   *  IMDb's `countriesOfOrigin.text` is always English ("United States", "United Kingdom"); they are canonicalised
   *  into the deployment's `language` so the merged dedup operates on the same strings the other sources write
   *  (Polish "USA"/"Wielka Brytania" on `kinowo`, English as-is elsewhere). */
  def of(d: ImdbClient.Details, language: Locale): Option[SourceData] = {
    val slot = SourceData(
      title          = d.title,
      originalTitle  = d.originalTitle,
      cast           = d.cast,
      director       = d.director,
      runtimeMinutes = d.runtimeMinutes,
      releaseYear    = d.releaseYear,
      countries      = models.SlotFields.countries(d.countries, language),
      posterUrl      = d.posterUrl
    )
    val hasContent = slot.title.isDefined || slot.originalTitle.isDefined ||
                     slot.cast.nonEmpty || slot.director.nonEmpty || slot.runtimeMinutes.isDefined ||
                     slot.releaseYear.isDefined || slot.countries.nonEmpty ||
                     slot.posterUrl.isDefined
    if (hasContent) Some(slot) else None
  }
}
