package services.identity.vocabulary

import java.time.LocalDate

/** What dates a listing's film beside the year it states: when and as what it screens, read off its screenings and
 *  its billing, never a fact it states. Evidence and the measured listing hold it alike
 *  (`services.identity.Evidence`, `services.identity.MeasuredListing`); every field is the listing's reading of the
 *  same name (`services.identity.Listing`). */
final case class ListingDating(
  /** The season a stage relay billing neither its season nor a year screens in: searched with its work, never
   *  measured. */
  broadcastSeason: Option[Int] = None,
  /** The season its screenings fall in, whatever it states: what picks among a house's records of its work's seasons,
   *  never searched. */
  screenedSeason: Option[Int] = None,
  /** The season its first screening falls in, unconditionally ([[ScreeningDays.season]]): the seasons its title can
   *  name (`services.titles.TitleSeason.near`). */
  screeningsSeason: Option[Int] = None,
  /** Its screening days, held only where its stated year is more than a season before the season it screens in: what
   *  the air-day test reads. */
  airDays: Seq[LocalDate] = Nil,
  /** The latest year its film can be from: what weighs against a film released after it as a stated year would, never
   *  for one. */
  yearUpperBound: Option[Int] = None,
  /** The year of the performance a stage relay's synopsis names: what dates an encore's relay, never measured. */
  performanceYear: Option[Int] = None,
  /** The year its first screening falls in, where its title bills an anniversary: what dates the film of the
   *  anniversary it bills. */
  anniversaryScreeningYear: Option[Int] = None)

object ListingDating {
  /** A listing nothing dates beside the year it states. */
  val Undated: ListingDating = ListingDating()
}
