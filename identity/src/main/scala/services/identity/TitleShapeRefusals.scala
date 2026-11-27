package services.identity

import services.identity.measures.{Film, IdentityMeasures, ListingShape, MeasuredListing, MultiFilmBill}
import services.identity.vocabulary.Denial

/** The films a listing's title SHAPE rules out, whatever else it bills ([[ListingDenials.evidenceDenial]] asks them after
 *  its stage house's billing): an old catalogue namesake of a bare billing under a film festival's banner
 *  ([[ListingShape.festivalsCatalogueNamesake]]), one work of a "//" double bill ([[MultiFilmBill.slashesAnotherWork]]), a
 *  film of another part than the one its title numbers and subtitles ([[IdentityMeasures.titlesAnotherPart]]), of another
 *  instalment than the one its title numbers and its original title subtitles ([[IdentityMeasures.numbersAnotherSubtitle]]),
 *  or another year's edition of the series its title dates ([[IdentityMeasures.datesAnotherEdition]]). */
private[identity] object TitleShapeRefusals {
  /** Each refusal, in the order it is asked: the denial it gives and the question of the listing and the film. */
  val refusals: List[(Denial, (MeasuredListing, Film) => Boolean)] = List(
    (Denial("a catalogue namesake of a bare billing under a film festival's banner"), ListingShape.festivalsCatalogueNamesake),
    (Denial("one work of a \"//\" double bill, whose other piece none of its titles holds"), MultiFilmBill.slashesAnotherWork),
    (Denial("a part its title gives a subtitle none of the film's titles holds"), IdentityMeasures.titlesAnotherPart),
    (Denial("an instalment its title numbers, whose subtitle none of the film's titles holds"), IdentityMeasures.numbersAnotherSubtitle),
    (Denial("another year's edition of the series its title dates"), IdentityMeasures.datesAnotherEdition))
}
