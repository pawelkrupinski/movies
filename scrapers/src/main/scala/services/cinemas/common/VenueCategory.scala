package services.cinemas.common

import models.CinemaMovie
import services.cinemas.pl.NonMovieEventClassifier

import java.util.Locale

/** The category a mixed venue — a cinema that is also its town's theatre, gig room and lecture hall — files an event
 *  under in its own ticketing: Savoy's `TypeDescription` ("Films", "Live Music", "Theatre On Screen"), a box office's
 *  category tab ("Cinema", "Theatre", "On Stage"). Each venue names its own, so only a name that can mean nothing but a
 *  live performance counts, and never one naming a screen: "Live Event", "Live Arts" and "Live / Event" are the relays
 *  (Keswick Alhambra, Hebden Bridge, Fordingbridge), "Concerts" André Rieu's, "Comedy" a film genre at The Odyssey, "Music
 *  Event" a concert film's ("George Michael: The Faith Tour"), "Musicals" Starlight Express' capture.
 *
 *  Measured 2026-11-13 over every UK Savoy venue's programme (64 venues, 1,930 events): the 120 events these names and
 *  the guards in [[filedAsLiveShow]] take are 120 live shows, among them every stage or tribute take the precision
 *  sample judged wrong (Regal Stowmarket's "Sods Presents: Pinocchio" and Garth Brooks tribute "Blame It All On My
 *  Roots", The Beehive's "HCTC Presents: \"The Little Mermaid\"", The Poly's "Beneath The Tracksuit: A Way With Words"). */
object VenueCategory {

  /** Names a venue files live performances under, whole (case and spacing aside). */
  private val LiveShow: Set[String] = Set("theatre", "theater", "on stage", "live music", "music", "kids shows", "cafe events",
    "interviews and talks", "talk", "talks", "quiz", "bingo", "stand up comedy", "dining and dancing")

  /** A category naming the screen: a listing filed under one as well is screened, whatever else it is filed under. */
  private val Screen = """film|cinema|screen|movie|broadcast|relay|encore""".r

  /** A synopsis telling a performance was captured for the screen ("Filmed live at the specially designed Starlight
   *  Auditorium", Empire Theatre Halstead's "Starlight Express" under "Theatre"). Never a bare "film" or "cinema": a
   *  tribute band's "music for dance, theatre and film", a fashion show "in support of a new cinema projector". */
  private val ScreenedPerformance = """(?i)\bfilmed\b|\brecorded live\b|\blive on screen\b|\bon the big screen\b|\bin cinemas\b|\bbroadcast\b|\bscreening\b""".r

  private def normalised(category: String): String = category.trim.toLowerCase(Locale.ROOT).replaceAll("""[\s-]+""", " ")

  /** Does `category` file live performances? */
  def namesLiveShow(category: String): Boolean = LiveShow(normalised(category))

  /** Does `category` name the screen? */
  def namesScreen(category: String): Boolean = Screen.findFirstIn(normalised(category)).isDefined

  /** Is `listing`, filed by its venue under `categories`, a live show rather than a screening: filed under a live
   *  performance's name and under no screen's, stating none of a film's facts (a year or a director: the venue typed in
   *  a record), naming no broadcast ("NTL: Hay Fever" under "Theatre") and told by its synopsis to be no capture? */
  def filedAsLiveShow(categories: Iterable[String], listing: CinemaMovie): Boolean =
    categories.exists(namesLiveShow) && !categories.exists(namesScreen) &&
      listing.movie.releaseYear.isEmpty && !listing.director.exists(_.trim.nonEmpty) &&
      !NonMovieEventClassifier.isNamedBroadcast(listing.movie.title) &&
      !listing.synopsis.exists(ScreenedPerformance.findFirstIn(_).isDefined)
}
