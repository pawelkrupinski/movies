package services.movies

/** The running times a screened film can have: a venue's runtime outside them is a slip, not the film's, and reads as
 *  unpublished — at the slot ([[CinemaSlotBuilder]]) and in the identity model alike. */
object FilmRuntime {

  /** The longest a screened film runs: "Sátántangó" is 432 minutes. DE Filmtheater Bleicherode bills "flüstern &
   *  SCHREIEN" at 6000 minutes, which vetoed the 1988 film by the learned `runtime.delta >= 81`. */
  val Max = 600

  /** Is `minutes` a running time a screened film can have ([[Max]])? */
  def plausible(minutes: Int): Boolean = minutes > 0 && minutes <= Max
}
