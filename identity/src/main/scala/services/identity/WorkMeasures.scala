package services.identity

import services.movies.TitleContainment

import IdentityMeasures.*

/** The WORK a title names: its title before a subtitle, and the works two titles share or bill.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object WorkMeasures {
  /** The WORK a title's subtitle hangs off: what stands before its first dash ("Cirque du Soleil:
   *  Kurios" of "Cirque du Soleil: Kurios - Gabinet osobliwości"), when a subtitle follows it. */
  private val SubtitleDash = """\s+[-–—]\s+""".r
  private def workOf(t: String): Option[String] =
    SubtitleDash.findFirstMatchIn(t).map(m => t.substring(0, m.start).trim).filter(w => TitleContainment.tokens(w).sizeIs >= 2)
  /** Is the listing's whole title the film's WORK — its title before a subtitle, after a dash, a comma
   *  or a colon ("Leonas" of "Leonas, el instinto más salvaje", "BTS WORLD TOUR 'ARIRANG' IN BUENOS
   *  AIRES" of "…: Live Viewing")? The words of the work, or None. */
  def titleIsWorkOf(l: Listing, f: Film): Option[Int] = workNamedBy(l.ownTitles, f)
  /** Is the original title the venue publishes the film's WORK, as [[titleIsWorkOf]] — DE "Ein Hund namens Quill",
   *  published as "Quill", of "Quill - Ein Freund für´s Leben"? */
  def originalTitleIsWorkOf(l: Listing, f: Film): Option[Int] = workNamedBy(l.originalTitle.toSeq, f)
  /** The words of the film's work (its title or original title before a subtitle) when one of `titles` is it. */
  private def workNamedBy(titles: Seq[String], f: Film): Option[Int] = {
    val own = titles.map(key).filter(_.nonEmpty).toSet
    f.ownTitles.flatMap(leadingWork).find(w => own(key(w))).map(w => TitleContainment.tokens(w).size)
  }

  /** Does the listing's title name the film by the WORKS of two of its titles, one piece each — its two pieces a dash
   *  parts, each what one title of the film stands before its subtitle ([[workOf]])? DE Kamino's "Ecrire la vie - Writing
   *  Life" bills the French "Écrire la vie - Annie Ernaux racontée par des lycéennes et des lycéens" by its own work and
   *  its English title's ("Writing Life – Annie Ernaux Through the Eyes of High School Students"): two languages' names
   *  of one film, each a whole piece, as a [[containment]] `segment` names it — not a `fragment` of either title, which
   *  left the film denied and the Danish "Giv os en chance", TMDB's English "Writing life", taken by one piece. */
  def billsWorksOfTwoTitles(l: Listing, f: Film): Boolean = {
    val works = f.titles.map(title => workOf(title).map(key).getOrElse(""))
    l.ownTitles.exists { own =>
      SubtitleDash.split(own).toSeq.map(key) match {
        case Seq(first, second) if first.nonEmpty && second.nonEmpty && first != second =>
          val (a, b) = (works.indexOf(first), works.indexOf(second))
          a >= 0 && b >= 0 && a != b
        case _ => false
      }
    }
  }

  /** Do the listing and the film bill the same WORK, each under its own subtitle — the title before a
   *  first comma, colon or dash ("BTS World Tour 'ARIRANG' In Buenos Aires: Live" and "…: Live
   *  Viewing")? The words of the work, or None. */
  def billsWorkOf(l: Listing, f: Film): Option[Int] = workNamedBy(l.ownTitles.flatMap(leadingWork), f)

  private val WorkSubtitle = """\s*[,:]\s+|\s+[-–—]\s+""".r
  private def leadingWork(t: String): Option[String] = WorkSubtitle.findFirstMatchIn(t).map(m => t.substring(0, m.start).trim)

  /** Do the listing and the film bill the same work under different subtitles — a venue's translated
   *  subtitle ("Gabinet osobliwości" for "Cabinet des curiosités")? */
  def sharesWork(l: Listing, f: Film): Boolean = {
    val works = f.ownTitles.flatMap(workOf).map(key).toSet
    l.ownTitles.flatMap(workOf).map(key).exists(works)
  }
}
