package services.cinemas.uk

import models.Cinema

/** The source a UK venue's programme is scraped from, with the venue's handle in it — a row's
 *  `primary` in `data/uk/roster.json`, generated into [[UkSourcesData]]'s table. `UkScraperCatalog`
 *  builds each venue's scraper from it. */
sealed trait UkFeed {
  /** A chain's own site (Cineworld / Vue / Showcase / Everyman / Odeon: a [[UkFeed.Chain]]), rather than an
   *  independent's box office or flicks.co.uk. */
  def chain: Boolean = false
}

object UkFeed {
  /** A chain's own site, by the chain's handle for the venue. */
  sealed trait Chain extends UkFeed { final override def chain = true }
  /** An independent's own box office: a ticketing system's install, or a client built for the one venue. */
  sealed trait BoxOffice extends UkFeed

  /** flicks.co.uk's page for the venue. */
  final case class Flicks(slug: String) extends UkFeed
  final case class Cineworld(slug: String) extends Chain
  final case class Vue(id: String) extends Chain
  final case class Showcase(id: String) extends Chain
  final case class Everyman(id: String) extends Chain
  final case class Odeon(id: String) extends Chain
  /** A Savoy Systems box office on the venue's own site. */
  final case class Savoy(dllBase: String) extends BoxOffice
  /** A booking-only Savoy host: the venue's site is elsewhere, Savoy only sells its tickets. */
  final case class SavoyBoxOffice(dllBase: String) extends BoxOffice
  /** A Spektrix client; `building` names the venue's, for a client that sells several. */
  final case class Spektrix(client: String, building: Option[String]) extends BoxOffice
  final case class Ticketsolve(client: String) extends BoxOffice
  /** Derby QUAD's Spektrix with its own detail pages ([[QuadDerbyClient]], built for that one venue). */
  case object QuadDerby extends BoxOffice
  case object BfiSouthbank extends BoxOffice
  case object NovaCinema extends BoxOffice
  /** The Old Court, Windsor: on no Flicks page, scraped off its own site. */
  case object TheOldCourt extends BoxOffice

  /** The feed a generated row names: its kind, as roster.json spells it (`data/uk/scripts/uk_roster.py`'s
   *  `PRIMARY_ARGS`, plus `flicks` for a venue scraped off Flicks), and its arguments. An unknown kind, or
   *  arguments the kind does not take, fails loudly. */
  def parse(kind: String, args: Seq[String]): UkFeed = (args match {
    case Seq()                                    => NoArgument.get(kind)
    case Seq(argument)                            => OneArgument.get(kind).map(_(argument))
    case Seq(client, building) if kind == "spektrix" => Some(Spektrix(client, Some(building)))
    case _                                        => None
  }).getOrElse(throw new IllegalArgumentException(s"no UK feed $kind(${args.mkString(", ")})"))

  /** [[parse]]'s kinds that take one argument: the venue's handle on that source. */
  private val OneArgument: Map[String, String => UkFeed] = Map(
    "flicks"         -> (Flicks(_)),
    "cineworld"      -> (Cineworld(_)),
    "vue"            -> (Vue(_)),
    "showcase"       -> (Showcase(_)),
    "everyman"       -> (Everyman(_)),
    "odeon"          -> (Odeon(_)),
    "savoy"          -> (Savoy(_)),
    "savoyBoxOffice" -> (SavoyBoxOffice(_)),
    "spektrix"       -> (Spektrix(_, None)),
    "ticketsolve"    -> (Ticketsolve(_))
  )

  /** [[parse]]'s kinds that take none: a client built for one venue. */
  private val NoArgument: Map[String, UkFeed] = Map(
    "quadDerby" -> QuadDerby, "bfiSouthbank" -> BfiSouthbank, "novaCinema" -> NovaCinema, "theOldCourt" -> TheOldCourt)
}

/** One UK venue's sources: its primary, and — for an own-site primary Flicks also lists — the
 *  flicks.co.uk slug it falls back to (`ChainFlicksFallback`). */
final case class UkVenueSources(cinema: Cinema, primary: UkFeed, flicksFallback: Option[String])
