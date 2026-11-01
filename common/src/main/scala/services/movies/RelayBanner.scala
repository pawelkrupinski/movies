package services.movies

import services.titlerules.{TitleRuleSet, TitleRuleTable}

/** Does a title bill an event-cinema RELAY -- a stage house's or a broadcaster's recording screened in cinemas, under its
 *  banner? What bills one is what the codebase already reads as a relay: an opera or ballet house ([[StageHouse.billed]]:
 *  "Tosca (ROH)", "Bolshoi Ballet: ..."), the title rules whose `effect` is `relay` (title-rules.tsv: "NT Live: ...", "... -
 *  National Theatre Live 2026", "The Met Opera: ...", "RBO: ...", a house's "... 2026/27: ..." season banner, "Exhibition On
 *  Screen: ...", an "... encore"), and a broadcaster's name wherever the box office puts it. Those rows strip or rename the
 *  banner for the LOOKUP; this keeps it as a fact, read off the title alone: the identity model denies a relay's record to a
 *  plain listing whose own text contradicts it (`services.identity.RelayRecords`). */
object RelayBanner {

  /** The `relay` rows of the title-rule table, their placeholders expanded, in every country's set alike. */
  private lazy val rules = TitleRuleSet(TitleRuleTable.rows.filter(_.effect == TitleRuleTable.RelayEffect).map(_.rule)).effectiveRules

  /** A broadcaster's name as words of a title, wherever the box office puts it: the rules read it at the title's edges,
   *  but UK Cineworld bills "Golden Boy (NT Live)", Electric Palace "Screening: NTL - Hay Fever (12A TBC)" and US Flicks
   *  "MetOpera: Carmen (2009)". */
  private val Names: Seq[Seq[String]] = Seq(Seq("nt", "live"), Seq("ntlive"), Seq("ntl"), Seq("national", "theatre", "live"),
    Seq("national", "theatre", "at", "home"), Seq("exhibition", "on", "screen"), Seq("metopera"))

  /** Each title's answer, worked out once: [[billed]] reads the title alone (the relay rows are fixed for the process), and
   *  the identity model asks it of every candidate record of every node it scores ([[services.identity.RelayRecords]]) --
   *  6% of a US order row's CPU, matching the same titles again (JFR, run 41347578716). A title evicted is read again,
   *  alike: the cache changes what an answer costs, never what it is. */
  private val answers = tools.BoundedCache.ofSize(65_536).build[String, java.lang.Boolean]()

  /** Does `title` bill a relay's banner? */
  def billed(title: String): Boolean = answers.get(title, t => java.lang.Boolean.valueOf(read(t))).booleanValue

  private def read(title: String): Boolean = {
    lazy val words = TitleContainment.tokens(title).toSeq
    StageHouse.billed(Seq(title)) || rules.exists(_.compiled.exists(_.findFirstIn(title).isDefined)) || Names.exists(words.containsSlice)
  }

  /** Does one of `titles` (a listing's raw and clean title) bill a relay's banner? */
  def billed(titles: Seq[String]): Boolean = titles.exists(billed)
}
