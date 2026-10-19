package clients.tools

import _root_.tools.CommandLine

/** The command line every live venue measure shares (WriteTicketsolve, WriteSpektrix, WriteSavoy): `--record` to
 *  re-record the captures the client's spec replays, then the venues to run, every one when none is named. A name the
 *  tool does not know stops it — `args.filterNot(_ == "--record")` read a mistyped `--recrod` or venue as a venue no
 *  entry matched, and the run measured nothing and said nothing. */
object VenueMeasure {

  final case class Run(record: Boolean, venues: Set[String]) {
    def wants(name: String): Boolean = venues.isEmpty || venues(name)
  }

  def commandLine(tool: String, summary: String): CommandLine =
    CommandLine(tool, summary, CommandLine.Flag("record", "re-record the captures the client's spec replays"))
      .withPositionals(CommandLine.Positional("venues", "only these (default: every one)", repeatable = true))

  /** The run asked, every venue named one of `known`. */
  def runOf(arguments: CommandLine.Arguments, known: Seq[String]): Run = {
    val named = arguments.positionalsFrom("venues")
    named.filterNot(known.contains).foreach(name => arguments.fail(s"no venue '$name' (one of ${known.distinct.mkString(", ")})"))
    Run(arguments.flag("record"), named.toSet)
  }
}
