package tools

import models.Country

import java.nio.file.{Path, Paths}

/**
 * An operator tool's command line, read strictly against the options it declares: an option it does not declare, a
 * valued option with no value (at the end, or followed by another `--option`), a single option given twice, one
 * positional too many, or a value its reader cannot convert stops the tool with the message and the help text on
 * stderr and exit code 2 — never a silently dropped flag or a shifted pair, the way `args.grouped(2)` read
 * `--apply --report r.md`. `--help` (or `-h`) in option position — not an option's value, nor after `--` — prints
 * the help text, built from the declarations, and exits 0.
 *
 * {{{
 *   val arguments = CommandLine("IdentityRefit", "re-tunes the identity weights",
 *     CommandLine.Flag("apply", "write the refitted artefact"),
 *     CommandLine.Value("report", "file", "where the markdown report goes")).parseOrExit(args)
 *   arguments.flag("apply"); arguments.path("report")
 * }}}
 *
 * `--name value` and `--name=value` both read; everything after `--` is positional. A tool whose first argument names
 * what it does (`BuildHardClusters select …` / `extend …`) declares one CommandLine per command in [[CommandLine.Commands]].
 */
final case class CommandLine(tool: String, summary: String, options: Seq[CommandLine.Declared], positionals: Seq[CommandLine.Positional])
    extends CommandLine.Grammar {
  import CommandLine.*

  require(options.map(_.name).distinct.size == options.size, s"$tool declares an option twice")
  require(positionals.dropRight(1).forall(!_.repeatable), s"$tool: only the last positional may repeat")

  def withPositionals(declared: Positional*): CommandLine = copy(positionals = declared)

  def help: String = {
    val usage   = (s"usage: $tool" +: Option.when(options.nonEmpty)("[options]").toSeq) ++
      positionals.map(p => if (p.repeatable) s"[${p.name}...]" else s"[${p.name}]")
    val rows    = positionals.map(p => p.name -> p.help) ++ options.map {
      case f: Flag  => s"--${f.name}" -> f.help
      case v: Value => s"--${v.name} <${v.meta}>" ->
        (v.help + v.default.fold("")(d => s" (default: $d)") + (if (v.repeatable) " (repeatable)" else ""))
    } :+ ("--help" -> "this text")
    val width   = rows.map(_._1.length).max
    (Seq(usage.mkString(" "), "", summary, "") ++ rows.map { case (left, right) => s"  ${left.padTo(width, ' ')}  $right" })
      .mkString("", "\n", "\n")
  }

  /** The arguments read against the declarations, or why the tool stops. Pure: a value read later that cannot be
   *  converted throws [[InvalidArguments]]. */
  def parse(args: Seq[String]): Either[Stop, Arguments] = read(args, message => throw InvalidArguments(message))

  def accepts(args: Seq[String]): Either[Stop, Unit] = parse(args).map(_ => ())

  /** The arguments, or the tool exits: 0 after printing the help, 2 after printing why and the help to stderr — also
   *  when a value read later cannot be converted, or [[Arguments.fail]] is called. */
  def parseOrExit(args: Array[String]): Arguments = exiting(tool, help)(refuse => read(args.toSeq, refuse))

  private def read(args: Seq[String], fail: String => Nothing): Either[Stop, Arguments] = {
    val byName  = options.map(o => o.name -> o).toMap
    val flags   = Set.newBuilder[String]
    val values  = scala.collection.mutable.LinkedHashMap.empty[String, Vector[String]]
    val free    = Vector.newBuilder[String]
    def take(value: Value, supplied: String): Option[Stop] =
      if (values.contains(value.name) && !value.repeatable) Some(Invalid(s"--${value.name} given twice"))
      else { values.update(value.name, values.getOrElse(value.name, Vector.empty) :+ supplied); None }
    def loop(rest: List[String]): Option[Stop] = rest match {
      case Nil                                 => None
      case "--" :: tail                        => free ++= tail; None
      case ("--help" | "-h") :: _              => Some(Help(help))
      case token :: tail if token.startsWith("--") =>
        val (name, inline) = token.drop(2).split("=", 2) match {
          case Array(n, v) => (n, Some(v))
          case Array(n)    => (n, None)
        }
        byName.get(name) match {
          case None                         => Some(Invalid(s"unknown option --$name"))
          case Some(_: Flag) if inline.nonEmpty => Some(Invalid(s"--$name takes no value"))
          case Some(_: Flag)                => flags += name; loop(tail)
          case Some(v: Value)               => inline match {
            case Some(supplied) => take(v, supplied).orElse(loop(tail))
            case None => tail match {
              case supplied :: after if !supplied.startsWith("--") => take(v, supplied).orElse(loop(after))
              case _                                         => Some(Invalid(s"--$name needs a value <${v.meta}>"))
            }
          }
        }
      case token :: tail                       => free += token; loop(tail)
    }
    loop(args.toList) match {
      case Some(stop) => Left(stop)
      case None =>
        val positional = free.result()
        val room       = if (positionals.lastOption.exists(_.repeatable)) Int.MaxValue else positionals.size
        if (positional.size > room) Left(Invalid(s"unexpected argument ${positional(room)}"))
        else Right(new Arguments(this, flags.result(), values.toMap, positional, fail))
    }
  }
}

object CommandLine {

  def apply(tool: String, summary: String, options: Declared*): CommandLine = CommandLine(tool, summary, options, Nil)

  /** What a tool's arguments are read against — one [[CommandLine]], or [[Commands]] — as a spec checks a caller's. */
  sealed trait Grammar {
    def tool: String
    def help: String
    /** Whether `args` read: why not, or the help asked for. */
    def accepts(args: Seq[String]): Either[Stop, Unit]
  }

  /**
   * A tool whose first argument names its command, and each command's arguments read strictly by that command's own
   * [[CommandLine]] (declared under the command's name: `CommandLine("extend", …)`): no command, or one it does not
   * declare, stops the tool as an unknown option does; `--help` first prints every command, after a command that
   * command's help.
   */
  final case class Commands(tool: String, summary: String, commands: CommandLine*) extends Grammar {
    require(commands.map(_.tool).distinct.size == commands.size, s"$tool declares a command twice")

    private val byName = commands.map(c => c.tool -> c.copy(tool = s"$tool ${c.tool}")).toMap

    def help: String = {
      val width = commands.map(_.tool.length).max
      (Seq(s"usage: $tool <command> [options]", "", summary, "") ++
        commands.map(c => s"  ${c.tool.padTo(width, ' ')}  ${c.summary}") ++ Seq("", s"$tool <command> --help: that command's options"))
        .mkString("", "\n", "\n")
    }

    /** The command named and its arguments, or why the tool stops. */
    def parse(args: Seq[String]): Either[Stop, (String, Arguments)] = args match {
      case Seq()                                        => Left(Invalid(s"no command (one of ${commands.map(_.tool).mkString(", ")})"))
      case first +: _ if first == "--help" || first == "-h" => Left(Help(help))
      case name +: rest => byName.get(name).toRight(Invalid(s"unknown command $name (one of ${commands.map(_.tool).mkString(", ")})"))
        .flatMap(_.parse(rest).map(name -> _))
    }

    def accepts(args: Seq[String]): Either[Stop, Unit] = parse(args).map(_ => ())

    /** The command named and its arguments, or the tool exits as [[CommandLine.parseOrExit]] does. */
    def parseOrExit(args: Array[String]): (String, Arguments) = args.headOption.flatMap(byName.get) match {
      case Some(line) => args.head -> line.parseOrExit(args.drop(1))
      case None       => exiting(tool, help)(_ => parse(args.toSeq))
    }
  }

  /** `read`'s arguments, or the tool exits: 0 after printing the help, 2 after printing why and `help` to stderr. */
  private def exiting[A](tool: String, help: String)(read: (String => Nothing) => Either[Stop, A]): A = {
    def refuse(message: String): Nothing = {
      System.err.println(s"$tool: $message\n\n$help")
      sys.exit(2)
    }
    read(refuse) match {
      case Right(arguments)   => arguments
      case Left(Help(text))   => print(text); sys.exit(0)
      case Left(Invalid(why)) => refuse(why)
    }
  }

  sealed trait Declared { def name: String; def help: String }
  /** A boolean `--name`: present or not. */
  final case class Flag(name: String, help: String) extends Declared
  /** `--name <meta>`: one value (or, repeatable, any number in order), and its value when absent. */
  final case class Value(name: String, meta: String, help: String, default: Option[String] = None, repeatable: Boolean = false)
      extends Declared
  /** A bare argument, in declaration order; the last may take every remaining one. */
  final case class Positional(name: String, help: String, repeatable: Boolean = false)

  sealed trait Stop
  final case class Help(text: String)       extends Stop
  final case class Invalid(message: String) extends Stop

  final case class InvalidArguments(message: String) extends RuntimeException(message)

  /** What a command line gave. Reading a name the tool never declared (or a flag as a value) is the tool's own bug and
   *  throws `IllegalArgumentException`; a value that cannot be converted, or a required one left out, is the caller's
   *  and stops the tool as [[CommandLine.parseOrExit]] does. */
  final class Arguments(val line: CommandLine, val flags: Set[String], val values: Map[String, Vector[String]], val positionals: Seq[String],
                        stop: String => Nothing) {

    def fail(message: String): Nothing = stop(message)

    def flag(name: String): Boolean = line.options.find(_.name == name) match {
      case Some(_: Flag) => flags(name)
      case _             => throw new IllegalArgumentException(s"${line.tool} declares no flag --$name")
    }

    def all(name: String): Seq[String] = { val v = declared(name); values.getOrElse(name, v.default.toVector) }
    def get(name: String): Option[String] = all(name).lastOption
    /** The value given, or else declared as the default — or the tool stops: a required option left out. */
    def required(name: String): String = get(name).getOrElse(fail(s"--$name <${declared(name).meta}> is required"))
    def requiredPath(name: String): Path = Paths.get(required(name))
    def requiredInt(name: String): Int = { required(name); int(name).get }

    def path(name: String): Option[Path] = get(name).map(Paths.get(_))
    def int(name: String): Option[Int] = converted(name, "a whole number")(_.toIntOption)
    def double(name: String): Option[Double] = converted(name, "a number")(_.toDoubleOption)

    def positional(name: String): Option[String] = positionalsFrom(name).headOption
    /** The positional given — or the tool stops: a required one left out. */
    def requiredPositional(name: String): String = positional(name).getOrElse(fail(s"<$name> is required"))
    /** The positional `name` and every one after it: a repeatable last positional's every value, in order. */
    def positionalsFrom(name: String): Seq[String] = line.positionals.indexWhere(_.name == name) match {
      case -1    => throw new IllegalArgumentException(s"${line.tool} declares no positional $name")
      case index => positionals.drop(index)
    }
    def positionalInt(name: String): Option[Int] =
      positional(name).map(v => v.toIntOption.getOrElse(fail(s"<$name> wants a whole number, got $v")))

    /** The country the positional names — a code no country has stops the tool, never reads as no country (and so
     *  the tool's default one), the way `args.headOption.flatMap(Country.byCode)` read a mistyped `gb`. */
    def country(name: String): Option[Country] = positional(name).map(countryOf)
    /** Every country the positional `name` and those after it name, each refused as [[country]] refuses one. */
    def countries(name: String): Seq[Country] = positionalsFrom(name).map(countryOf)

    private def countryOf(code: String): Country =
      Country.byCode(code).getOrElse(fail(s"no country '$code' (one of ${Country.all.map(_.code).mkString(", ")})"))

    private def converted[A](name: String, what: String)(read: String => Option[A]): Option[A] =
      get(name).map(v => read(v).getOrElse(fail(s"--$name wants $what, got $v")))

    private def declared(name: String): Value = line.options.find(_.name == name) match {
      case Some(v: Value) => v
      case _              => throw new IllegalArgumentException(s"${line.tool} declares no valued option --$name")
    }
  }
}
