package tools

import java.nio.file.{Path, Paths}

/**
 * An operator tool's command line, read strictly against the options it declares: an option it does not declare, a
 * valued option with no value (at the end, or followed by another `--option`), a single option given twice, one
 * positional too many, or a value its reader cannot convert stops the tool with the message and the help text on
 * stderr and exit code 2 — never a silently dropped flag or a shifted pair, the way `args.grouped(2)` read
 * `--apply --report r.md`. `--help` (or `-h`) prints the help text, built from the declarations, and exits 0.
 *
 * {{{
 *   val arguments = CommandLine("IdentityRefit", "re-tunes the identity weights",
 *     CommandLine.Flag("apply", "write the refitted artefact"),
 *     CommandLine.Value("report", "file", "where the markdown report goes")).parseOrExit(args)
 *   arguments.flag("apply"); arguments.path("report")
 * }}}
 *
 * `--name value` and `--name=value` both read; everything after `--` is positional.
 */
final case class CommandLine(tool: String, summary: String, options: Seq[CommandLine.Declared], positionals: Seq[CommandLine.Positional]) {
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

  /** The arguments, or the tool exits: 0 after printing the help, 2 after printing why and the help to stderr — also
   *  when a value read later cannot be converted, or [[Arguments.fail]] is called. */
  def parseOrExit(args: Array[String]): Arguments = {
    def refuse(message: String): Nothing = {
      System.err.println(s"$tool: $message\n\n$help")
      sys.exit(2)
    }
    read(args.toSeq, refuse) match {
      case Right(arguments)     => arguments
      case Left(Help(text))     => print(text); sys.exit(0)
      case Left(Invalid(why))   => refuse(why)
    }
  }

  private def read(args: Seq[String], fail: String => Nothing): Either[Stop, Arguments] = {
    if (args.exists(a => a == "--help" || a == "-h")) return Left(Help(help))
    val byName  = options.map(o => o.name -> o).toMap
    val flags   = Set.newBuilder[String]
    val values  = scala.collection.mutable.LinkedHashMap.empty[String, Vector[String]]
    val free    = Vector.newBuilder[String]
    def take(value: Value, supplied: String): Option[Invalid] =
      if (values.contains(value.name) && !value.repeatable) Some(Invalid(s"--${value.name} given twice"))
      else { values.update(value.name, values.getOrElse(value.name, Vector.empty) :+ supplied); None }
    def loop(rest: List[String]): Option[Invalid] = rest match {
      case Nil                                 => None
      case "--" :: tail                        => free ++= tail; None
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
      case Some(invalid) => Left(invalid)
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
    def required(name: String): String = get(name).getOrElse(fail(s"--$name <${declared(name).meta}> is required"))

    def path(name: String): Option[Path] = get(name).map(Paths.get(_))
    def int(name: String): Option[Int] = converted(name, "a whole number")(_.toIntOption)
    def double(name: String): Option[Double] = converted(name, "a number")(_.toDoubleOption)

    def positional(name: String): Option[String] = line.positionals.indexWhere(_.name == name) match {
      case -1    => throw new IllegalArgumentException(s"${line.tool} declares no positional $name")
      case index => positionals.lift(index)
    }

    private def converted[A](name: String, what: String)(read: String => Option[A]): Option[A] =
      get(name).map(v => read(v).getOrElse(fail(s"--$name wants $what, got $v")))

    private def declared(name: String): Value = line.options.find(_.name == name) match {
      case Some(v: Value) => v
      case _              => throw new IllegalArgumentException(s"${line.tool} declares no valued option --$name")
    }
  }
}
