package scripts

import tools.CommandLine

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

/**
 * Every `sbt "…/runMain <package>.<Tool> <args>"` (or `java … -cp <classpath> <package>.<Tool> <args>`, up to the
 * line's pipe or redirect, its backslash continuations joined) the repository's scripts, workflows and skills type,
 * with its shell expansions stood in by a placeholder value, so a spec can parse each against the tool's own
 * [[CommandLine]]: a flag a caller passes that the tool no longer declares fails the spec, not the next weekly run.
 */
object ToolCallers {

  final case class Call(file: Path, tool: String, args: Seq[String])

  private val Callers  = Seq("scripts", ".github", ".claude/skills").map(Paths.get(_))
  private val RunMain  = """(?:Test|IntegrationTest|Fixtures)/runMain ([a-z]+\.[A-Za-z]+)([^"'`\n]*)""".r
  private val JavaMain = """(?m)-cp\s+\S+\s+([a-z]+\.[A-Z][A-Za-z]+)([^|;&<>\n]*?)(?=\s+\d*[<>]|\s*[|;&]|\s*$)""".r
  /** `${VAR:+--flag $VAR}`: the flag and a value, as a caller that sets VAR passes them. */
  private val IfSet    = """\$\{\w+:\+([^}]*)\}""".r
  private val Expanded = """\$\{\{[^}]*\}\}|\$\{[^}]*\}|\$\([^)]*\)|\$\w+""".r

  def calls(packageName: String): Seq[Call] = Callers.filter(Files.isDirectory(_)).flatMap { root =>
    val walk = Files.walk(root)
    try walk.iterator.asScala.filter(Files.isRegularFile(_)).toSeq finally walk.close()
  }.sorted.flatMap { file =>
    Try(Files.readString(file, StandardCharsets.UTF_8)).toOption.toSeq.flatMap(callsIn(file, _, packageName))
  }.distinct

  /** The calls of `packageName`'s tools in `text` (read from `file`), a backslash-continued command line read as the
   *  one line the shell reads, so a flag on a continuation line is checked too. */
  def callsIn(file: Path, text: String, packageName: String): Seq[Call] = {
    val joined = text.replace("\\\n", " ")
    (RunMain.findAllMatchIn(joined) ++ JavaMain.findAllMatchIn(joined)).collect {
      case m if m.group(1).startsWith(s"$packageName.") =>
        val stood = Expanded.replaceAllIn(IfSet.replaceAllIn(m.group(2), set => Regex.quoteReplacement(set.group(1))), "x").replace("\"", "")
        Call(file, m.group(1), stood.trim.split("\\s+").toSeq.filter(_.nonEmpty))
    }.toSeq
  }

  /** The tool's declared command line (or commands), if it reads its arguments through one. */
  def commandLineOf(tool: String): Option[CommandLine.Grammar] = for {
    module <- Try(Class.forName(s"$tool$$")).toOption
    method <- Try(module.getMethod("commandLine")).toOption
  } yield method.invoke(module.getField("MODULE$").get(null)).asInstanceOf[CommandLine.Grammar]
}
