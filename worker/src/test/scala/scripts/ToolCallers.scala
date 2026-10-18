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
 * with its shell expansions stood in — by the file's own value for a variable it assigns exactly once to a literal
 * (`args="--measure --out dir"`), by a placeholder value otherwise — so a spec can parse each against the tool's own
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
  private val Assignment = """(?<![\w$-])(\w+)=("[^"]*"|'[^']*'|[^\s;&|)]*)""".r

  /** The variables `text` assigns exactly once, to a value with no expansion of its own: the one value a caller's
   *  `$name` can hold. A variable assigned twice (`scope=""`, then `scope=" --countries $cc"`) is left to the placeholder. */
  private def literals(text: String): Map[String, String] =
    Assignment.findAllMatchIn(text).toSeq.groupBy(_.group(1)).collect {
      case (name, Seq(only)) if !only.group(2).exists(c => c == '$' || c == '`') =>
        name -> only.group(2).stripPrefix("\"").stripSuffix("\"").stripPrefix("'").stripSuffix("'")
    }

  /** A script's self-test (`<tool>-test.sh`, `<tool>_test.py`): what it holds are the command lines it EXPECTS its
   *  tool to log through a fake, not calls of its own. */
  private def isSelfTest(file: Path): Boolean = {
    val name = file.getFileName.toString
    name.endsWith("-test.sh") || name.endsWith("_test.py")
  }

  def calls(packageName: String): Seq[Call] = Callers.filter(Files.isDirectory(_)).flatMap { root =>
    val walk = Files.walk(root)
    try walk.iterator.asScala.filter(file => Files.isRegularFile(file) && !isSelfTest(file)).toSeq finally walk.close()
  }.sorted.flatMap { file =>
    Try(Files.readString(file, StandardCharsets.UTF_8)).toOption.toSeq.flatMap(callsIn(file, _, packageName))
  }.distinct

  /** The calls of `packageName`'s tools in `text` (read from `file`), a backslash-continued command line read as the
   *  one line the shell reads, so a flag on a continuation line is checked too. */
  def callsIn(file: Path, text: String, packageName: String): Seq[Call] = {
    val joined = text.replace("\\\n", " ")
    val known  = literals(joined)
    val Named  = """\$\{(\w+)\}|\$(\w+)""".r
    (RunMain.findAllMatchIn(joined) ++ JavaMain.findAllMatchIn(joined)).collect {
      case m if m.group(1).startsWith(s"$packageName.") =>
        val ifSet = IfSet.replaceAllIn(m.group(2), set => Regex.quoteReplacement(set.group(1)))
        val named = Named.replaceAllIn(ifSet, v => Regex.quoteReplacement(known.getOrElse(Option(v.group(1)).getOrElse(v.group(2)), v.matched)))
        val stood = Expanded.replaceAllIn(named, "x").replace("\"", "")
        Call(file, m.group(1), stood.trim.split("\\s+").toSeq.filter(_.nonEmpty))
    }.toSeq
  }

  /** The tool's declared command line (or commands), if it reads its arguments through one. */
  def commandLineOf(tool: String): Option[CommandLine.Grammar] = for {
    module <- Try(Class.forName(s"$tool$$")).toOption
    method <- Try(module.getMethod("commandLine")).toOption
  } yield method.invoke(module.getField("MODULE$").get(null)).asInstanceOf[CommandLine.Grammar]
}
