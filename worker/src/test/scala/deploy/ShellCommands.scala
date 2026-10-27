package deploy

import scala.collection.mutable

/**
 * THE REPO FILES A SHELL SCRIPT RUNS, read off its text: what a workflow's `run:`, a composite action's `run:` and a git
 * hook EXECUTE, for WorkflowScriptsRunnableSpec. Not a shell: a lexer good enough to find where commands start, and
 * which words name files in this repository.
 *
 *  - A command word with a `/` in it (`scripts/ci/x.sh`, `./gradlew`) is RUN: the file needs its executable bit and a
 *    shebang.
 *  - The script an interpreter is handed (`bash x.sh`, `python3 x.py`, `source x.sh`, `. x.sh`, `node x.mjs`) is READ:
 *    it only has to exist.
 *
 * A word built at run time (any `$`, `${{ }}` or glob) names nothing checkable and is skipped, as is an absolute path
 * (the runner's machine, not this repository). `cd <literal>` moves the directory paths resolve against, a subshell
 * `( )` / `$( )` restores it, and a `cd` to a computed directory makes the rest unknown (skipped) -- unless the caller
 * says such a `cd` always lands on a checkout's root, as a git hook's do. Heredoc bodies, comments, `case` patterns,
 * `for` lists and redirection targets are not commands.
 */
object ShellCommands {

  /** How a script uses a repo file: RUN it as a command, or hand it to an interpreter to READ. */
  enum Use { case Run, Read }

  /** One repo-relative path the script uses, and the 1-based line of the script it is on. */
  final case class Reference(path: String, use: Use, line: Int)

  /** The words that start a command without being it. */
  private val Keywords = Set("if", "then", "else", "elif", "do", "while", "until", "!", "{", "}", "time", "fi", "done")

  /** Prefixes that run the word after them (their own flags and, for `timeout`, its duration skipped). */
  private val Wrappers = Set("exec", "command", "nohup", "sudo", "env", "timeout", "nice", "stdbuf", "xvfb-run")

  /** Interpreters whose first non-flag argument is a script they read; `-c` / `-m` / `-e` mean there is none. */
  private val Interpreters = Set("bash", "sh", "zsh", "python", "python3", "source", ".", "node", "ruby", "perl")
  private val InlineFlags  = Set("-c", "-m", "-e", "--eval", "-")

  private sealed trait Token { def line: Int }
  private final case class Word(text: String, dynamic: Boolean, line: Int) extends Token
  private final case class Op(text: String, line: Int)                     extends Token

  /** Every repo path the script RUNS or READS, resolved against `cwd` (repo-relative, "" for the root). */
  def references(
      script: String,
      cwd: String = "",
      computedCdIsRoot: Boolean = false,
      ownDirectory: Option[String] = None,
  ): Seq[Reference] = {
    val tokens = lex(withoutHeredocs(anchored(script, ownDirectory)))
    val out    = mutable.ArrayBuffer.empty[Reference]
    // The directory stack: a subshell pushes, its close pops. None: unknown, so nothing resolves.
    var dirs: List[Option[String]] = List(Some(cwd))
    // What each open paren closes: a subshell (pops a dir) or a case pattern (does not).
    var closers: List[String] = Nil
    var atStart               = true
    var expectPattern         = false
    var skipToSeparator       = false
    var caseDepth             = 0
    var backtick              = false
    var interpreterOf: Option[(String, Int)] = None // waiting for an interpreter's script word
    var cdPending                            = false
    var redirectNext                         = false
    var wrapperSkip                          = 0 // words still to skip after a wrapper (timeout's duration)

    def resolve(word: String): Option[String] =
      if (word == Root.dropRight(1) || word.startsWith(Root)) normalise(word.drop(Root.length - 1))
      else dirs.head.flatMap(base => normalise(if (base.isEmpty) word else s"$base/$word"))

    def separator(): Unit = {
      atStart = true; skipToSeparator = false; interpreterOf = None; cdPending = false; wrapperSkip = 0
    }

    tokens.foreach {
      case Op(op, _) if expectPattern =>
        if (op == ")") { expectPattern = false; separator() }
      case Word(w, _, _) if expectPattern =>
        if (w == "esac") { expectPattern = false; caseDepth -= 1; separator() }
      case Op(">" | "<", _) => redirectNext = true
      case Op("\n" | ";" | "&&" | "||" | "|" | "&" | "|&", _) => separator()
      case Op(";;" | ";&" | ";;&", _) => separator(); if (caseDepth > 0) expectPattern = true
      case Op("(" | "$(", _) =>
        dirs = dirs.head :: dirs; closers = "subshell" :: closers; separator()
      case Op(")", _) =>
        if (closers.nonEmpty) { closers = closers.tail; if (dirs.tail.nonEmpty) dirs = dirs.tail }
        separator(); atStart = false
      case Op("`", _) =>
        if (!backtick) { dirs = dirs.head :: dirs; separator() }
        else { if (dirs.tail.nonEmpty) dirs = dirs.tail; separator(); atStart = false }
        backtick = !backtick
      case Op(_, _) => ()
      case Word(_, _, _) if redirectNext => redirectNext = false
      case Word(_, _, _) if skipToSeparator => ()
      case Word(w, false, _) if cdPending && w.startsWith("-") && w != "-" => () // cd -P
      case Word(w, dynamic, _) if cdPending =>
        cdPending = false; skipToSeparator = true
        val target = if (dynamic || w == "-" || w.startsWith("/") || w.startsWith("~")) None else resolve(w)
        dirs = (if (dynamic && computedCdIsRoot) Some("") else target) :: dirs.tail
      case Word(w, dynamic, line) if interpreterOf.nonEmpty =>
        if (InlineFlags(w)) { interpreterOf = None; skipToSeparator = true }
        else if (w.startsWith("-")) ()
        else {
          interpreterOf = None; skipToSeparator = true
          if (!dynamic && checkable(w)) resolve(w).foreach(p => out += Reference(p, Use.Read, line))
        }
      case Word(w, dynamic, line) if atStart =>
        if (wrapperSkip > 0) wrapperSkip -= 1
        else if (w.startsWith("-") && !dynamic) ()
        else if (Keywords(w) || (!dynamic && w.matches("[A-Za-z_][A-Za-z0-9_]*(\\[[^]]*])?\\+?=.*"))) ()
        else if (dynamic && w.matches("[A-Za-z_][A-Za-z0-9_]*\\+?=.*")) ()
        else if (w == "esac") caseDepth = (caseDepth - 1).max(0)
        else if (w == "case") { caseDepth += 1; skipToSeparator = true; expectPattern = true; atStart = false }
        else if (w == "for" || w == "select") skipToSeparator = true
        else if (w == "timeout") wrapperSkip = 1
        else if (Wrappers(w)) ()
        else if (w == "cd" || w == "pushd") { atStart = false; cdPending = true }
        else if (Interpreters(w)) { atStart = false; interpreterOf = Some(w -> line) }
        else {
          atStart = false; skipToSeparator = true
          if (!dynamic && w.contains('/') && checkable(w)) resolve(w).foreach(p => out += Reference(p, Use.Run, line))
        }
      case Word(_, _, _) => ()
    }
    out.toSeq
  }

  /** Where a word names the checkout's root whatever the directory: the runner's workspace, written out. */
  private val Root = "\u0000root\u0000/"

  /** The script with the runner's workspace (`${{ github.workspace }}`, `$GITHUB_WORKSPACE`) and the script's own
   *  directory -- a composite action's (`${{ github.action_path }}`, `$GITHUB_ACTION_PATH`), a script file's
   *  (`$(dirname "${BASH_SOURCE[0]}")`, `$(dirname "$0")`) -- written as [[Root]]-anchored paths: they are fixed, so a
   *  path built on them names a repo file as surely as a relative one. */
  private def anchored(script: String, ownDirectory: Option[String]): String = {
    val workspace = """\$\{\{\s*github\.workspace\s*}}|"\$\{?GITHUB_WORKSPACE}?"|\$\{?GITHUB_WORKSPACE}?(?![A-Za-z0-9_])"""
    val action    = """\$\{\{\s*github\.action_path\s*}}|"\$\{?GITHUB_ACTION_PATH}?"|\$\{?GITHUB_ACTION_PATH}?(?![A-Za-z0-9_])"""
    val own       = """\$\(dirname "(?:\$\{BASH_SOURCE\[0]}|\$0)"\)"""
    val rooted    = script.replaceAll(workspace, java.util.regex.Matcher.quoteReplacement(Root.dropRight(1)))
    ownDirectory.fold(rooted) { dir =>
      val here = java.util.regex.Matcher.quoteReplacement(Root + dir)
      rooted.replaceAll(action, here).replaceAll(own, here)
    }
  }

  /** A literal relative path: not absolute, not home-relative, no glob, no expression. */
  private def checkable(word: String): Boolean =
    word.nonEmpty && !word.startsWith("/") && !word.startsWith("~") && !word.exists("*?[]{}".contains(_)) &&
      (word.contains('/') || word.matches(".*\\.(sh|py|mjs|js|rb|pl)"))

  /** `a/./b/../c` as `a/c`; None for a path that climbs out of the repository. */
  private[deploy] def normalise(path: String): Option[String] = {
    val parts = path.split('/').filter(p => p.nonEmpty && p != ".").foldLeft(Option(List.empty[String])) {
      case (Some(acc), "..") => acc match { case _ :: rest => Some(rest); case Nil => None }
      case (Some(acc), p)    => Some(p :: acc)
      case (None, _)         => None
    }
    parts.map(_.reverse.mkString("/"))
  }

  /** The script with every heredoc's body blanked (line numbers kept): it is data, not commands. */
  private def withoutHeredocs(script: String): String = {
    val Heredoc = """<<-?\s*(['"]?)([A-Za-z_][A-Za-z0-9_]*)\1""".r
    val lines   = script.split("\n", -1).toVector
    val out     = mutable.ArrayBuffer.empty[String]
    var ending: List[String] = Nil
    lines.foreach { line =>
      if (ending.nonEmpty) {
        out += ""
        if (line.trim == ending.head) ending = ending.tail
      } else {
        out += line
        ending = Heredoc.findAllMatchIn(line.replace("<<<", "   ")).map(_.group(2)).toList
      }
    }
    out.mkString("\n")
  }

  private def lex(text: String): Vector[Token] = {
    val out     = Vector.newBuilder[Token]
    val word    = new StringBuilder
    var dynamic = false
    var started = false
    var line    = 1
    var i       = 0
    def end(): Unit = {
      if (started) out += Word(word.toString, dynamic, line)
      word.clear(); dynamic = false; started = false
    }
    def op(s: String): Unit = { end(); out += Op(s, line); i += s.length }
    def closing(from: Int, open: Char, close: Char): Int = { // index just past the matching close
      var depth = 0; var j = from
      while (j < text.length && !(text(j) == close && depth == 1)) {
        if (text(j) == open) depth += 1 else if (text(j) == close) depth -= 1
        if (text(j) == '\n') line += 1
        j += 1
      }
      (j + 1).min(text.length)
    }
    while (i < text.length) {
      val c = text(i)
      if (c == '\\' && i + 1 < text.length && text(i + 1) == '\n') { i += 2; line += 1 }
      else if (c == '\\' && i + 1 < text.length) { word += text(i + 1); started = true; i += 2 }
      else if (c == '\n') { op("\n"); line += 1 }
      else if (c == ' ' || c == '\t' || c == '\r') { end(); i += 1 }
      else if (c == '#' && !started) { while (i < text.length && text(i) != '\n') i += 1 }
      else if (c == '\'') {
        val close = text.indexOf('\'', i + 1)
        val until = if (close < 0) text.length else close
        val body  = text.substring(i + 1, until)
        line += body.count(_ == '\n'); word ++= body; started = true; i = until + 1
      } else if (c == '"') {
        var j = i + 1
        while (j < text.length && text(j) != '"') { if (text(j) == '\\') j += 1; j += 1 }
        val body = text.substring(i + 1, j.min(text.length))
        if (body.contains('$') || body.contains('`')) dynamic = true
        line += body.count(_ == '\n'); word ++= body; started = true; i = j + 1
      } else if (text.startsWith("$((", i)) { dynamic = true; started = true; i = closing(i + 1, '(', ')') }
      else if (text.startsWith("$(", i)) { if (started) dynamic = true; op("$(") }
      else if (text.startsWith("<(", i) || text.startsWith(">(", i)) op("$(")
      else if (text.startsWith("${{", i)) {
        val close = text.indexOf("}}", i); dynamic = true; started = true; i = if (close < 0) text.length else close + 2
      } else if (text.startsWith("${", i)) { dynamic = true; started = true; i = closing(i + 1, '{', '}') }
      else if (c == '$') { dynamic = true; started = true; word += c; i += 1 }
      else if (c == '`') op("`")
      else if (Seq(";;&", ";;", ";&", "&&", "||", "|&", "&>>", "&>", ">>", ">&", "<&", "<<<").exists(text.startsWith(_, i))) {
        val o = Seq(";;&", ";;", ";&", "&&", "||", "|&", "&>>", "&>", ">>", ">&", "<&", "<<<").find(text.startsWith(_, i)).get
        if (started && word.forall(_.isDigit) && (o.startsWith(">") || o.startsWith("<"))) { word.clear(); started = false }
        op(if (o.exists("<>".contains(_))) ">" else o)
        if (o == ">&" || o == "<&") () // the fd/target word follows and is skipped as the redirect target
      } else if (c == '>' || c == '<') {
        if (started && word.forall(_.isDigit)) { word.clear(); started = false }
        if (text.startsWith("<<", i)) { end(); out += Op(">", line); i += 2; while (i < text.length && text(i) == '-') i += 1 }
        else op(">")
      } else if (c == ';' || c == '|' || c == '&' || c == '(' || c == ')') op(c.toString)
      else {
        if ("*?[".contains(c)) dynamic = true
        word += c; started = true; i += 1
      }
    }
    end()
    out.result()
  }
}
