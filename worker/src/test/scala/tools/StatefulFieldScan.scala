package tools

import java.nio.file.Path

/**
 * The state a class KEEPS: every mutable map, set or [[services.identity.Memo]] declared in a class, trait or object
 * body — what outlives one call — with the type it is keyed by. A local in a method body is one call's scratch and is
 * not listed.
 *
 * Scala is not parsed: braces are followed, and a `{` opens a template body when the text before it, back to the last
 * brace, holds a `class` / `trait` / `object` header or a `new X` (an anonymous class) with nothing after it but a
 * header's parameters, `extends` and `with`. A declaration is a field when the innermost open brace is a template's.
 */
object StatefulFieldScan {

  /** One kept field: where it is declared, its name, the types its entries are keyed by and hold (a set's: its element),
   *  and the declaration's line. */
  final case class Field(path: String, name: String, line: Int, key: String, value: String, text: String) {
    def id: String = s"$path#$name"
  }

  /** A mutable map, set or Memo built (or typed) in a declaration, with its type arguments when it names them. */
  private val Stateful =
    ("""(?:\bmutable\.(?:HashMap|LinkedHashMap|Map|TreeMap|LongMap|HashSet|LinkedHashSet|Set|BitSet)|\bTrieMap|""" +
      """\bConcurrentHashMap|\bIdentityHashMap|\bjava\.util\.HashMap|\bMemo(?=\[))\b(?:\.(?:empty|newKeySet|from))?\s*(\[)?""").r
  /** An immutable map or set held in a `var`: state just the same, swapped whole. */
  private val SwappedWhole = """(?<![\w.])(?:Map|Set)\b(?:\.empty)?\s*(\[)""".r

  private val Declaration = """^\s*(?:(?:private|protected)(?:\[\w+\])?\s+|@volatile\s+|override\s+|final\s+|lazy\s+)*(val|var)\s+(\w+)\b""".r
  private val TemplateHeader = """\b(?:class|trait|object|enum)\s+\w+|\bnew\s+[\w.]+(?:\[[^\]]*\])?""".r

  /** Every kept field in `src` (comments already dropped by [[ScalaSourceScan.code]], lines preserved). */
  def fieldsIn(path: String, src: String): Seq[Field] = {
    val lines = src.split("\n", -1)
    val starts = lines.scanLeft(0)(_ + _.length + 1)
    val templateAt = templateLines(src, starts)
    lines.indices.flatMap { i =>
      Declaration.findFirstMatchIn(lines(i)).filter(_ => templateAt(i)).flatMap { decl =>
        val rest  = lines(i).substring(decl.end)
        val found = Stateful.findFirstMatchIn(rest).orElse(Option.when(decl.group(1) == "var")(SwappedWhole.findFirstMatchIn(rest)).flatten)
        found.map { m =>
          val typeArgs = if (m.group(1) != null) ScalaSourceScan.topLevelParts(bracketed(rest, m.end - 1)).map(_.trim) else Nil
          Field(path, decl.group(2), i + 1, keyOf(m.matched, typeArgs), typeArgs.lastOption.getOrElse("?"), lines(i).trim)
        }
      }
    }
  }

  def fieldsIn(path: Path): Seq[Field] = fieldsIn(path.toString, ScalaSourceScan.codeOf(path))

  /** What a collection is keyed by: its first type argument, the element of a set, `Long` for a LongMap. */
  private def keyOf(collection: String, typeArgs: Seq[String]): String =
    if (collection.contains("LongMap")) "Long"
    else if (collection.contains("BitSet")) "Int"
    else typeArgs.headOption.getOrElse("?")

  /** The text inside the `[` at `open`, up to its matching `]`. */
  private def bracketed(text: String, open: Int): String = {
    var depth = 0
    var i     = open
    while (i < text.length) {
      text(i) match {
        case '[' => depth += 1
        case ']' =>
          depth -= 1
          if (depth == 0) return text.substring(open + 1, i)
        case _ =>
      }
      i += 1
    }
    text.substring(open + 1)
  }

  /** For each line, whether the innermost brace open at its start is a template body's. */
  private def templateLines(src: String, starts: Array[Int]): Int => Boolean = {
    val atLine = new Array[Boolean](starts.length)
    var stack  = List(true) // a file's top level declares like a template
    var from   = 0
    var line   = 0
    var i      = 0
    while (i < src.length) {
      while (line + 1 < starts.length && starts(line + 1) <= i) { line += 1; atLine(line) = stack.head }
      src(i) match {
        case '"' => i = skipString(src, i)
        case '{' =>
          stack = opensTemplate(src.substring(from, i)) :: stack
          from = i + 1
        case '}' =>
          if (stack.tail.nonEmpty) stack = stack.tail
          from = i + 1
        case _ =>
      }
      i += 1
    }
    atLine(0) = true
    idx => idx < atLine.length && atLine(idx)
  }

  /** The index of the closing quote of the string starting at `open` (a `"""` one whole). */
  private def skipString(src: String, open: Int): Int =
    if (src.startsWith("\"\"\"", open)) { val end = src.indexOf("\"\"\"", open + 3); if (end < 0) src.length else end + 2 }
    else {
      var i = open + 1
      while (i < src.length && src(i) != '"' && src(i) != '\n') { if (src(i) == '\\') i += 1; i += 1 }
      i
    }

  /** Does `before` (the text since the last brace) end in a template header: `class X(...) extends Y with Z`? */
  private[tools] def opensTemplate(before: String): Boolean =
  {
    val header = withoutParens(before)
    TemplateHeader.findAllMatchIn(header).toSeq.lastOption.exists { m =>
      val tail = header.substring(m.end)
      !tail.replace("=>", "").contains("=") && """\b(?:def|val|var)\b""".r.findFirstIn(tail).isEmpty
    }
  }

  private def withoutParens(text: String): String = {
    val out   = new StringBuilder
    var depth = 0
    text.foreach {
      case '(' | '[' => depth += 1
      case ')' | ']' => depth -= 1
      case c if depth == 0 => out += c
      case _ =>
    }
    out.toString
  }
}
