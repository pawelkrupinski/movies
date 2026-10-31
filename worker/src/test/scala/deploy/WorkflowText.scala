package deploy

/**
 * A workflow read as text (RepoFile reads workflows as text): a job's own keys, each with its whole value, and its steps.
 */
object WorkflowText {

  /** A line without its trailing `# comment`: a `#` after whitespace, outside every `${{ }}` and quoted string. */
  def stripComment(line: String): String = {
    var depth, quote = 0
    var i            = 0
    while (i < line.length) {
      if (line.startsWith("${{", i)) depth += 1
      else if (line.startsWith("}}", i) && depth > 0) depth -= 1
      else if (line(i) == '\'' && depth > 0) quote ^= 1
      else if (line(i) == '#' && depth == 0 && quote == 0 && (i == 0 || line(i - 1).isWhitespace))
        return line.take(i).replaceAll("\\s+$", "")
      i += 1
    }
    line
  }

  /** A job's own keys (not its steps'), each with its whole value, continuation lines included, comments dropped. */
  def keys(job: String): Map[String, String] = {
    val lines = job.linesIterator.toVector.drop(1).map(stripComment).filter(_.trim.nonEmpty)
    if (lines.isEmpty) Map.empty
    else {
      val indent = lines.head.takeWhile(_ == ' ').length
      val Key    = """^\s*([\w-]+):\s*(.*)$""".r
      lines
        .foldLeft(Vector.empty[(String, Vector[String])]) { (acc, line) =>
          line match {
            case Key(k, rest) if line.takeWhile(_ == ' ').length == indent => acc :+ (k -> Vector(rest))
            case _ if acc.nonEmpty                                       => acc.init :+ (acc.last._1 -> (acc.last._2 :+ line))
            case _                                                       => acc
          }
        }
        .map((k, v) => k -> v.mkString("\n"))
        .toMap
    }
  }

  /** The jobs a job needs. */
  def needs(job: String): Set[String] = keys(job).get("needs").toSet.flatMap { v =>
    v.replaceAll("[\\[\\]]", " ").split("[,\\s]+").map(_.stripPrefix("-").trim).filter(_.nonEmpty)
  }

  /** A job's steps, each as its own keys (see [[keys]]), in order. */
  def steps(job: String): Seq[Map[String, String]] = keys(job).get("steps").toSeq.flatMap { value =>
    val lines = value.linesIterator.toVector.filter(_.trim.nonEmpty)
    val dash  = lines.filter(_.trim.startsWith("- ")).map(_.takeWhile(_ == ' ').length).minOption
    dash.toSeq.flatMap { at =>
      val starts = lines.indices.filter(i => lines(i).takeWhile(_ == ' ').length == at && lines(i).trim.startsWith("- "))
      starts.zip(starts.drop(1) :+ lines.length).map { (from, until) =>
        val item = lines.slice(from, until)
        keys(("step:" +: (item.head.replaceFirst("- ", "  ") +: item.tail)).mkString("\n"))
      }
    }
  }
}
