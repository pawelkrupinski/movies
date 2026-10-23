package deploy

/** The JVM options each deployed tier's launcher bakes into conf/application.ini: build.sbt's
 *  `launcherOptions(...)` call in that tier's project names infra/jvm/<file>.options, read here in
 *  the same order, so specs check what the build bakes rather than a copy of it. */
object LauncherOptions {

  /** The option files per tier (sbt project name), in their order on the command line. */
  val files: Map[String, Seq[String]] = {
    val Project = """(?m)^lazy val (\w+) = \(project""".r
    val Call    = """launcherOptions\(([^)]*)\)""".r
    val build   = RepoFile.read("build.sbt")
    val starts  = Project.findAllMatchIn(build).map(m => m.start -> m.group(1)).toSeq
    Call.findAllMatchIn(build).filterNot(_.group(1).contains("String*")).map { call =>
      val project = starts.filter(_._1 < call.start).last._2
      project -> "\"([^\"]+)\"".r.findAllMatchIn(call.group(1)).map(_.group(1)).toSeq
    }.toMap
  }

  /** One options file's flags, comments and blank lines dropped. */
  def file(name: String): Seq[String] =
    RepoFile.read(s"infra/jvm/$name").linesIterator.map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#")).toSeq

  /** Every flag the tier's launcher bakes, in order. */
  def of(tier: String): Seq[String] = files(tier).flatMap(file)
}
