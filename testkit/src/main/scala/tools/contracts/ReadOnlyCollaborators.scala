package tools.contracts

/**
 * The guard behind the persistence role splits (`MovieRepositoryReader` / `MovieChangeFeed` /
 * `MovieRepository`, `ScrapeArchiveReader` / `ScrapeArchiveRepository`, `TaskProducer` /
 * `TaskConsumer` / `TaskQueueMonitor` / `TaskQueue`). The split is compile-time only in one
 * direction: a component typed with the reader cannot write, but nothing stops a later edit
 * widening its parameter back to the writer, and that compiles. This reads the declared types
 * from the class files, so a spec can pin that a read-only component still takes the narrow role.
 *
 * An intersection parameter (`MovieRepositoryReader & MovieChangeFeed`) erases to its first part,
 * so it reads here as the reader — never as the writer.
 */
object ReadOnlyCollaborators {
  /** Where `cls` (its public constructors, and its methods named `methods`) declares a parameter
   *  or result of exactly the `writer` type — empty when it takes only narrower roles. */
  def writerTypedIn(cls: Class[?], writer: Class[?], methods: String*): Seq[String] = {
    val constructors = cls.getConstructors.toSeq.flatMap { constructor =>
      constructor.getParameterTypes.toSeq.zipWithIndex.collect {
        case (parameter, index) if parameter == writer => s"${cls.getSimpleName}'s constructor parameter ${index + 1}"
      }
    }
    val named = cls.getMethods.toSeq.filter(method => methods.contains(method.getName)).flatMap { method =>
      method.getParameterTypes.toSeq.zipWithIndex.collect {
        case (parameter, index) if parameter == writer => s"${cls.getSimpleName}.${method.getName}'s parameter ${index + 1}"
      } ++ Option.when(method.getReturnType == writer)(s"${cls.getSimpleName}.${method.getName}'s result")
    }
    val missing = methods.filterNot(name => cls.getMethods.exists(_.getName == name))
    require(missing.isEmpty, s"${cls.getName} has no public method ${missing.mkString(", ")}: the guard would check nothing")
    constructors ++ named
  }
}
