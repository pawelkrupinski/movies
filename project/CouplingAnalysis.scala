import sbt._
import xsbti.api.DefinitionType

/**
 * The production class graph (`kinowo.build.Coupling.Graph`) read from the modules' Compile analyses -- Zinc's own record,
 * as the test-impact selection reads it (project/TestImpact.scala): each module's classes of the sources under its
 * `<module>/src/main/` (generated sources -- Twirl views, routes -- are left out), the class->class dependencies within
 * a module (`internalClassDep`) and across modules (`externalClassDep`, another module's binary name mapped back to its
 * source name), each class folded into its top-level class, and whether that is a trait or an abstract class (its
 * recorded API). For a source of two or more top-level classes, Zinc's record is corrected from its class files: Zinc
 * records no dependency between two classes of ONE source file, so their uses of each other come from the class files
 * (`Coupling.sameFileEdges`); and it charges every import of the file to one of them (`Coupling.importOwner`), which
 * keeps only the dependencies its own class files confirm or no other class of the file accounts for
 * (`Coupling.withoutImportCharge`).
 */
object CouplingAnalysis {
  import kinowo.build.{ClassReferences, Coupling}

  /** A source of two or more top-level classes: its binary names -> top-level classes, its compiled classes, its text. */
  private final case class SharedSource(tops: Map[String, String], compiled: Seq[ClassReferences.Compiled], text: String)

  def graph(repo: File, units: Seq[(String, xsbti.compile.CompileAnalysis)], converter: xsbti.FileConverter): Coupling.Graph = {
    val all = units.map { case (module, a) => module -> a.asInstanceOf[sbt.internal.inc.Analysis] }
    // class -> its top-level class, and top-level class -> (module, abstract), for production sources only
    val topOf = scala.collection.mutable.Map.empty[String, String]
    val types = Seq.newBuilder[Coupling.Type]
    val shared = Seq.newBuilder[SharedSource]
    all.foreach { case (module, a) =>
      a.relations.classes.forwardMap.foreach { case (src, names) =>
        if (TestImpact.repoPath(repo, src).startsWith(s"$module/src/main/")) {
          val tops = Coupling.topLevel(names.toSet.map(Coupling.normalize))
          names.foreach(n => topOf(n) = tops(Coupling.normalize(n)))
          // a type is abstract when its recorded API says so, under its own name or its object's
          tops.values.toSeq.distinct.foreach { t =>
            types += Coupling.Type(t, module, names.exists(n => Coupling.normalize(n) == t && isAbstract(a, n)))
          }
          if (tops.values.toSet.size > 1) {
            val binaries = names.toSeq.flatMap(n => a.relations.productClassName.forward(n).map(_ -> tops(Coupling.normalize(n)))).toMap
            val compiled = a.relations.products(src).toSeq.map(p => ClassReferences.read(java.nio.file.Files.readAllBytes(converter.toPath(p))))
            shared += SharedSource(binaries, compiled, new String(java.nio.file.Files.readAllBytes(converter.toPath(src)), "UTF-8"))
          }
        }
      }
    }
    val sourceNameOf: Map[String, String] = all.flatMap { case (_, a) =>
      a.relations.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap
    val topOfBinary: Map[String, String] = sourceNameOf.collect { case (binary, name) if topOf.contains(name) => binary -> topOf(name) }
    val zinc = all.flatMap { case (_, a) =>
      a.relations.internalClassDep.all.toSeq ++
        a.relations.externalClassDep.all.toSeq.map { case (from, to) => from -> sourceNameOf.getOrElse(to, to) }
    }.collect { case (from, to) if topOf.contains(from) && topOf.contains(to) => topOf(from) -> topOf(to) }
      .filter { case (from, to) => from != to }.toSet
    val sources = shared.result()
    val edges = sources.foldLeft(zinc) { (edges, s) =>
      Coupling.importOwner(s.text, s.tops.values.toSet).fold(edges) { owner =>
        Coupling.withoutImportCharge(edges, owner, Coupling.classFileUses(s.compiled, s.tops, topOfBinary).getOrElse(owner, Set.empty), s.tops.values.toSet)
      }
    }
    Coupling.Graph(types.result().sortBy(_.name), edges ++ sources.flatMap(s => Coupling.sameFileEdges(s.compiled, s.tops)))
  }

  private def isAbstract(a: sbt.internal.inc.Analysis, name: String): Boolean =
    a.apis.internal.get(name).exists { analyzed =>
      val cls = analyzed.api().classApi()
      cls.definitionType() == DefinitionType.Trait || cls.modifiers().isAbstract
    }
}
