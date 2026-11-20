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
    val compiledAll = Seq.newBuilder[ClassReferences.Compiled]
    productionSources(repo, all).foreach { case (module, a, src, names, tops) =>
      names.foreach(n => topOf(n) = tops(Coupling.normalize(n)))
      // a type is abstract when its recorded API says so, under its own name or its object's
      tops.values.toSeq.distinct.foreach { t =>
        types += Coupling.Type(t, module, names.exists(n => Coupling.normalize(n) == t && isAbstract(a, n)))
      }
      val compiled = read(a, src, converter)
      compiledAll ++= compiled
      if (tops.values.toSet.size > 1) {
        val binaries = names.toSeq.flatMap(n => a.relations.productClassName.forward(n).map(_ -> tops(Coupling.normalize(n)))).toMap
        shared += SharedSource(binaries, compiled, new String(java.nio.file.Files.readAllBytes(converter.toPath(src)), "UTF-8"))
      }
    }
    val sourceNameOf: Map[String, String] = binarySources(all)
    val topOfBinary: Map[String, String] = sourceNameOf.collect { case (binary, name) if topOf.contains(name) => binary -> topOf(name) }
    val raw = all.flatMap { case (_, a) =>
      a.relations.internalClassDep.all.toSeq ++
        a.relations.externalClassDep.all.toSeq.map { case (from, to) => from -> sourceNameOf.getOrElse(to, to) }
    }.filter { case (from, to) => topOf.contains(from) && topOf.contains(to) && topOf(from) != topOf(to) }
    val zinc = raw.map { case (from, to) => topOf(from) -> topOf(to) }.toSet
    // the named types (a nested one too: `StageRelays.Relay`) each edge's target side is, as Zinc records them
    val zincUses: Map[(String, String), Set[String]] =
      raw.groupBy { case (from, to) => (topOf(from), topOf(to)) }.map { case (k, v) => k -> v.map(e => Coupling.normalize(e._2)).toSet }
    val sources = shared.result()
    val edges = sources.foldLeft(zinc) { (edges, s) =>
      Coupling.importOwner(s.text, s.tops.values.toSet).fold(edges) { owner =>
        Coupling.withoutImportCharge(edges, owner, Coupling.classFileUses(s.compiled, s.tops, topOfBinary).getOrElse(owner, Set.empty), s.tops.values.toSet)
      }
    }
    // DATA OR COLLABORATOR, judged per named type -- a nested one by itself, an object apart from its companion class: a
    // binary name's named type (a local or anonymous class folds into its enclosing one), and each one's top-level type
    val compiled = compiledAll.result()
    val unitOfBinary: Map[String, String] = sourceNameOf.collect { case (binary, name) if topOf.contains(name) => binary -> Coupling.Collaborators.unitOf(binary, name) }
    val topOfUnit: Map[String, String] = sourceNameOf.collect { case (binary, name) if topOf.contains(name) => Coupling.Collaborators.unitOf(binary, name) -> topOf(name) }
    val unitOf: String => Option[String] = Coupling.foldBinary(_, unitOfBinary)
    val reasons = Coupling.Collaborators.classify(compiled, unitOf)
    val classFileUses: Map[String, Set[String]] = compiled.flatMap(c => Coupling.foldBinary(c.name, topOfBinary).map(_ -> c.uses.flatMap(u => unitOf(u).toSeq)))
      .groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    val typed = types.result().sortBy(_.name)
    val graphEdges = edges ++ sources.flatMap(s => Coupling.sameFileEdges(s.compiled, s.tops))
    val coupled = Coupling.Collaborators.coupledEdges(graphEdges, classFileUses, zincUses, u => topOfUnit.getOrElse(u, u), reasons.contains)
    // each top-level type's own parents (its class's and its object's, not a nested class's), as top-level types
    val parents: Map[String, Set[String]] = compiled.flatMap { c =>
      unitOfBinary.get(c.name).filter(u => topOfUnit.get(u).contains(u.stripSuffix("$"))).map(u => u.stripSuffix("$") ->
        (c.shape.superName +: c.shape.interfaces).flatMap(p => topOfBinary.get(p)).filter(_ != u.stripSuffix("$")).toSet)
    }.groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    Coupling.Graph(typed, graphEdges, Coupling.Collaborators.data(topOfUnit.keys.toSeq, reasons), reasons, Some(coupled),
      Coupling.supertypeClosure(parents))
  }

  /** The class files of the production classes `tops`, by top-level class (each with its object, its nested classes
   *  and its anonymous ones), and every production binary name's top-level class: what ExemptionSlack reads. */
  def classFiles(repo: File, units: Seq[(String, xsbti.compile.CompileAnalysis)], converter: xsbti.FileConverter,
                 tops: Set[String]): (Map[String, Seq[ClassReferences.Compiled]], Map[String, String]) = {
    val all = units.map { case (module, a) => module -> a.asInstanceOf[sbt.internal.inc.Analysis] }
    val sources = productionSources(repo, all)
    val sourceNameOf = binarySources(all)
    val topOf: Map[String, String] = sources.flatMap { case (_, _, _, names, top) => names.map(n => n -> top(Coupling.normalize(n))) }.toMap
    val topOfBinary = sourceNameOf.collect { case (binary, name) if topOf.contains(name) => binary -> topOf(name) }
    val files = sources.filter { case (_, _, _, _, top) => top.values.exists(tops) }
      .flatMap { case (_, a, src, _, _) => read(a, src, converter) }
      .flatMap(c => Coupling.foldBinary(c.name, topOfBinary).filter(tops).map(_ -> c))
    (files.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }, topOfBinary)
  }

  /** Each module's sources under its `src/main/` (generated ones left out): the module, its analysis, the source, its
   *  class names and each one's top-level class (normalized). */
  private def productionSources(repo: File, all: Seq[(String, sbt.internal.inc.Analysis)]): Seq[(String, sbt.internal.inc.Analysis, xsbti.VirtualFileRef, Seq[String], Map[String, String])] =
    all.flatMap { case (module, a) =>
      a.relations.classes.forwardMap.toSeq.collect { case (src, names) if TestImpact.repoPath(repo, src).startsWith(s"$module/src/main/") =>
        (module, a, src, names.toSeq, Coupling.topLevel(names.toSet.map(Coupling.normalize)))
      }
    }

  /** Every binary name -> its source name (`a.Outer$Inner` -> `a.Outer.Inner`). */
  private def binarySources(all: Seq[(String, sbt.internal.inc.Analysis)]): Map[String, String] =
    all.flatMap { case (_, a) => a.relations.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap

  private def read(a: sbt.internal.inc.Analysis, src: xsbti.VirtualFileRef, converter: xsbti.FileConverter): Seq[ClassReferences.Compiled] =
    a.relations.products(src).toSeq.map(p => ClassReferences.read(java.nio.file.Files.readAllBytes(converter.toPath(p))))

  private def isAbstract(a: sbt.internal.inc.Analysis, name: String): Boolean =
    a.apis.internal.get(name).exists { analyzed =>
      val cls = analyzed.api().classApi()
      cls.definitionType() == DefinitionType.Trait || cls.modifiers().isAbstract
    }
}
