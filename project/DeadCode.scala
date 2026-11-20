import sbt._

/**
 * DEAD-CODE CANDIDATES (`deadCode`, scripts/dead-code): production classes, objects and methods nothing calls, read from
 * Zinc's own record -- the class dependencies the coupling report and the test-impact selection read (project/
 * CouplingAnalysis.scala, project/TestImpact.scala) -- and from the class files: a source's classes' uses of each other
 * (which Zinc does not record, `Coupling.sameFileEdges`) and each class's declared methods (`ClassReferences.Shape`).
 *
 *  - A top-level class or object of a deployed module's `src/main` is UNREFERENCED when no other top-level class of any
 *    analysis -- production, generated (the Play router, the Twirl views), fixtures or tests -- depends on it; TEST-ONLY
 *    when only test-side classes (`src/test`, `src/it`, `src/page`, `src/fixtures`, and testkit, itkit, e2e, repochecks,
 *    quality) do.
 *  - A method a production class file declares public -- a Scala public, protected or package-qualified `def`; not a
 *    field's accessor, a synthetic or bridge method, or a name holding `$` (a default argument, a private one mangled)
 *    -- is UNCALLED when no compiled class uses its NAME (Zinc's used names), TEST-ONLY when only test-side classes do.
 *    Matching by name is conservative: an unrelated method of the same name keeps it alive, never the other way round,
 *    and an override is alive whenever its parent's method is called. A class's own uses count: a method called only
 *    inside its class is alive (it may want `private`, which is not this tool's question).
 *
 * Entry points are never candidates: a class with a `main`, every class a production resource or launcher file names
 * (`routes`, `application.conf`, `META-INF/services`, the AOT class list, the launchers' JVM options under `infra/jvm`:
 * `mentioned`: kept as classes, their methods still read), and the methods the compiler or a framework calls by
 * convention ([[Conventional]]), an anonymous class's, and a `@targetName`'d one. A method of an unreferenced class is
 * reported with its class, not on its own.
 */
object DeadCode {

  /** One candidate: `class` or `method`, `unreferenced` or `test-only`, its full name, module and source. */
  final case class Candidate(kind: String, verdict: String, name: String, module: String, source: String) {
    def row: String = s"$kind\t$verdict\t$name\t$module\t$source"
  }

  /** Methods the compiler synthesises or a framework or the language calls by name, never by a call Zinc records. */
  val Conventional: Set[String] = Set("main", "apply", "unapply", "unapplySeq", "copy", "equals", "hashCode", "toString",
    "canEqual", "productArity", "productElement", "productElementName", "productElementNames", "productPrefix",
    "productIterator", "fromProduct", "ordinal", "values", "valueOf", "fromOrdinal", "writeReplace", "readResolve",
    "finalize", "clone", "<init>", "<clinit>")

  private val TestSideModules = Set("testkit", "itkit", "e2e", "repochecks", "quality")

  /** Whether a source of `module` at `path` is test-side: a test module's, or a source set other than `src/main`. */
  def isTestSide(module: String, path: String): Boolean =
    TestSideModules(module) || !path.startsWith(s"$module/src/main/") && path.matches("[^/]+/src/.*")

  private final case class Owner(top: String, module: String, source: String, production: Boolean, testSide: Boolean)

  /** `classFile` reads a product's bytes; `libraryMethods` the method names a library class (a binary name) and its
   *  supertypes declare -- a method overriding one is the library's to call (Play's `invokeBlock`, a Reactive Streams
   *  `onNext`), never a call Zinc records. */
  def find(repo: File, units: Seq[TestImpact.Unit], mentioned: String => Boolean, classFile: xsbti.VirtualFileRef => Array[Byte],
           libraryMethods: String => Set[String]): Seq[Candidate] = {
    import kinowo.build.{ClassReferences, Coupling}
    val all = units.map(u => u.module -> u.analysis.asInstanceOf[sbt.internal.inc.Analysis])
    val owner = scala.collection.mutable.Map.empty[String, Owner]
    val sameFile = Set.newBuilder[(String, String)]
    val compiled = Seq.newBuilder[(ClassReferences.Compiled, Owner)]
    all.foreach { case (module, a) =>
      a.relations.classes.forwardMap.foreach { case (src, names) =>
        val path = TestImpact.repoPath(repo, src)
        val tops = Coupling.topLevel(names.toSet.map(Coupling.normalize))
        val test = isTestSide(module, path)
        val production = !test && path.startsWith(s"$module/src/main/")
        names.foreach(n => owner(n) = Owner(tops(Coupling.normalize(n)), module, path, production, test))
        if (production) {
          val products = a.relations.products(src).toSeq.map(p => ClassReferences.read(classFile(p)))
          compiled ++= products.map(_ -> owner(names.head))
          if (tops.values.toSet.size > 1) {
            val binaries = names.toSeq.flatMap(n => a.relations.productClassName.forward(n).map(_ -> tops(Coupling.normalize(n)))).toMap
            sameFile ++= Coupling.sameFileEdges(products, binaries)
          }
        }
      }
    }
    val sourceNameOf: Map[String, String] = all.flatMap { case (_, a) =>
      a.relations.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap
    // who depends on each top-level class, and who uses each name: (production or generated, test-side)
    val users = scala.collection.mutable.Map.empty[String, (Boolean, Boolean)].withDefaultValue((false, false))
    def use(table: scala.collection.mutable.Map[String, (Boolean, Boolean)], key: String, testSide: Boolean): scala.Unit = {
      val (main, test) = table(key)
      table(key) = if (testSide) (main, true) else (true, test)
    }
    sameFile.result().foreach { case (_, to) => use(users, to, testSide = false) }
    all.foreach { case (_, a) =>
      (a.relations.internalClassDep.all.toSeq ++
        a.relations.externalClassDep.all.toSeq.map { case (from, to) => from -> sourceNameOf.getOrElse(to, to) }).foreach { case (from, to) =>
        (owner.get(from), owner.get(to)) match {
          case (Some(f), Some(t)) if f.top != t.top => use(users, t.top, f.testSide)
          case _                                    => ()
        }
      }
    }
    val nameUsers = scala.collection.mutable.Map.empty[String, (Boolean, Boolean)].withDefaultValue((false, false))
    all.foreach { case (_, a) =>
      sbt.internal.inc.UsedNamesAccess.of(a.relations).foreach { case (cls, used) =>
        val testSide = owner.get(cls).forall(_.testSide)
        used.foreach(use(nameUsers, _, testSide))
      }
    }
    def verdict(used: (Boolean, Boolean)): Option[String] = used match {
      case (true, _)      => None
      case (false, true)  => Some("test-only")
      case (false, false) => Some("unreferenced")
    }
    val classFiles = compiled.result()
    val tops = owner.values.filter(_.production).groupBy(_.top).map { case (top, owners) => top -> owners.head }
    val hasMain = classFiles.collect { case (c, o) if c.shape.methods.exists(_.name == "main") => o.top }.toSet
    val entry: String => Boolean = top => mentioned(top) || hasMain(top)
    val classes = tops.toSeq.flatMap { case (top, o) =>
      if (entry(top)) None else verdict(users(top)).map(v => Candidate("class", v, top, o.module, o.source))
    }
    val dead = classes.map(_.name).toSet
    val texts = scala.collection.mutable.Map.empty[String, String]
    def sourceText(path: String): String = texts.getOrElseUpdate(path, IO.read(repo / path))
    val byBinary = classFiles.map { case (c, _) => c.name -> c }.toMap
    // the library supertypes of a class: walked up through our own classes' superclasses and interfaces
    def librarySupers(name: String, seen: Set[String] = Set.empty): Set[String] =
      if (seen(name)) Set.empty
      else byBinary.get(name).fold(Set(name)) { c =>
        (c.shape.superName +: c.shape.interfaces).filter(_.nonEmpty).toSet.flatMap((p: String) => librarySupers(p, seen + name))
      }
    // the methods our own supertypes of a class declare: a trait's method is reported on the trait, not on each class
    // that carries its mixin forwarder
    def ownSuperMethods(name: String, seen: Set[String] = Set.empty): Set[String] = byBinary.get(name).fold(Set.empty[String]) { c =>
      (c.shape.superName +: c.shape.interfaces).filter(p => p.nonEmpty && !seen(p) && byBinary.contains(p)).toSet
        .flatMap((p: String) => byBinary(p).shape.methods.map(_.name).toSet ++ ownSuperMethods(p, seen + name))
    }
    val methods = classFiles.flatMap { case (c, o) =>
      if (dead(o.top) || c.name.contains("$anon")) Nil
      else {
        val accessors = c.shape.fields.map(_.name).toSet
        lazy val inherited = (librarySupers(c.name) - "java.lang.Object").flatMap(libraryMethods) ++ ownSuperMethods(c.name)
        // a `@targetName("x")` method is `x` in its class file and its source name to every caller
        lazy val renamed = TargetName.findAllMatchIn(sourceText(o.source)).map(_.group(1)).toSet
        c.shape.methods.collect { case m if isDeclared(m.name, m.flags) && !accessors(m.name) && !inherited(m.name) && !renamed(m.name) => m.name }.distinct
          .flatMap(m => verdict(nameUsers(m)).map(v => Candidate("method", v, s"${Coupling.normalize(c.name)}.$m", o.module, o.source)))
      }
    }
    (classes ++ methods).distinct.sortBy(c => (c.kind, c.verdict, c.name))
  }

  private val TargetName = """@targetName\("([^"]+)"\)""".r

  // ACC_PUBLIC, and not ACC_BRIDGE or ACC_SYNTHETIC
  private def isDeclared(name: String, flags: Int): Boolean =
    (flags & 0x0001) != 0 && (flags & 0x1040) == 0 && !name.contains('$') && !Conventional(name) && !name.matches("_\\d+")

  /** Whether `text` names a class: its full name, or a dotted prefix of a name it holds (`controllers.Films.show` names
   *  `controllers.Films`), a binary name's `$` read as a dot. */
  def mentions(text: String): String => Boolean = {
    val words = "[A-Za-z_][A-Za-z0-9_$.]*".r.findAllIn(text).map(_.replace('$', '.').stripSuffix(".")).toSet
    words.flatMap(w => w.split('.').inits.filter(_.nonEmpty).map(_.mkString(".")))
  }
}
