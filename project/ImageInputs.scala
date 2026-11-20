import sbt.*
import sbt.Keys.*

/**
 * EVERY REPOSITORY PATH A DEPLOYABLE'S IMAGE IS BUILT FROM, derived from the build itself.
 *
 * Each tier's publish in `.github/workflows/main.yml` runs only when its `changed-<tier>` path filter matched since the
 * tier's `deployed-<tier>` marker. That filter is a hand-kept list, and a hand-kept list drifts from what the image
 * holds: `test/resources/fixtures/identity-judgements.tsv`, which the worker bundles as a resource, was missing from the
 * worker's, so an edit to it reached prod only with the next unrelated worker deploy. `ImagePathGateSpec` reads this
 * task's answer (staged into the worker's test resources) and fails when a tier's filter misses any path of it.
 *
 * What an image holds: the main sources and resources of every module in the deployable's COMPILE dependency closure
 * (test, it, page and fixtures edges ship nothing), the repository files outside those a module bundles or reads into
 * its dist ([[imageRepoFiles]], declared by whatever bundles one), and the build definition and the Dockerfile -- less
 * its [[buildOnly]] files.
 */
object ImageInputs {

  /**
   * THE BUILD DEFINITION NO IMAGE IS BUILT FROM: the files of `project/` that only measure or test the code (the coupling
   * and test-impact reports, the test-fork and classpath guards, a batch's stale-TASTy retry, `sbt run`'s hooks, this
   * file). A change to one ships an image of the same bytes, and publishing it restarted every worker into a full
   * identity re-resolve for nothing (d7a733703, `project/Coupling.scala`, 2026-11-20). Both tiers' publish filters in
   * main.yml exclude exactly these, and `ImagePathGateSpec` holds each to it: no setting the dist is staged from is
   * defined in one or calls one (the [[stageDefinitions]] below), and no other file of `project/` uses one. A file that
   * starts building the image leaves this list.
   */
  val buildOnly: Seq[String] = Seq(
    "ClassCycles.scala", "ClassReferences.scala", "Coupling.scala", "CouplingAnalysis.scala", "CouplingSimulation.scala",
    "ExemptionSlack.scala", "ForkedTests.scala",
    "HeadlessSafeInteractionMode.scala", "ImageInputs.scala", "ReaderNames.scala", "StaleTastyRetry.scala",
    "TestImpact.scala", "UnitClasspathGuard.scala", "Warmup.scala", "WriteFailureTripwire.scala",
  ).map("project/" + _)

  val stageDefinitions = taskKey[Seq[String]](
    "Where each setting this deployable's staged dist (`Universal / stage`) is built from is defined in the repository: " +
      "`<path>\t<first line>\t<last line>`, sorted.")

  val imageRepoFiles = settingKey[Seq[File]](
    "Repository files outside any module's main sources and resources that this module's build bundles or reads into " +
      "its dist (a copied resource, the launcher's JVM options, a staged script). Whatever bundles one declares it " +
      "here, so imageInputs sees it.")

  val imageInputs = taskKey[Seq[String]](
    "Every repository path (repository-relative, sorted) this deployable's image is built from.")

  /** The modules whose Compile classpath the deployable's own Compile reaches, itself first. */
  def compileClosure(dependencies: sbt.internal.BuildDependencies, root: ProjectRef): Seq[ProjectRef] = {
    def reachesCompile(configuration: Option[String]): Boolean =
      configuration.getOrElse("compile").split(";").exists { mapping =>
        mapping.split("->").head.split(",").map(_.trim).contains("compile")
      }
    def walk(pending: List[ProjectRef], seen: Vector[ProjectRef]): Vector[ProjectRef] = pending match {
      case Nil => seen
      case next :: rest if seen.contains(next) => walk(rest, seen)
      case next :: rest =>
        val onward = dependencies.classpath.getOrElse(next, Nil).filter(dep => reachesCompile(dep.configuration)).map(_.project)
        walk(rest ++ onward, seen :+ next)
    }
    walk(List(root), Vector.empty)
  }

  /** `imageRepoFiles` empty unless a module declares one; `imageInputs` on a deployable. */
  val buildSettings: Seq[Setting[?]] = Seq(ThisBuild / imageRepoFiles := Nil)

  val deployableSettings: Seq[Setting[?]] = Seq(
    imageInputs := Def.taskDyn {
      val repo    = (LocalRootProject / baseDirectory).value
      val modules = compileClosure(buildDependencies.value, thisProjectRef.value)
      val inModules = ScopeFilter(inProjects(modules*), inConfigurations(Compile))
      val modulesThemselves = ScopeFilter(inProjects(modules*))
      Def.task {
        val mainTrees = (Compile / sourceDirectory).all(inModules).value.flatMap(directory => (directory ** "*").get())
        val unmanaged = unmanagedSources.all(inModules).value.flatten ++ unmanagedResources.all(inModules).value.flatten
        val bundled   = imageRepoFiles.all(modulesThemselves).value.flatten
        val buildDefinition =
          (repo / "project" * ("*.scala" || "*.sbt" || "*.properties")).get() :+ (repo / "build.sbt") :+ (repo / "Dockerfile")
        (mainTrees ++ unmanaged ++ bundled ++ buildDefinition)
          .filter(_.isFile)
          .flatMap(file => IO.relativize(repo, file))
          .filterNot(path => path.split('/').contains("target") || buildOnly.contains(path))
          .distinct
          .sorted
      }
    }.value,
    stageDefinitions := {
      val repo      = (LocalRootProject / baseDirectory).value
      val structure = Keys.buildStructure.value
      // The stage, and each module's jar it holds: the stage reaches the other modules' through dynamic tasks only.
      val stage    = Def.ScopedKey(Scope(Select(thisProjectRef.value), Select(ConfigKey("universal")), Zero, Zero), com.typesafe.sbt.packager.Keys.stage.key)
      val jars     = compileClosure(buildDependencies.value, thisProjectRef.value)
        .map(module => Def.ScopedKey(Scope(Select(module), Select(ConfigKey("compile")), Zero, Zero), packageBin.key))
      definitionsBehind(stage +: jars, structure, repo)
    },
  )

  /**
   * Where every setting `roots` read, transitively, is defined: its repository file and lines. sbt keeps each setting's
   * position (a `.sbt` file's absolute path, a `project/` source's path as it was compiled, wherever that checkout was);
   * the plugins' and sbt's own lie outside the repository and are left out. A dynamic task's (`taskDyn`) inner reads
   * are not in the static graph and so not here.
   */
  def definitionsBehind(roots: Seq[Def.ScopedKey[?]], structure: sbt.internal.BuildStructure, repo: File): Seq[String] = {
    val compiled = Def.compiled(structure.settings, actual = true)(structure.delegates, structure.scopeLocal, Def.showFullKey)
    def repoPath(path: String): Option[String] = {
      val name = path.split('/').last
      if (path.contains("/project/") && (repo / "project" / name).isFile) Some(s"project/$name")
      else IO.relativize(repo, new File(path)).filter(relative => (repo / relative).isFile)
    }
    val seen  = scala.collection.mutable.Set.empty[Def.ScopedKey[?]]
    val found = scala.collection.mutable.Set.empty[String]
    def walk(key: Def.ScopedKey[?]): Unit = if (seen.add(key)) compiled.get(key).foreach { setting =>
      setting.settings.foreach { definition =>
        val located = definition.pos match {
          case RangePosition(path, range) => repoPath(path).map(p => s"$p\t${range.start}\t${range.end}")
          case LinePosition(path, line)   => repoPath(path).map(p => s"$p\t$line\t$line")
          case _                          => None
        }
        found ++= located
      }
      setting.dependencies.foreach(walk)
    }
    roots.foreach(walk)
    found.toSeq.sorted
  }

  /** Stages each deployable's [[imageInputs]] as `image-inputs/<name>.txt` and its [[stageDefinitions]] as
   *  `image-inputs/<name>-stage-definitions.txt` among the `config`'s resources, and [[buildOnly]] as
   *  `image-inputs/build-only.txt`. */
  def staged(config: Configuration, deployables: ProjectReference*): Setting[?] = {
    val each = ScopeFilter(inProjects(deployables*))
    config / resourceGenerators += Def.task {
      val directory = (config / resourceManaged).value / "image-inputs"
      def write(fileName: String, lines: Seq[String]): File = {
        val file = directory / fileName
        IO.writeLines(file, lines)
        file
      }
      val names = name.all(each).value
      write("build-only.txt", buildOnly) +:
        (names.zip(imageInputs.all(each).value).map { case (deployable, paths) => write(s"$deployable.txt", paths) } ++
          names.zip(stageDefinitions.all(each).value).map { case (deployable, lines) =>
            write(s"$deployable-stage-definitions.txt", lines)
          })
    }.taskValue
  }
}
