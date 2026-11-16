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
 * its dist ([[imageRepoFiles]], declared by whatever bundles one), and the build definition and the Dockerfile.
 */
object ImageInputs {

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
          .filterNot(path => path.split('/').contains("target"))
          .distinct
          .sorted
      }
    }.value,
  )

  /** Stages each deployable's [[imageInputs]] as `image-inputs/<name>.txt` among the `config`'s resources. */
  def staged(config: Configuration, deployables: ProjectReference*): Setting[?] = {
    val each = ScopeFilter(inProjects(deployables*))
    config / resourceGenerators += Def.task {
      val directory = (config / resourceManaged).value / "image-inputs"
      name.all(each).value.zip(imageInputs.all(each).value).map { case (deployableName, paths) =>
        val file = directory / s"$deployableName.txt"
        IO.writeLines(file, paths)
        file
      }
    }.taskValue
  }
}
