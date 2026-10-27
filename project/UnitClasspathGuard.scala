import sbt._
import sbt.Keys._

/**
 * No unit Test classpath holds a way to reach a real dependency (the owner, 2026-10-27: "unit tests have all external
 * deps mocked, so those with real deps move to it or e2e"; "structurally enforce").
 *
 * The integration support -- the Mongo cluster helpers, the child JVM, git, keytool, the repository's scripts -- lives
 * in the `itkit` module, which only the `it`, page and e2e configs depend on; the libraries that start a real database
 * or browser are on no unit classpath either. A unit spec reaching for one does not compile, and should a dependency
 * edit put one back on a unit classpath, this guard fails every unit Test task of that module with the entry named.
 *
 * What it cannot see -- a production class that talks to a real thing (the Mongo driver, `RealHttpFetch`) is on every
 * classpath through main -- `NoRealDependenciesInUnitSpecsSpec` lints for.
 */
object UnitClasspathGuard {

  /** The projects a unit classpath may not hold. */
  val ForbiddenProjects: Set[String] = Set("itkit")

  /** The browser drivers' organisations: test kits bring them for browser suites (scalatestplus-play's, Play's). */
  val BrowserOrganizations: Seq[String] = Seq("org.seleniumhq.selenium", "org.htmlunit", "net.sourceforge.htmlunit")

  /** Library organisations that start or drive a real database or browser. */
  val ForbiddenOrganizations: Seq[String] =
    Seq("org.testcontainers", "de.flapdoodle", "com.microsoft.playwright", "io.github.bonigarcia") ++ BrowserOrganizations

  /** The entries of `classpath` (each its module, else its path) no unit classpath may hold, but `allowed`'s. */
  def forbidden(classpath: Seq[(Option[ModuleID], File)], allowed: Seq[String]): Seq[String] = classpath.collect {
    case (Some(m), _) if ForbiddenProjects(m.name) ||
        ForbiddenOrganizations.exists(o => m.organization.startsWith(o) && !allowed.exists(m.organization.startsWith)) =>
      s"${m.organization}:${m.name}"
    case (None, file) if ForbiddenProjects.exists(p => file.getPath.contains(s"/$p/target/")) => file.getPath
  }.distinct

  /** `Test / fullClasspath`, refused when it holds a [[forbidden]] entry. */
  val settings: Seq[Setting[_]] = allowing(Nil)

  /** [[settings]], but for libraries of `organizations` a module's test kit cannot load without -- never itkit. */
  def allowing(organizations: Seq[String]): Seq[Setting[_]] = Seq(
    Test / fullClasspath := {
      val classpath = (Test / fullClasspath).value
      val found     = forbidden(classpath.map(entry => (entry.get(moduleID.key), entry.data)), organizations)
      if (found.nonEmpty)
        sys.error(s"${thisProject.value.id}'s unit Test classpath holds what reaches a real dependency: ${found.mkString(", ")}. " +
          "A spec needing it belongs in an `it` config (project/UnitClasspathGuard.scala).")
      classpath
    },
  )
}
