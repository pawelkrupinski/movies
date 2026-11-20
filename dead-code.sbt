// DEAD-CODE CANDIDATES (project/DeadCode.scala, scripts/dead-code): `deadCode <out>` writes `kind<TAB>verdict<TAB>name
// <TAB>module<TAB>source` rows -- production classes no other class depends on and methods whose name no class uses --
// less the entry points the production resources and launcher files name. It reads every module's main, fixtures,
// test, integration and page analyses (project/AnalysisUnits.scala's `callers`).
lazy val deadCode = inputKey[Unit]("production classes and members nothing calls: deadCode <out>")

deadCode := {
  val args = Def.spaceDelimited("<out>").parsed
  require(args.size == 1, "deadCode <out>")
  val repo  = baseDirectory.value
  val units = AnalysisUnits.callers.value
  // the files that name a class to load by reflection or at boot: routes, configuration, service registrations, the AOT
  // class list, the launchers' JVM options
  val named = Seq("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")
    .flatMap(m => ((repo / m / "src" / "main" / "resources") ** ("routes" || "*.conf" || "*.txt" || "*.properties")).get ++
      ((repo / m / "src" / "main" / "resources" / "META-INF" / "services") * "*").get) ++ ((repo / "infra" / "jvm") * "*.options").get
  val mentioned = DeadCode.mentions(named.filter(_.length < 4000000).map(IO.read(_)).mkString("\n"))
  val converter = fileConverter.value
  // the deployed apps' library classes, to read the methods a production class overrides from them
  val libraries = ((LocalProject("web") / Compile / dependencyClasspath).value ++ (LocalProject("worker") / Compile / dependencyClasspath).value)
    .map(_.data).filter(_.getName.endsWith(".jar")).distinct
  val loader = new java.net.URLClassLoader(libraries.map(_.toURI.toURL).toArray, ClassLoader.getPlatformClassLoader)
  val libraryMethods = (binary: String) =>
    scala.util.Try(Class.forName(binary, false, loader)).toOption.fold(Set.empty[String]) { c =>
      Iterator.iterate[Class[_]](c)(_.getSuperclass).takeWhile(_ != null).flatMap(k => (k +: k.getInterfaces.toSeq).flatMap(i =>
        (i.getDeclaredMethods ++ i.getMethods).map(_.getName))).toSet
    }
  val found =
    try DeadCode.find(repo, units, mentioned, ref => java.nio.file.Files.readAllBytes(converter.toPath(ref)), libraryMethods)
    finally loader.close()
  IO.writeLines(file(args(0)), found.map(_.row))
  streams.value.log.info(s"deadCode: ${found.size} candidate(s) to ${args(0)}")
}

