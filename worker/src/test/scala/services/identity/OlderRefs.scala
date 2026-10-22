package services.identity

/**
 * Code newer than the oldest ref a tool may be pointed at, reached BY NAME. scripts/identity-slice and scripts/title-trace
 * drop this checkout's tool files (their TOOL_FILES, this one among them) into every ref they decide under — a deployed
 * worker's build, a bisect's first commit — so a tool file naming a member that ref lacks fails its compile, and with it
 * every `--ref` and `--bisect` reaching back past the member (`Listing.properTitle`, `TitleNormalizer.facts`,
 * 2026-10-17/18). Read through these, such a member is None where the ref has none; scripts/identity-slice-old-ref-test.sh
 * compiles both tools into refs from before them.
 */
object OlderRefs {

  /** The value of `record`'s case-class field `name`, where its class has one. */
  def field(record: Product, name: String): Option[Any] =
    record.productElementNames.zip(record.productIterator).collectFirst { case (`name`, value) => value }

  /** The text an optional (`Option[String]`) field holds: None where it holds none or the class has no such field. */
  def optionalText(record: Product, name: String): Option[String] = field(record, name).collect { case Some(text: String) => text }

  /** `on.member(args…)` (or a val's getter, with no args), where its class has such a public member. */
  def call(on: AnyRef, member: String, args: AnyRef*): Option[AnyRef] =
    on.getClass.getMethods.find(m => m.getName == member && m.getParameterCount == args.size).map(_.invoke(on, args*))

  /** `own` as the interface `iface`, the `members` named passed to `inner` instead — where `iface` has them. A tool file's
   *  wrapper overrides only what the oldest ref has, so it cannot override a lookup a newer ref added
   *  (`IdentityLookups.overview`, 2026-10-18): named here, the wrapper passes it through to its source, and in a ref
   *  without it the name matches nothing. */
  def passing[T <: AnyRef](iface: Class[T], own: T, inner: T, members: Set[String]): T =
    java.lang.reflect.Proxy.newProxyInstance(iface.getClassLoader, Array[Class[?]](iface), (_, method, args) => {
      val target = if (members(method.getName)) inner else own
      try method.invoke(target, Option(args).getOrElse(Array.empty[AnyRef])*)
      catch { case e: java.lang.reflect.InvocationTargetException => throw e.getCause }
    }).asInstanceOf[T]

  /** The worker wiring's agreement policy object (`identities`, `listedOn`), under the name the ref gives it:
   *  `IdentityProjectionWiring`, `IdentityCutoverWiring` before 2026-10-18. */
  lazy val projectionWiring: AnyRef =
    Seq("modules.wiring.IdentityProjectionWiring$", "modules.wiring.IdentityCutoverWiring$").iterator
      .flatMap(name => scala.util.Try(Class.forName(name).getField("MODULE$").get(null)).toOption).nextOption()
      .getOrElse(throw new IllegalStateException("this ref has neither IdentityProjectionWiring nor IdentityCutoverWiring"))

  /** The listing set the identity model resolves off `byCinema`: `ModelRead.corpus`, prod's lean read, where the ref has
   *  it; read whole (`Listing.corpus`) in a ref from before it (2026-10-21), as that ref's own replays read them. */
  def modelCorpus(byCinema: Iterable[(models.Cinema, Seq[models.CinemaMovie])], normalizer: services.movies.TitleNormalizer): Seq[Listing] =
    scala.util.Try(Class.forName("services.identity.ModelRead$").getField("MODULE$").get(null)).toOption
      .flatMap(read => call(read, "corpus", byCinema, normalizer)).map(_.asInstanceOf[Seq[Listing]])
      .getOrElse(Listing.corpus(byCinema, normalizer))

  /** The lookups added after the oldest ref a tool runs under, which its wrappers pass to their source ([[passing]]). */
  val NewerLookups: Set[String] = Set("overview", "numbersFilmsByTmdbId")
}
