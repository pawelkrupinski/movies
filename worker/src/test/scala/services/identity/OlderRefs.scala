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

  /** An agreement stage's open questions, as the families' (family, question) pairs, the posters to hash and the catalogue
   *  questions: off its one `wanted` set of `Question`s (2026-10-24), else the separate `wanted` (the pairs),
   *  `wantedPosters` and `wantedCatalogue` an older ref keeps. A `Question` is matched by its case's name, not its type. */
  def stageWanted(stage: AnyRef): (Set[(agreement.VoterFamily, String)], Set[agreement.AgreementStage.PosterQuestion], Set[CatalogueQuestion]) = {
    val wanted = call(stage, "wanted").get.asInstanceOf[Set[Any]]
    call(stage, "wantedPosters") match {
      case Some(posters) =>
        (wanted.asInstanceOf[Set[(agreement.VoterFamily, String)]], posters.asInstanceOf[Set[agreement.AgreementStage.PosterQuestion]],
          call(stage, "wantedCatalogue").get.asInstanceOf[Set[CatalogueQuestion]])
      case None =>
        def of(kind: String): Set[Product] = wanted.collect { case q: Product if q.productPrefix == kind => q }
        (of("Family").map(q => (q.productElement(0).asInstanceOf[agreement.VoterFamily], q.productElement(1).asInstanceOf[String])),
          of("Poster").map(_.productElement(0).asInstanceOf[agreement.AgreementStage.PosterQuestion]),
          of("Catalogue").map(_.productElement(0).asInstanceOf[CatalogueQuestion]))
    }
  }

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

  /** The resolve's inputs over `pins` (its calibration, pins and title decorations) as production builds them:
   *  `ResolveInputs.of` where the ref has it (2026-10-25), the one builder the worker and the drift checks use, else as an
   *  older ref's resolve defaulted them. An input this tool does not pass on (a field of `ResolveInputs` beyond these
   *  three) is refused, never dropped: the slice would decide without it while the worker decides with it. */
  def resolveInputs(pins: Seq[Pin]): (IdentityCalibration, PinConstraints, TitleDecorations) =
    scala.util.Try(Class.forName("services.identity.ResolveInputs$").getField("MODULE$").get(null)).toOption
      .fold((IdentityCalibration.resolver, PinConstraints(pins), TitleDecorations.resolver)) { builder =>
        val inputs = call(builder, "of", pins).getOrElse(throw new IllegalStateException("ResolveInputs.of no longer takes the pins alone: " +
          "pass its new input in OlderRefs.resolveInputs")).asInstanceOf[Product]
        val unread = inputs.productElementNames.toSet -- ResolvedInputs
        require(unread.isEmpty, s"ResolveInputs holds ${unread.mkString(", ")}, which OlderRefs.resolveInputs does not pass on: pass it")
        (field(inputs, "calibration").get.asInstanceOf[IdentityCalibration], field(inputs, "pins").get.asInstanceOf[PinConstraints],
          field(inputs, "decorations").get.asInstanceOf[TitleDecorations])
      }

  /** The inputs [[resolveInputs]] passes on. */
  val ResolvedInputs: Set[String] = Set("calibration", "pins", "decorations")

  /** A TMDB client over `fetch` that never sleeps: an older ref's client retries a failure itself and takes the pause
   *  between tries as `retrySleep` (gone 2026-10-24, the wire's `RetryingHttpFetch` retries since), passed a no-op here;
   *  a newer one has no such parameter. Every other parameter at its default (a fresh `JsonBodies` where the ref has one). */
  def tmdbClient(fetch: tools.HttpFetch, key: settings.TmdbApiKey, language: java.util.Locale): clients.TmdbClient = {
    val noSleep: Long => Unit = _ => ()
    val constructor = classOf[clients.TmdbClient].getConstructors.maxBy(_.getParameterCount)
    val args = constructor.getParameterTypes.map {
      case t if t == classOf[tools.HttpFetch]        => fetch
      case t if t == classOf[Option[?]]              => Some(key)
      case t if t == classOf[java.util.Locale]       => language
      case t if t == classOf[Function1[?, ?]]        => noSleep
      case t                                         => t.getConstructor().newInstance()
    }
    constructor.newInstance(args.map(_.asInstanceOf[AnyRef])*).asInstanceOf[clients.TmdbClient]
  }

  /** The lookups added after the oldest ref a tool runs under, which its wrappers pass to their source ([[passing]]). */
  val NewerLookups: Set[String] = Set("overview", "numbersFilmsByTmdbId")
}
