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
}
