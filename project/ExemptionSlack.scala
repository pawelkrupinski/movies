package kinowo.build

/**
 * THE SLACK IN AN EXEMPTION (scripts/coupling-report --exemption-slack, `sbt couplingExemptionSlack`; the weekly
 * coupling-exemptions.yml report): for a class exempt from the fan-out limit by a `class` row, which of the
 * collaborators it counts it could give up -- read from its class files' methods (ClassReferences.Method: the types
 * each method names), each collaborator mapped as the held fan-out maps it ([[Coupling.heldTargets]]: a supertype of
 * another target is that target, a contract's member the contract). A method is a method of the source: a lambda's
 * body counts as the method holding it, a nested or anonymous class as one method of its own.
 *
 *  - PASS-THROUGH: a constructor parameter only forwarded -- the constructor names it and calls none of its methods, no
 *    field holds it and no method uses it: it is handed into a constructor or factory inside;
 *  - BUILDS ANOTHER: used only while the class is built (its constructor or its body's initialisers), held by no field
 *    and used by no method after: it only builds another value the class keeps;
 *  - CO-USED: two or more collaborators used by exactly the same methods -- every method using either uses both: one
 *    collaborator holding them would do;
 *  - REACHED THROUGH: a collaborator used only by methods that also use another one, which hands it out (a method of
 *    that one's returns it): the class reaches through that one, which could answer for it instead;
 *  - ONLY BESIDE (a hint, not cut): used only by methods that also use another one which does not hand it out.
 *
 * The suggested cut takes each pass-through and builder out (the value built injected instead: a type the class
 * already counts), holds each co-used group in one collaborator and asks each reached-through collaborator's provider;
 * the estimate is the targets left (the union rule: what remains, each collaborator once).
 *
 * Compiled twice, as Coupling.scala is: into the build (Scala 2.12) and into common's tests (Scala 3, CouplingSpec).
 */
object ExemptionSlack {

  /** One collaborator of the class: the methods using it (the constructor's own uses apart), whether the constructor
   *  takes it, calls it or names it at all, and whether a field holds it. */
  final case class Use(target: String, methods: Set[String], parameter: Boolean, calledWhileBuilt: Boolean, builtWith: Boolean, held: Boolean) {
    def passThrough: Boolean = methods.isEmpty && parameter && !calledWhileBuilt && !held
    def buildsAnother: Boolean = methods.isEmpty && !passThrough && builtWith && !held
  }

  /** A class's slack: its held fan-out, each collaborator's use, the co-used groups, the reached-through and only-beside
   *  pairs (the collaborator, the one whose methods cover its), and the estimated fan-out after the suggested cut. */
  final case class Slack(cls: String, fanOut: Int, uses: Seq[Use], coUsed: Seq[Seq[String]], through: Seq[(String, String)], beside: Seq[(String, String)]) {
    def after: Int =
      fanOut - (uses.filter(u => u.passThrough || u.buildsAnother).map(_.target).toSet ++ coUsed.flatMap(_.tail) ++ through.map(_._1)).size
  }

  private val Constructors = Set("<init>", "<clinit>", "$init$")

  /** The method of the source a compiled method of `cls`'s own class files is: a lambda's body (`run$$anonfun$1`,
   *  `$anonfun$run$1`) its enclosing method, a default argument (`run$default$2`) or a lazy val's initialiser its own
   *  method, a private method a lambda or the companion reaches (`services$X$$run`) itself, the constructor's default
   *  arguments (`$lessinit$greater$default$1`) the constructor; a lambda the name does not place (`$anonfun$1`) is
   *  `a lambda`, a method like any other: never read as the constructor's, which would call what it uses slack. */
  def enclosing(cls: String, method: String): String = {
    val mangled = cls.replace('.', '$') + "$$"
    val m = if (method.startsWith(mangled)) method.stripPrefix(mangled) else method
    val cut = Seq("$$anonfun", "$default$", "$lzyINIT", "$adapted").map(m.indexOf(_)).filter(_ > 0)
    if (m.startsWith("$lessinit$greater")) "<init>"
    else if (m.startsWith("$anonfun$")) {
      val rest = m.stripPrefix("$anonfun$").stripPrefix("adapted$").takeWhile(_ != '$')
      if (rest.isEmpty || rest.forall(_.isDigit)) "a lambda" else rest
    }
    else if (cut.nonEmpty) m.substring(0, cut.min)
    else m
  }

  /** `cls`'s slack over its held targets (`targets`), from its class files (`compiled`: the class, its object, the
   *  classes nested in it and its anonymous ones), each binary name they name mapped to a target by `target`;
   *  `handsOut(provider, collaborator)`: whether a method of the provider returns the collaborator. */
  def of(cls: String, targets: Set[String], compiled: Seq[ClassReferences.Compiled], target: String => Option[String],
         handsOut: (String, String) => Boolean = (_, _) => false): Slack = {
    def own(c: ClassReferences.Compiled): Boolean = c.name == cls || c.name == cls + "$"
    val accessors = compiled.filter(own).flatMap(_.shape.fields).map(_.name).toSet
    // (method of the source, compiled method): a nested or anonymous class is one method, named after it
    val methods = compiled.flatMap { c =>
      c.shape.methods.filterNot(m => own(c) && m.descriptor.startsWith("()") && accessors(m.name))
        .map(m => (if (own(c)) enclosing(cls, m.name) else c.name.stripPrefix(cls), m))
    }
    def targetsOf(names: Iterable[String]): Set[String] = names.flatMap(n => target(n).toSeq).toSet
    val building = methods.filter { case (n, _) => Constructors(n) }.map(_._2)
    val constructors = compiled.filter(_.name == cls).flatMap(_.shape.methods.filter(_.name == "<init>"))
    val parameters = targetsOf(constructors.flatMap(m => ClassReferences.Signatures.types(m.descriptor).map(_.replace('/', '.'))))
    val held = targetsOf(compiled.filter(own).flatMap(_.shape.fields).flatMap(_.types))
    val usedBy: Map[String, Set[String]] = methods.filterNot { case (n, _) => Constructors(n) }
      .flatMap { case (n, m) => targetsOf(m.uses).map(_ -> n) }.groupBy(_._1).map { case (t, ns) => t -> ns.map(_._2).toSet }
    val uses = targets.toSeq.sorted.map { t =>
      Use(t, usedBy.getOrElse(t, Set.empty[String]), parameters(t), building.exists(m => targetsOf(m.calls.map(_.takeWhile(_ != '#')))(t)),
        building.exists(m => targetsOf(m.uses)(t)), held(t))
    }
    val used = uses.filter(_.methods.nonEmpty)
    val coUsed = used.groupBy(_.methods).values.map(_.map(_.target).sorted).filter(_.size > 1).toSeq.sortBy(_.head)
    val grouped = coUsed.flatMap(_.tail).toSet
    // each collaborator whose methods another's strictly cover: that one hands it out (reached through), or does not
    val covered = used.filterNot(u => grouped(u.target)).flatMap { u =>
      val covers = used.filter(o => o.target != u.target && !grouped(o.target) && u.methods.subsetOf(o.methods) && u.methods != o.methods)
        .sortBy(o => (o.methods.size, o.target))
      covers.find(o => handsOut(o.target, u.target)).map(o => (u.target, o.target, true))
        .orElse(covers.headOption.map(o => (u.target, o.target, false)))
    }.sortBy(_._1)
    Slack(cls, targets.size, uses, coUsed, covered.collect { case (a, b, true) => (a, b) }, covered.collect { case (a, b, false) => (a, b) })
  }

  /** Whether a method of `provider`'s class files (`compiled`: its class and object) other than a constructor or a
   *  private one returns `collaborator`'s type (`target`: a binary name's top-level class). */
  def handsOut(compiled: Seq[ClassReferences.Compiled], target: String => Option[String], collaborator: String): Boolean =
    compiled.exists(_.shape.methods.exists { m =>
      !m.isPrivate && !Constructors(m.name) && !m.name.contains("$anonfun") &&
        ClassReferences.Signatures.types(m.descriptor.substring(m.descriptor.indexOf(')') + 1)).exists(t => target(t.replace('/', '.')).contains(collaborator))
    })

  /** The held target a binary name a class file of `cls` names counts as, as [[Coupling.heldTargets]] counts it: its
   *  top-level class (`topOf`) when that is a target, the contract it is a member of, or the target it is a supertype of;
   *  None for `cls` itself and anything it does not count. */
  def target(cls: String, targets: Set[String], g: Coupling.Graph, e: Coupling.Exemptions, topOf: String => Option[String])(binary: String): Option[String] =
    topOf(binary).filter(_ != cls).flatMap { top =>
      if (targets(top)) Some(top)
      else e.contractsOf(cls).find(_._2(top)).map("contract:" + _._1).filter(targets)
        .orElse(targets.toSeq.sorted.find(t => g.supertypes.getOrElse(t, Set.empty[String])(top)))
    }

  private def short(name: String): String = name.substring(name.lastIndexOf('.') + 1)

  /** The markdown report of every class's slack; `why`: each collaborator's verdict (collaborators.tsv's). */
  def report(slacks: Seq[Slack], why: String => String, limit: Int = Coupling.DefaultFanOutLimit): String = {
    val out = Seq.newBuilder[String]
    val stale = slacks.filter(_.after <= limit)
    out += "# Exemption slack"
    out += ""
    out += s"Every `class` exemption row of test/resources/coupling-exemptions.tsv: its collaborators, which of them it could give " +
      s"up, and its estimated fan-out after that cut. ${stale.size} of ${slacks.size} would come within the limit ($limit)" +
      (if (stale.isEmpty) "." else ": " + stale.map(s => s"`${short(s.cls)}` ${s.fanOut} -> ${s.after}").mkString(", ") + ".")
    slacks.sortBy(s => (s.after - s.fanOut, s.cls)).foreach { s =>
      def named(ts: Seq[String]): String = ts.map(t => s"`${short(t)}`").mkString(", ")
      out += ""
      out += s"## `${s.cls}`: ${s.fanOut} -> ${s.after}${if (s.after <= limit) " (within the limit: the exemption may be stale)" else ""}"
      out += ""
      s.uses.foreach { u =>
        val flags = Seq(
          if (u.passThrough) Some("**pass-through**: only forwarded into a constructor or factory") else None,
          if (u.buildsAnother) Some("**builds another**: used only while the class is built") else None,
          s.coUsed.find(_.contains(u.target)).map(g => s"**co-used** with ${named(g.filter(_ != u.target))}"),
          s.through.find(_._1 == u.target).map(b => s"**reached through** `${short(b._2)}`, which hands it out"),
          s.beside.find(_._1 == u.target).map(b => s"only beside `${short(b._2)}`")).flatten
        val where = if (u.methods.isEmpty) "no method" else u.methods.toSeq.sorted.mkString(", ")
        out += s"- `${u.target}` (${why(u.target)}): $where${if (flags.isEmpty) "" else " -- " + flags.mkString("; ")}"
      }
      val cuts = s.uses.filter(_.passThrough).map(u => s"take what `${short(u.target)}` is forwarded into, not `${short(u.target)}`") ++
        s.uses.filter(_.buildsAnother).map(u => s"take what `${short(u.target)}` builds, not `${short(u.target)}`") ++
        s.coUsed.map(g => s"one collaborator holding ${named(g)} (-${g.size - 1})") ++
        s.through.map { case (a, b) => s"ask `${short(b)}` for what `${short(a)}` answers" }
      out += ""
      out += (if (cuts.isEmpty) "Suggested cut: none found -- each collaborator is used on its own." else s"Suggested cut (${s.fanOut} -> ${s.after}): " + cuts.mkString("; ") + ".")
    }
    out.result().mkString("\n") + "\n"
  }
}
