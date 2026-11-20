// THE MOVE RULE OF scripts/scalafix-move: a top-level class, trait or object moved to another package (and, optionally,
// renamed), every reference rewritten from the compiler's own record of what each name means (SemanticDB) -- never by
// matching text. Loaded from source by scalafix (`--rules=file:scripts/scalafix-rules/MoveSymbol.scala`), compiled by
// scalafix's own Scala 2.13; configured `--settings.MoveSymbol.from=a.b.C --settings.MoveSymbol.to=x.y.D`.
//
//  - an import of the old name (`import a.b.C`, `import a.b.{C, E}`, `import a.b.{C => K}`) is dropped from its importer
//    and the new one added (`import x.y.D`, `import x.y.{D => K}`); an import of the old name's members (`import a.b.C.*`,
//    `import a.b.C.apply`) is re-pointed (`import x.y.D.*`);
//  - a qualified reference (`a.b.C(1)`, `_root_.a.b.C`, a type `a.b.C`) is rewritten whole (`x.y.D`);
//  - an unqualified reference the old package's own sources made without an import gains `import x.y.D` (and is renamed
//    when the name changes);
//  - the moved source's `package a.b` clause becomes `package x.y`, and every name of the OLD package it used without an
//    import gains `import a.b.<Name>`.
// What it leaves -- a wildcard `import a.b.*` that also brought C in, a reference only a string or a comment holds -- the
// compile that scripts/scalafix-move runs afterwards names; nothing here guesses at text.
package fix

import metaconfig.{ConfDecoder, Configured}
import metaconfig.generic.Surface
import scalafix.v1._

import scala.meta._

final case class MoveSymbolConfig(from: String = "", to: String = "")

object MoveSymbolConfig {
  val default: MoveSymbolConfig = MoveSymbolConfig()
  implicit val surface: Surface[MoveSymbolConfig] = metaconfig.generic.deriveSurface[MoveSymbolConfig]
  implicit val decoder: ConfDecoder[MoveSymbolConfig] = metaconfig.generic.deriveDecoder(default)
}

class MoveSymbol(config: MoveSymbolConfig) extends SemanticRule("MoveSymbol") {
  def this() = this(MoveSymbolConfig.default)

  override def withConfiguration(c: Configuration): Configured[Rule] =
    c.conf.getOrElse("MoveSymbol")(this.config).map(new MoveSymbol(_))

  private def split(fqn: String): (String, String) = {
    val i = fqn.lastIndexOf('.')
    (fqn.substring(0, i), fqn.substring(i + 1))
  }

  private def importOf(pkg: String, importee: String): Importer =
    s"import $pkg.$importee".parse[Stat].get.asInstanceOf[Import].importers.head

  private def path(t: Tree): String = t.syntax.stripPrefix("_root_.")

  override def fix(implicit doc: SemanticDocument): Patch = {
    if (config.from.isEmpty || config.to.isEmpty) return Patch.empty
    val (fromPkg, fromName) = split(config.from)
    val (toPkg, toName)     = split(config.to)
    val fromSymbol = fromPkg.replace('.', '/') + "/" + fromName
    val oldPackage = fromPkg.replace('.', '/') + "/"
    def isFrom(s: Symbol): Boolean = s.value == fromSymbol + "#" || s.value == fromSymbol + "."
    def renamed(n: Name): Patch = if (toName == fromName) Patch.empty else Patch.replaceTree(n, toName)

    val packages = doc.tree.collect { case p: Pkg => p }
    val inOldPackage = packages.exists(p => path(p.ref) == fromPkg)
    val definesIt = doc.tree.collect {
      case d: Defn.Class if isFrom(d.symbol)  => d
      case d: Defn.Trait if isFrom(d.symbol)  => d
      case d: Defn.Object if isFrom(d.symbol) => d
    }.nonEmpty

    val importers = doc.tree.collect { case i: Importer => i }
    val inImporter = importers.flatMap(i => i.ref.pos.start to i.ref.pos.end).toSet
    val importPatches = importers.map { i =>
      val ref = path(i.ref)
      if (ref == fromPkg) i.importees.collect {
        case n @ Importee.Name(Name(`fromName`))        => Patch.removeImportee(n) + Patch.addGlobalImport(importOf(toPkg, toName))
        case r @ Importee.Rename(Name(`fromName`), to)  => Patch.removeImportee(r) + Patch.addGlobalImport(importOf(toPkg, s"{$toName => ${to.value}}"))
      }.asPatch
      else if (ref == config.from || ref.startsWith(config.from + ".")) Patch.replaceTree(i.ref, config.to + ref.stripPrefix(config.from))
      else Patch.empty
    }.asPatch

    // a qualified reference: the outermost select whose name means the old symbol, its qualifier a package path
    val qualified = doc.tree.collect {
      case t @ Term.Select(_, n) if isFrom(n.symbol) && !inImporter(t.pos.start) => Patch.replaceTree(t, config.to)
      case t @ Type.Select(_, n) if isFrom(n.symbol) && !inImporter(t.pos.start) => Patch.replaceTree(t, config.to)
    }.asPatch
    val selected = doc.tree.collect { case Term.Select(_, n) => n; case Type.Select(_, n) => n }.toSet

    // an unqualified reference, in a source of the old package that needed no import -- or the definition itself, renamed
    val bare = doc.tree.collect {
      case n: Term.Name if isFrom(n.symbol) && !selected(n) && !inImporter(n.pos.start) => n
      case n: Type.Name if isFrom(n.symbol) && !selected(n) && !inImporter(n.pos.start) => n
    }
    val bareImported = importers.exists(i => path(i.ref) == fromPkg && i.importees.exists {
      case Importee.Name(Name(`fromName`)) | Importee.Rename(Name(`fromName`), _) | Importee.Wildcard() => true
      case _ => false
    })
    val barePatches = bare.map(renamed).asPatch +
      (if (bare.nonEmpty && inOldPackage && !definesIt && !bareImported) Patch.addGlobalImport(importOf(toPkg, toName)) else Patch.empty)

    // the moved source: its package clause, and an import for each name of the old package it used bare
    val movedPatches =
      if (!definesIt) Patch.empty
      else {
        val clause = packages.filter(p => path(p.ref) == fromPkg).map(p => Patch.replaceTree(p.ref, toPkg)).asPatch
        val siblings = doc.tree.collect {
          case n: Term.Name if !selected(n) && !inImporter(n.pos.start) && n.symbol.owner.value == oldPackage && !isFrom(n.symbol) => n.value
          case n: Type.Name if !selected(n) && !inImporter(n.pos.start) && n.symbol.owner.value == oldPackage && !isFrom(n.symbol) => n.value
        }.distinct.filter(name => !doc.tree.collect { case d: Defn => d }.exists(_.symbol.displayName == name))
        clause + siblings.map(name => Patch.addGlobalImport(importOf(fromPkg, name))).asPatch
      }
    importPatches + qualified + barePatches + movedPatches
  }
}
