package kinowo.build

/**
 * THE CLASSES A COMPILED CLASS USES, read from its class file: what project/CouplingAnalysis.scala adds to Zinc's record
 * for the classes of ONE source file. Zinc records a class->class dependency only between DIFFERENT source files (its
 * compiler bridge drops a dependency whose target is defined in the source being compiled, inheritance aside), so two
 * top-level classes sharing a file would otherwise use each other for free -- and moving a collaborator into a class's
 * file would lower its fan-out without lowering its coupling.
 *
 * A class's uses: its superclass and interfaces, its generic signature, every field's and method's type and generic
 * signature, and in every method body the classes its instructions name (`new`, a cast or `instanceof`, a field read or
 * write, a call -- its owner and its descriptor --, an invokedynamic's descriptor, a class literal, a caught exception)
 * and its local variables' types. Not read: annotations, the InnerClasses / NestMembers / PermittedSubclasses
 * bookkeeping (a sealed parent naming its children is no use of them), and the compiler's sum-type mirror (a sealed
 * parent's companion `ordinal(x)` testing `x` against every child) -- a synthetic, as `copy` and `apply` are, with no
 * dependency the source wrote.
 *
 * Compiled twice, as Coupling.scala is: into the build (Scala 2.12) and into common's tests (Scala 3, CouplingSpec).
 */
object ClassReferences {

  /** A class file's own class (binary name, dotted: `services.Foo$Bar`), the classes it uses (binary names too), and its
   *  [[Shape]]: what Coupling.Collaborators reads to tell a data type from a collaborator. */
  final case class Compiled(name: String, uses: Set[String], shape: Shape = Shape.empty)

  /** A field: its name, whether it is static and final, and the classes its type names (descriptor and generic
   *  signature; binary names, dotted). */
  final case class Field(name: String, isStatic: Boolean, isFinal: Boolean, types: Seq[String])

  /** What a class file says of its class's kind and behaviour: its superclass and interfaces, whether it is an interface
   *  or abstract, its fields, the methods its bodies call (`owner#name`, the owner's binary name dotted; one taking no
   *  argument also as `owner#name()`: `Instant.now()`, not `Instant.now(clock)`) and the objects
   *  they read (the owners of every `MODULE$` read: a Scala object's module class), and its own fields a method other
   *  than a constructor or static initialiser writes (`rewritten`: a `var`'s, or a hand-rolled cache's), and each of
   *  its methods' own uses (`methods`: what ExemptionSlack reads to tell which collaborators each method touches). */
  final case class Shape(superName: String, interfaces: Seq[String], isInterface: Boolean, isAbstract: Boolean,
                         fields: Seq[Field], calls: Set[String], modules: Set[String], rewritten: Set[String] = Set.empty,
                         methods: Seq[Method] = Nil)

  /** One method: its name and descriptor, whether it is private, the classes its signature and body name (binary names,
   *  dotted) and the calls its body makes (`owner#name`, as [[Shape]]'s). A lambda's body is a method of its own
   *  (`run$$anonfun$1`). `flags`: its access flags (what project/DeadCode.scala reads a public, non-synthetic method by). */
  final case class Method(name: String, descriptor: String, isPrivate: Boolean, uses: Set[String], calls: Set[String], flags: Int = 0)
  object Shape {
    val empty: Shape = Shape("java.lang.Object", Nil, isInterface = false, isAbstract = false, Nil, Set.empty, Set.empty)
  }

  /** The mirror method a sealed parent's companion carries (`ordinal(x: Parent): Int`): read as no use. */
  private def isMirrorOrdinal(name: String, descriptor: String): Boolean =
    name == "ordinal" && descriptor.startsWith("(L") && descriptor.endsWith(")I") && descriptor.indexOf(';') == descriptor.length - 3

  def read(bytes: Array[Byte]): Compiled = {
    val in = new java.io.DataInputStream(new java.io.ByteArrayInputStream(bytes))
    if (in.readInt() != 0xCAFEBABE) throw new IllegalArgumentException("not a class file")
    in.readUnsignedShort(); in.readUnsignedShort()
    val count: Int = in.readUnsignedShort()
    val tags = new Array[Int](count)
    val first = new Array[Int](count)
    val second = new Array[Int](count)
    val utf8 = new Array[String](count)
    var i = 1
    while (i < count) {
      val tag: Int = in.readUnsignedByte()
      tags(i) = tag
      tag match {
        case 1 => utf8(i) = in.readUTF()
        case 7 | 8 | 16 | 19 | 20 => first(i) = in.readUnsignedShort()
        case 3 | 4 => in.readInt()
        case 5 | 6 => in.readLong(); i += 1
        case 9 | 10 | 11 | 12 | 17 | 18 => first(i) = in.readUnsignedShort(); second(i) = in.readUnsignedShort()
        case 15 => in.readUnsignedByte(); first(i) = in.readUnsignedShort()
        case other => throw new IllegalArgumentException(s"constant pool tag $other")
      }
      i += 1
    }
    val uses = Set.newBuilder[String]
    val calls = Set.newBuilder[String]
    val modules = Set.newBuilder[String]
    var muted = false
    var inConstructor = false
    val rewritten = Set.newBuilder[(String, String)]
    // the method being read: its uses and calls (null outside a method)
    var methodUses: scala.collection.mutable.Set[String] = null
    var methodCalls: scala.collection.mutable.Set[String] = null
    def use(name: String): Unit = { uses += name; if (methodUses != null) methodUses += name }
    def call(name: String): Unit = { calls += name; if (methodCalls != null) methodCalls += name }
    // a field write (putfield, putstatic) outside a constructor: the field's owner and name
    def written(index: Int): Unit = if (!inConstructor && index > 0 && index < count && tags(index) == 9)
      rewritten += ((className(first(index)), memberName(index)))
    def className(index: Int): String = utf8(first(index)).replace('/', '.')
    def memberName(index: Int): String = utf8(first(second(index)))
    def signature(index: Int): Unit =
      if (!muted && index > 0 && utf8(index) != null) Signatures.types(utf8(index)).foreach(use)
    def classEntry(index: Int): Unit = if (!muted && index > 0 && tags(index) == 7) {
      val name: String = utf8(first(index))
      if (name.startsWith("[")) signature(first(index)) else use(name)
    }
    // a constant an instruction names: a class, a member (its owner and its type), a method type, an invokedynamic
    def constant(index: Int): Unit = if (index > 0 && index < count) tags(index) match {
      case 7 => classEntry(index)
      case 9 | 10 | 11 =>
        if (tags(index) == 9) { if (memberName(index) == "MODULE$") modules += className(first(index)) }
        else {
          val named: String = className(first(index)) + "#" + memberName(index)
          call(named)
          if (utf8(second(second(index))).startsWith("()")) call(named + "()") // a call taking no argument, told apart
        }
        classEntry(first(index)); constant(second(index))
      case 12 => signature(second(index))
      case 16 => signature(first(index))
      case 15 => constant(first(index))
      case 17 | 18 => constant(second(index))
      case _ => ()
    }
    def skip(n: Int): Unit = in.readFully(new Array[Byte](n))
    def attributes(code: Boolean): Unit = {
      val n: Int = in.readUnsignedShort()
      (0 until n).foreach { _ =>
        val name: String = utf8(in.readUnsignedShort())
        val length: Int = in.readInt()
        name match {
          case "Signature" => signature(in.readUnsignedShort())
          case "Code" if !code => body()
          case "LocalVariableTable" | "LocalVariableTypeTable" if code =>
            val entries: Int = in.readUnsignedShort()
            (0 until entries).foreach { _ => in.readUnsignedShort(); in.readUnsignedShort(); in.readUnsignedShort(); signature(in.readUnsignedShort()); in.readUnsignedShort() }
          case _ => skip(length)
        }
      }
    }
    def body(): Unit = {
      in.readUnsignedShort(); in.readUnsignedShort()
      val code = new Array[Byte](in.readInt())
      in.readFully(code)
      instructions(code, constant, written)
      val handlers: Int = in.readUnsignedShort()
      (0 until handlers).foreach { _ => in.readUnsignedShort(); in.readUnsignedShort(); in.readUnsignedShort(); classEntry(in.readUnsignedShort()) }
      attributes(code = true)
    }
    val access: Int = in.readUnsignedShort()
    val self: Int = in.readUnsignedShort()
    val superIndex: Int = in.readUnsignedShort()
    classEntry(superIndex)
    val interfaces: Seq[String] = (0 until in.readUnsignedShort()).map { _ => val i: Int = in.readUnsignedShort(); classEntry(i); className(i) }
    val fields: Seq[Field] = (0 until in.readUnsignedShort()).map { _ =>
      val flags: Int = in.readUnsignedShort()
      val fieldName: String = utf8(in.readUnsignedShort())
      val descriptor: Int = in.readUnsignedShort()
      signature(descriptor)
      // the field's type: its descriptor, and its generic signature
      var generic: Seq[String] = Nil
      (0 until in.readUnsignedShort()).foreach { _ =>
        val attribute: String = utf8(in.readUnsignedShort())
        val length: Int = in.readInt()
        if (attribute == "Signature") { val index: Int = in.readUnsignedShort(); signature(index); generic = Signatures.types(utf8(index)) }
        else skip(length)
      }
      val types: Seq[String] = (Signatures.types(utf8(descriptor)) ++ generic).map(_.replace('/', '.')).distinct
      Field(fieldName, (flags & 0x0008) != 0, (flags & 0x0010) != 0, types)
    }
    val methods: Seq[Method] = (0 until in.readUnsignedShort()).map { _ =>
      val flags: Int = in.readUnsignedShort()
      val name: String = utf8(in.readUnsignedShort())
      inConstructor = name == "<init>" || name == "<clinit>"
      val descriptor: Int = in.readUnsignedShort()
      methodUses = scala.collection.mutable.Set.empty[String]
      methodCalls = scala.collection.mutable.Set.empty[String]
      // the mirror's signature and body are the compiler's, not the source's: read past them
      muted = isMirrorOrdinal(name, utf8(descriptor))
      signature(descriptor); attributes(code = false)
      muted = false
      val method = Method(name, utf8(descriptor), (flags & 0x0002) != 0, methodUses.toSet.map((u: String) => u.replace('/', '.')), methodCalls.toSet,
        flags)
      methodUses = null; methodCalls = null
      method
    }
    attributes(code = false)
    val name: String = className(self)
    val shape = Shape(if (superIndex > 0) className(superIndex) else "", interfaces, (access & 0x0200) != 0, (access & 0x0400) != 0,
      fields, calls.result(), modules.result() - name, rewritten.result().collect { case (owner, field) if owner == name => field }, methods)
    Compiled(name, uses.result().map(_.replace('/', '.')) - name, shape)
  }

  /** Every constant-pool index a method body's instructions name, handed to `constant`; a field write's to `written` too. */
  private def instructions(code: Array[Byte], constant: Int => Unit, written: Int => Unit): Unit = {
    def u1(at: Int): Int = code(at) & 0xff
    def u2(at: Int): Int = (u1(at) << 8) | u1(at + 1)
    def s4(at: Int): Int = (u2(at) << 16) | u2(at + 2)
    var pc = 0
    while (pc < code.length) {
      val op: Int = u1(pc)
      pc += (op match {
        case 0x12 => constant(u1(pc + 1)); 2
        case 0xb3 | 0xb5 => constant(u2(pc + 1)); written(u2(pc + 1)); 3
        case 0x13 | 0x14 | 0xb2 | 0xb4 | 0xb6 | 0xb7 | 0xb8 | 0xbb | 0xbd | 0xc0 | 0xc1 => constant(u2(pc + 1)); 3
        case 0xb9 | 0xba => constant(u2(pc + 1)); 5
        case 0xc5 => constant(u2(pc + 1)); 4
        case 0x10 | 0x15 | 0x16 | 0x17 | 0x18 | 0x19 | 0x36 | 0x37 | 0x38 | 0x39 | 0x3a | 0xa9 | 0xbc => 2
        case 0x11 | 0x84 | 0xc6 | 0xc7 => 3
        case b if b >= 0x99 && b <= 0xa8 => 3
        case 0xc8 | 0xc9 => 5
        case 0xc4 => if (u1(pc + 1) == 0x84) 6 else 4
        case 0xaa =>
          val at: Int = pc + 1 + (3 - (pc % 4))
          12 + (s4(at + 8) - s4(at + 4) + 1) * 4 + (at - pc)
        case 0xab =>
          val at: Int = pc + 1 + (3 - (pc % 4))
          8 + s4(at + 4) * 8 + (at - pc)
        case _ => 1
      })
    }
  }

  /** The classes a descriptor or a generic signature names (internal names, `a/b/C`; an inner class of a generic outer,
   *  `La/Outer<TT;>.Inner;`, as `a/Outer$Inner`). Type variables, primitives and wildcards name none. */
  object Signatures {
    def types(s: String): Seq[String] = {
      val out = Seq.newBuilder[String]
      var i = 0
      def at(c: Char): Boolean = i < s.length && s.charAt(i) == c
      def until(stops: String): String = { val from = i; while (i < s.length && stops.indexOf(s.charAt(i)) < 0) i += 1; s.substring(from, i) }
      def arguments(): Unit = {
        i += 1
        while (i < s.length && !at('>')) { if (at('+') || at('-')) i += 1; if (at('*')) i += 1 else reference() }
        i += 1
      }
      def reference(): Unit =
        if (at('L')) {
          i += 1
          var name: String = until(";<.")
          out += name
          if (at('<')) arguments()
          while (at('.')) {
            i += 1
            name = name + "$" + until(";<.")
            out += name
            if (at('<')) arguments()
          }
          i += 1
        } else if (at('T')) { until(";"); i += 1 }
        else if (at('[')) { i += 1; reference() }
        else i += 1
      if (at('<')) { // formal type parameters: `Name:ClassBound:InterfaceBound...`
        i += 1
        while (i < s.length && !at('>')) {
          until(":")
          while (at(':')) { i += 1; if (at('L') || at('T') || at('[')) reference() }
        }
        i += 1
      }
      while (i < s.length) reference()
      out.result()
    }
  }
}
