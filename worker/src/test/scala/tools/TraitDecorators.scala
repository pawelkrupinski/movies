package tools

import java.io.File
import java.lang.reflect.{InvocationHandler, Method, Modifier, ParameterizedType, Proxy, Type}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{CompletableFuture, CopyOnWriteArrayList}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters._

/**
 * Reflection over the DECORATORS of a trait — the classes that implement a trait by
 * wrapping another instance of it (retry, metering, caching, fallback …).
 *
 * The failure these exist for: a decorator overrides the member it adds behaviour to
 * and silently inherits the trait's DEFAULT for every other one, so a member added to
 * the trait is live on the real implementation and dead through the chain, with
 * nothing failing. `listingIsComplete` and `chainVenueId` shipped through the
 * cinema-scraper decorators that way; `getBytes` / header-carrying `get` through the
 * HTTP ones. A hand-written list of members in the spec has the same hole one level
 * up — a new member is missing from the list too — so these enumerate both the
 * decorators and the members by reflection.
 *
 * Scala 3 emits a mixin forwarder for every inherited default, so "does the class
 * declare the method" is always yes; only behaviour tells a forward from a default.
 */
object TraitDecorators {

  /** Every concrete, named class compiled alongside `anchor` (its classes directory
   *  or jar) that implements `trait` and takes an instance of it — directly, or inside
   *  a collection/tuple — as a constructor parameter. */
  def discover(`trait`: Class[?], anchor: Class[?]): Set[Class[?]] =
    classNamesBeside(anchor).iterator
      .filterNot(name => name.contains("$anon") || name.endsWith("$") || "\\$\\d".r.findFirstIn(name).isDefined)
      .flatMap(name => scala.util.Try(Class.forName(name, false, anchor.getClassLoader)).toOption)
      .filter(c => `trait`.isAssignableFrom(c) && !c.isInterface && !Modifier.isAbstract(c.getModifiers))
      .filter(_.getConstructors.exists(_.getGenericParameterTypes.exists(mentions(_, `trait`))))
      .toSet

  /** The trait's own members: public, non-static, compiler-synthesised helpers
   *  (`$default$` argument getters, `$init$`) left out. */
  def members(`trait`: Class[?]): Seq[Method] =
    `trait`.getMethods.toSeq
      .filterNot(m => Modifier.isStatic(m.getModifiers) || m.getName.contains("$"))
      .filter(_.getDeclaringClass == `trait`)
      .sortBy(signature)

  /** The members the trait gives a BODY — the ones a decorator inherits silently. */
  def defaulted(`trait`: Class[?]): Seq[Method] = members(`trait`).filter(_.isDefault)

  def signature(m: Method): String =
    s"${m.getName}(${m.getParameterTypes.map(_.getSimpleName).mkString(", ")})"

  /** One call a [[recording]] delegate received. */
  final case class Call(method: String, args: Seq[Any])

  /** An instance of `trait` that records every call and answers `answer(method)`. */
  def recording[T](`trait`: Class[T], answer: Method => Any): (T, java.util.List[Call]) = {
    val calls = new CopyOnWriteArrayList[Call]()
    val handler: InvocationHandler = (proxy: AnyRef, method: Method, args: Array[AnyRef]) =>
      method.getName match {
        case "hashCode" if method.getParameterCount == 0 => Int.box(System.identityHashCode(proxy))
        case "equals" if method.getParameterCount == 1   => Boolean.box(proxy eq args(0))
        case "toString" if method.getParameterCount == 0 => s"recording ${`trait`.getSimpleName}"
        case _ =>
          calls.add(Call(signature(method), Option(args).map(_.toSeq).getOrElse(Seq.empty)))
          answer(method).asInstanceOf[AnyRef]
      }
    (Proxy.newProxyInstance(`trait`.getClassLoader, Array(`trait`), handler).asInstanceOf[T], calls)
  }

  /** A plausible argument for a parameter of type `t`, or a failure naming the type so
   *  the next person to add a member with a new parameter type extends this. */
  def sampleArgument(t: Class[?]): Any = t match {
    case c if c == classOf[String]                         => "https://decorator.test/probe"
    case c if c == classOf[Map[?, ?]]                      => Map("X-Decorator-Probe" -> "1")
    case c if c == Integer.TYPE                            => 1
    case c if c == java.lang.Long.TYPE                     => 1L
    case c if c == java.lang.Boolean.TYPE                  => true
    case c if c == classOf[Function0[?]]                   => () => ()
    case other =>
      throw new AssertionError(s"TraitDecorators.sampleArgument: no sample for a ${other.getName} parameter — add one")
  }

  /** A value of `t` for a recording delegate to answer with. */
  def sampleAnswer(t: Class[?]): Any = t match {
    case c if c == Void.TYPE                        => null
    case c if c == classOf[String]                  => "delegate-body"
    // Not valid UTF-8, so a decorator that round-trips it through a String is caught.
    case c if c == classOf[Array[Byte]]             => Array[Byte](0xff.toByte, 0xfe.toByte, 0x7a)
    case c if c == classOf[CompletableFuture[?]]    => CompletableFuture.completedFuture("delegate-body")
    case c if c == classOf[Option[?]]               => Some(new AutoCloseable { def close(): Unit = () })
    case c if c == Integer.TYPE                     => 7
    case c if c == java.lang.Boolean.TYPE           => true
    case other =>
      throw new AssertionError(s"TraitDecorators.sampleAnswer: no sample for a ${other.getName} answer — add one")
  }

  private def mentions(t: Type, target: Class[?]): Boolean = t match {
    case c: Class[?]          => c == target
    case p: ParameterizedType => mentions(p.getRawType, target) || p.getActualTypeArguments.exists(mentions(_, target))
    case _                    => false
  }

  private def classNamesBeside(anchor: Class[?]): Seq[String] = {
    val location = Paths.get(anchor.getProtectionDomain.getCodeSource.getLocation.toURI)
    def toName(relative: String) = relative.stripSuffix(".class").replace('/', '.').replace(File.separatorChar, '.')
    if (Files.isDirectory(location))
      Files.walk(location).iterator.asScala.toSeq
        .filter(_.toString.endsWith(".class"))
        .map((p: Path) => toName(location.relativize(p).toString))
    else {
      val jar = new JarFile(location.toFile)
      try jar.entries.asScala.map(_.getName).filter(_.endsWith(".class")).map(toName).toSeq
      finally jar.close()
    }
  }
}
