package tools.contracts

import java.io.File
import java.lang.reflect.{Constructor, Modifier, Type}
import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * Finds every implementation of a trait on the project's own class roots, and builds each
 * one from its constructor — so a contract spec runs against an implementation the day it
 * is written, with no edit to the spec.
 *
 * Why: a fake that drifts from the real class passes every spec written against it while
 * production does something else. `InMemoryTaskQueue.amendWaiting` said "amended" where
 * Mongo said "unchanged"; `InlineResolveDispatcher` dropped a re-try the queue dispatcher
 * raised; the in-memory staging group read walked the whole backlog where Mongo read an
 * index. Each was one behaviour, in one implementation, nobody ran the other against.
 */
object Implementations {

  /** Every concrete, named class implementing `contract` in the class roots holding
   *  `anchors` (a class from each module to search — its own `classes` directory or jar),
   *  except the doubles marked [[FailsOnPurpose]]. */
  def of[T](contract: Class[T], anchors: Class[?]*): Seq[Class[? <: T]] = {
    val loader = contract.getClassLoader
    anchors.flatMap(root).distinct.flatMap(classNames).distinct.sorted.flatMap { name =>
      Try(Class.forName(name, false, loader)).toOption
    }.filter { cls =>
      contract.isAssignableFrom(cls) && cls != contract && !cls.isInterface &&
        !classOf[FailsOnPurpose].isAssignableFrom(cls) &&
        !Modifier.isAbstract(cls.getModifiers) && !cls.isAnonymousClass && !cls.isLocalClass &&
        !cls.isSynthetic && !cls.getName.contains("$anon")
    }.map(_.asInstanceOf[Class[? <: T]])
  }

  /**
   * Build `cls` from its widest public constructor. Each parameter is taken from `supply`
   * (keyed by the parameter's generic type name, e.g. `scala.Option<org.mongodb.scala.MongoDatabase>`,
   * then its raw class name), else from the parameter's Scala default. Left when a
   * parameter has neither — naming it, so the spec can supply it.
   */
  def construct[T](cls: Class[T], supply: Type => Option[AnyRef]): Either[String, T] = {
    val constructors = cls.getConstructors.toSeq.sortBy(-_.getParameterCount)
    constructors.headOption.toRight(s"${cls.getName} has no public constructor").flatMap { ctor =>
      val types = ctor.getGenericParameterTypes.toSeq
      val args  = types.zipWithIndex.map { case (t, i) => supply(t).orElse(default(cls, i)).toRight(t.getTypeName) }
      val missing = args.collect { case Left(name) => name }
      if (missing.nonEmpty) Left(s"${cls.getName}: no value for constructor parameter(s) ${missing.mkString(", ")} — supply them")
      else Right(ctor.asInstanceOf[Constructor[T]].newInstance(args.collect { case Right(v) => v }*))
    }
  }

  /** The value of Scala's default for constructor parameter `index` (0-based), off the
   *  companion's `$lessinit$greater$default$N`. */
  private def default(cls: Class[?], index: Int): Option[AnyRef] =
    Try {
      val companion = Class.forName(cls.getName + "$", true, cls.getClassLoader)
      val module    = companion.getField("MODULE$").get(null)
      companion.getMethod(s"$$lessinit$$greater$$default$$${index + 1}").invoke(module)
    }.toOption

  private def root(anchor: Class[?]): Option[Path] =
    Option(anchor.getProtectionDomain.getCodeSource).map(source => Paths.get(source.getLocation.toURI))

  private def classNames(root: Path): Seq[String] =
    if (Files.isDirectory(root))
      Files.walk(root).iterator().asScala.map(root.relativize(_).toString)
        .filter(_.endsWith(".class")).map(toName).toSeq
    else if (root.toString.endsWith(".jar")) {
      val jar = new JarFile(root.toFile)
      try jar.entries().asScala.map(_.getName).filter(_.endsWith(".class")).map(toName).toSeq
      finally jar.close()
    } else Nil

  private def toName(path: String): String = path.stripSuffix(".class").replace(File.separatorChar, '.').replace('/', '.')
}
