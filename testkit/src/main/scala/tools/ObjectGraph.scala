package tools

import java.lang.reflect.Modifier
import scala.collection.mutable
import scala.util.Try

/** Walks the instance graph under a composition root by reflection and reports every
 *  value a partial function picks out, each with the field path it was first reached by.
 *
 *  What a wiring spec asks of a root is usually "which X did it end up holding" — which
 *  `Country`, which cinemas, which locale — and the answer is spread over dozens of
 *  constructor parameters nobody exposes. Walking the graph answers it for every member,
 *  including the ones added after the spec was written, which is the point: a new
 *  component constructed with a defaulted `Country` is caught without the spec naming it.
 *
 *  Only this codebase's objects and the collections they hold are entered. Library
 *  internals (JDK, Mongo driver, Pekko, Play, Prometheus, Caffeine) are opaque: nothing a
 *  wiring passes a library is interesting here, and their field graphs are large and
 *  closed to reflection. `opaque` names further roots to stop at — a value the caller
 *  knows is global by design. A matched value is recorded and not entered. */
object ObjectGraph {

  private val LibraryPrefixes = Seq(
    "java.", "javax.", "jdk.", "sun.", "com.sun.", "org.mongodb.", "com.mongodb.", "org.bson.",
    "org.apache.", "play.", "akka.", "io.prometheus.", "com.github.benmanes.", "org.slf4j.",
    "ch.qos.", "com.fasterxml.", "com.typesafe.", "io.netty.", "org.jsoup.", "scala.concurrent.",
    "scala.reflect.", "scala.runtime.", "scala.Enumeration", "okhttp3.", "kotlin.")

  private val MaxDepth = 60

  def collect[T](root: AnyRef, opaque: AnyRef => Boolean = _ => false)(pick: PartialFunction[AnyRef, T]): Seq[(String, T)] = {
    val seen  = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[AnyRef, java.lang.Boolean]())
    val found = mutable.LinkedHashMap.empty[T, String]
    // BREADTH-first, so every object is first reached — and marked seen — at its SHALLOWEST
    // depth. Depth-first reached many objects first down a long chain, near `MaxDepth`, and
    // `seen` then stopped the short path from ever expanding them: whatever sat below was
    // invisible to the walk however close to the root it really was. It also reports the
    // shortest path to each value, which is the one a reader can follow.
    val queue = mutable.Queue[(AnyRef, String, Int)]((root, root.getClass.getSimpleName, 0))

    def push(value: Any, path: String, depth: Int): Unit = value match {
      case ref: AnyRef if ref != null && depth <= MaxDepth && !seen.contains(ref) => queue.enqueue((ref, path, depth))
      case _                                                                     =>
    }

    while (queue.nonEmpty) {
      val (obj, path, depth) = queue.dequeue()
      if (seen.add(obj)) {
        if (pick.isDefinedAt(obj)) { val v = pick(obj); if (!found.contains(v)) found(v) = path }
        else if (!opaque(obj)) obj match {
          case _: String | _: java.lang.Number | _: java.lang.Boolean | _: java.lang.Character | _: Class[?] =>
          case it: scala.collection.Iterable[?] =>
            Try(it.iterator.zipWithIndex.take(200000).foreach { case (e, i) => push(e, s"$path[$i]", depth + 1) })
          case arr: Array[AnyRef] => arr.iterator.zipWithIndex.foreach { case (e, i) => push(e, s"$path[$i]", depth + 1) }
          case m: java.util.Map[?, ?] =>
            Try(m.entrySet.forEach(e => { push(e.getKey, s"$path.key", depth + 1); push(e.getValue, s"$path[${e.getKey}]", depth + 1) }))
          case c: java.util.Collection[?] => Try(c.forEach(e => push(e, s"$path[]", depth + 1)))
          case _ if !isLibrary(obj.getClass) =>
            fields(obj.getClass).foreach { f =>
              Try(f.get(obj)).foreach(v => push(v, s"$path.${f.getName}", depth + 1))
            }
          case _ =>
        }
      }
    }
    found.toSeq.map { case (value, where) => where -> value }
  }

  /** Force every `lazy val` of `obj` that has not been read yet, and name the ones that threw.
   *
   *  The walk sees fields, and an unforced lazy val's field is null: whatever it WOULD build is
   *  invisible, so a spec that only walks is as complete as the list of members it thought to
   *  touch first — the opposite of "a component added tomorrow is caught without listing it".
   *  Call this on a composition root before [[collect]]. A member that throws is returned, not
   *  swallowed: the caller decides whether its absence from the walk is acceptable. */
  def forceLazyMembers(obj: AnyRef): Seq[(String, Throwable)] =
    Iterator.iterate[Class[?]](obj.getClass)(_.getSuperclass).takeWhile(k => k != null && !isLibrary(k))
      .flatMap(_.getDeclaredFields.iterator)
      .flatMap(f => LazyField.findFirstMatchIn(f.getName).map(m => f -> m.group(1)))
      .filter { case (f, _) => Try { f.setAccessible(true); f.get(obj) == null }.getOrElse(false) }
      .flatMap { case (f, name) =>
        accessor(obj.getClass, name).fold(Option(name -> (new NoSuchMethodException(s"no accessor for lazy val $name"): Throwable))) { m =>
          Try(m.invoke(obj)).failed.toOption.map(e => name -> Option(e.getCause).getOrElse(e))
        }
      }.toSeq

  // Scala 3 backs `lazy val name` with a field `name$lzy1`.
  private val LazyField = """^(.+)\$lzy\d+$""".r

  private def accessor(c: Class[?], name: String): Option[java.lang.reflect.Method] =
    Iterator.iterate[Class[?]](c)(_.getSuperclass).takeWhile(_ != null)
      .flatMap(k => Try(k.getDeclaredMethod(name)).toOption)
      .find(m => Try(m.setAccessible(true)).isSuccess)

  private def isLibrary(c: Class[?]): Boolean = {
    val name = c.getName
    c.isPrimitive || LibraryPrefixes.exists(name.startsWith)
  }

  private val fieldCache = mutable.Map.empty[Class[?], Seq[java.lang.reflect.Field]]

  private def fields(c: Class[?]): Seq[java.lang.reflect.Field] = fieldCache.synchronized {
    fieldCache.getOrElseUpdate(c,
      Iterator.iterate[Class[?]](c)(_.getSuperclass).takeWhile(k => k != null && !isLibrary(k))
        .flatMap(_.getDeclaredFields.iterator)
        .filterNot(f => Modifier.isStatic(f.getModifiers) || f.getType.isPrimitive)
        .filter(f => Try(f.setAccessible(true)).isSuccess)
        .toSeq)
  }
}
