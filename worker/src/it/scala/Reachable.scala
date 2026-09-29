package integration

import java.lang.reflect.{Array => Arrays, Modifier}
import java.util.IdentityHashMap

/**
 * The objects a structure itself reaches, counted by class — unlike [[LiveHeap]]'s heap-wide
 * deltas, which count whatever the parallel suites of one `itAll` JVM allocate meanwhile. Walks
 * application and Scala-library objects field by field and arrays element by element; a JDK object
 * other than an array is counted but not entered (the module system forbids it, and a structure's
 * own shape is what is being measured). `outside` are collaborators the structure only points at.
 */
object Reachable {

  def count(root: AnyRef, outside: Seq[AnyRef] = Nil): Map[String, Long] = {
    val seen   = new IdentityHashMap[AnyRef, java.lang.Boolean]()
    outside.foreach(seen.put(_, java.lang.Boolean.TRUE))
    val counts = scala.collection.mutable.HashMap.empty[String, Long]
    val stack  = scala.collection.mutable.Stack[AnyRef](root)
    while (stack.nonEmpty) {
      val next = stack.pop()
      if (next != null && seen.put(next, java.lang.Boolean.TRUE) == null) {
        val cls = next.getClass
        counts.updateWith(cls.getName)(n => Some(n.getOrElse(0L) + 1))
        if (cls.isArray) {
          if (!cls.getComponentType.isPrimitive) (0 until Arrays.getLength(next)).foreach(i => stack.push(Arrays.get(next, i)))
        } else if (entered(cls)) fieldsOf(cls).foreach(f => stack.push(f.get(next)))
      }
    }
    counts.toMap
  }

  private def entered(cls: Class[?]): Boolean = !Seq("java.", "javax.", "jdk.", "sun.").exists(cls.getName.startsWith)

  private val fields = new java.util.concurrent.ConcurrentHashMap[Class[?], Seq[java.lang.reflect.Field]]()
  private def fieldsOf(cls: Class[?]): Seq[java.lang.reflect.Field] = fields.computeIfAbsent(cls, c =>
    Iterator.iterate[Class[?]](c)(_.getSuperclass).takeWhile(k => k != null && entered(k))
      .flatMap(_.getDeclaredFields).filter(f => !Modifier.isStatic(f.getModifiers) && !f.getType.isPrimitive)
      .map { f => f.setAccessible(true); f }.toSeq)
}
