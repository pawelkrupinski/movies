package tools

import scala.quoted.*

/**
 * Calls a constructor with its arguments matched by TYPE rather than by position, at compile time.
 *
 * Written for Play's generated `router.Routes`, whose constructor takes one parameter per controller in the order the
 * controllers first appear in the routes file: moving a route reorders the constructor, and every positional call
 * (the composition root, the route protection matrix) broke with it. Here each argument goes to the one parameter its
 * static type conforms to, so the routes file can be reordered freely.
 *
 * The constructor used is the shortest one that takes every argument. Each argument must fit exactly one of its
 * parameters, and no argument may go unused; anything else is a compile error that names the types involved.
 */
object ConstructByType {

  /** `new R(...)` with every parameter supplied by the one argument of its type. */
  inline def apply[R](inline arguments: Any*): R = ${ construct[R]('arguments, partial = false) }

  /** As [[apply]], with `null` for every parameter no argument fits: for a spec that exercises only part of `R`. */
  inline def partially[R](inline arguments: Any*): R = ${ construct[R]('arguments, partial = true) }

  private def construct[R: Type](arguments: Expr[Seq[Any]], partial: Boolean)(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val target = TypeRepr.of[R]
    val supplied = arguments match {
      case Varargs(exprs) => exprs.map(_.asTerm)
      case _              => report.errorAndAbort("ConstructByType takes its arguments listed out, not as a sequence")
    }
    val constructors = target.classSymbol.toList.flatMap(_.declarations).filter(s => s.isClassConstructor && !s.flags.is(Flags.Private))

    // `Right(arguments in parameter order)` or `Left(why this constructor does not fit)`.
    def fit(constructor: Symbol): Either[String, List[Term]] = {
      val parameterTypes = target.memberType(constructor) match {
        case method: MethodType => method.paramTypes
        case _                  => Nil
      }
      val candidates = parameterTypes.map(p => p -> supplied.filter(_.tpe.widen <:< p))
      val ambiguous  = candidates.collect { case (p, cs) if cs.size > 1 => s"${p.show} (${cs.map(_.tpe.widen.show).mkString(", ")})" }
      val missing    = candidates.collect { case (p, cs) if cs.isEmpty => p.show }
      val used       = candidates.flatMap(_._2).toSet
      val unused     = supplied.filterNot(used).map(_.tpe.widen.show)
      if (ambiguous.nonEmpty) Left(s"more than one argument fits ${ambiguous.mkString("; ")}")
      else if (unused.nonEmpty) Left(s"no parameter takes ${unused.mkString(", ")}")
      else if (missing.nonEmpty && !partial) Left(s"no argument of type ${missing.mkString(", ")}")
      else Right(candidates.map { case (_, cs) => cs.headOption.getOrElse(Literal(NullConstant())) })
    }

    val fits = constructors.map(c => c -> fit(c)).sortBy { case (c, _) => c.paramSymss.flatten.size }
    fits.collectFirst { case (constructor, Right(arguments)) => (constructor, arguments) } match {
      case Some((constructor, arguments)) =>
        New(TypeTree.of[R]).select(constructor).appliedToArgs(arguments).asExprOf[R]
      case None =>
        val reasons = fits.collect { case (c, Left(why)) => s"  (${c.paramSymss.flatten.size} parameters) $why" }
        report.errorAndAbort(s"No constructor of ${target.show} fits these arguments:\n${reasons.mkString("\n")}")
    }
  }
}
