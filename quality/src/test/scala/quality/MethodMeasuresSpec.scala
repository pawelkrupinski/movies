package quality

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The measures against SonarSource's published scores ("Cognitive Complexity", G. Ann Campbell, 2017: the examples of
 *  section 3 and Appendix C, ported to Scala; where Scala has no form of a Java construct the port says what stands in) -- so the complexity ratchet counts what Sonar counts. */
class MethodMeasuresSpec extends AnyFlatSpec with Matchers {

  private def method(source: String, name: String): MethodMeasures.Method =
    MethodMeasures.of("Example.scala", source).fold(fail(_), _.find(_.name == name).getOrElse(fail(s"no method $name")))

  "a match" should "score as Sonar's switch: one structural increment, one cyclomatic path per case (getWords: 1 and 4)" in {
    val getWords = method(
      """object Example {
        |  def getWords(number: Int): String = number match {
        |    case 1 => "one"
        |    case 2 => "a couple"
        |    case 3 => "a few"
        |    case _ => "lots"
        |  }
        |}""".stripMargin, "getWords")
    getWords.cognitive shouldBe 1
    getWords.cyclomatic shouldBe 4
  }

  "nested loops and an if" should "add their nesting (sumOfPrimes, 7 in Java: Scala has no labelled continue, so a flag and an if at nesting 1 stand for it)" in {
    val sumOfPrimes = method(
      """object Example {
        |  def sumOfPrimes(max: Int): Int = {
        |    var total = 0
        |    for (i <- 1 to max) {          // +1
        |      var prime = true
        |      for (j <- 2 until i) {       // +2 (nesting = 1)
        |        if (i % j == 0) {          // +3 (nesting = 2)
        |          prime = false
        |        }
        |      }
        |      if (prime) total += i        // +2 (nesting = 1)
        |    }
        |    total
        |  }
        |}""".stripMargin, "sumOfPrimes")
    sumOfPrimes.cognitive shouldBe 8
    sumOfPrimes.cyclomatic shouldBe 5
  }

  "try, catch and loops" should "score 9 (Sonar's myMethod: the try adds nothing, the catch +1, the loops their nesting)" in {
    val myMethod = method(
      """object Example {
        |  def myMethod(condition1: Boolean, condition2: Boolean): Unit = {
        |    try {
        |      if (condition1) {                      // +1
        |        for (i <- 0 until 10) {              // +2 (nesting = 1)
        |          while (condition2) { println(i) }  // +3 (nesting = 2)
        |        }
        |      }
        |    } catch {                                // +1
        |      case e: (IllegalStateException | IllegalArgumentException) =>
        |        if (condition2) println(e)           // +2 (nesting = 1)
        |    }
        |  }
        |}""".stripMargin, "myMethod")
    myMethod.cognitive shouldBe 9
  }

  "a lambda" should "add nesting but no increment (Sonar's myMethod2: 2)" in {
    val myMethod2 = method(
      """object Example {
        |  def myMethod2(condition1: Boolean): Runnable = {
        |    val r: Runnable = () => {
        |      if (condition1) println("x")   // +2 (nesting = 1)
        |    }
        |    r
        |  }
        |}""".stripMargin, "myMethod2")
    myMethod2.cognitive shouldBe 2
  }

  "boolean operators" should "add one per sequence of like operators (Sonar's section 3: a && b && c || d || e && f is 3, with its if 4)" in {
    val sequences = method(
      """object Example {
        |  def sequences(a: Boolean, b: Boolean, c: Boolean, d: Boolean, e: Boolean, f: Boolean): Int =
        |    if (a && b && c || d || e && f) 1 else 0
        |}""".stripMargin, "sequences")
    sequences.cognitive shouldBe 5 // the if 1, its else 1, the three sequences 3
    sequences.cyclomatic shouldBe 7 // 1, the if, five operators
    method("object Example { def one(a: Boolean, b: Boolean, c: Boolean) = a && b && c }", "one").cognitive shouldBe 1
    method("object Example { def negated(a: Boolean, b: Boolean, c: Boolean) = a && !(b && c) }", "negated").cognitive shouldBe 2
  }

  "an else-if chain" should "add one per else if and else, with no nesting (Appendix C's toRegexp: 20)" in {
    val toRegexp = method(
      """object Example {
        |  private val SpecialChars = "()[]^$.{}+|"
        |  private def isSlash(ch: Char) = ch == '/' || ch == '\\'
        |  def toRegexp(antPattern: String, directorySeparator: String): String = {
        |    val escapedDirectorySeparator = "\\" + directorySeparator
        |    val sb = new StringBuilder(antPattern.length)
        |    sb.append('^')
        |    var i = if (antPattern.startsWith("/") ||                  // +1 (the ternary), +1
        |      antPattern.startsWith("\\")) 1 else 0                   // (the ternary's else: see below)
        |    while (i < antPattern.length) {                           // +1
        |      val ch = antPattern.charAt(i)
        |      if (SpecialChars.indexOf(ch) != -1) {                    // +2 (nesting = 1)
        |        sb.append('\\').append(ch)
        |      } else if (ch == '*') {                                  // +1
        |        if (i + 1 < antPattern.length                         // +3 (nesting = 2)
        |          && antPattern.charAt(i + 1) == '*') {               // +1
        |          if (i + 2 < antPattern.length                       // +4 (nesting = 3)
        |            && isSlash(antPattern.charAt(i + 2))) {           // +1
        |            sb.append("(?:.*").append(escapedDirectorySeparator).append("|)")
        |            i += 2
        |          } else {                                            // +1
        |            sb.append(".*")
        |            i += 1
        |          }
        |        } else {                                              // +1
        |          sb.append("[^").append(escapedDirectorySeparator).append("]*?")
        |        }
        |      } else if (ch == '?') {                                  // +1
        |        sb.append("[^").append(escapedDirectorySeparator).append("]")
        |      } else if (isSlash(ch)) {                                // +1
        |        sb.append(escapedDirectorySeparator)
        |      } else {                                                // +1
        |        sb.append(ch)
        |      }
        |      i += 1
        |    }
        |    sb.append('$')
        |    sb.toString
        |  }
        |}""".stripMargin, "toRegexp")
    // Sonar's 20 counts Java's `? :` as one increment; Scala's `if ... else` is an if and an else: one more
    toRegexp.cognitive shouldBe 21
  }

  "an if without an else" should "add no else increment" in {
    method("object Example { def f(a: Boolean): Unit = if (a) println(1) }", "f").cognitive shouldBe 1
    method("object Example { def f(a: Boolean): Unit = if (a) { if (!a) println(1) else println(2) } }", "f").cognitive shouldBe 4
  }

  "a nested def" should "fold into its method, add nesting, and count as an inner def; a class's methods inside do not" in {
    val outer = method(
      """object Example {
        |  def outer(xs: Seq[Int]): Int = {
        |    def inner(x: Int): Int = if (x > 0) x else 0     // +2 (nesting = 1), else +1
        |    def other(x: Int): Int = x match { case 0 => 1; case _ => 2 } // +2 (nesting = 1)
        |    val anon = new Runnable { def run(): Unit = () }
        |    anon.run()
        |    xs.map(inner).map(other).sum
        |  }
        |}""".stripMargin, "outer")
    outer.cognitive shouldBe 5
    outer.innerDefs shouldBe 2
    MethodMeasures.of("Example.scala", "object Example { def outer = { def inner = 1; inner } }").toOption.get.map(_.name) shouldBe Seq("outer")
  }

  "a method" should "be named by its owners and span its lines" in {
    val found = MethodMeasures.of("Example.scala",
      """package a.b
        |class Outer {
        |  object Inner {
        |    def f: Int =
        |      1
        |  }
        |}""".stripMargin).toOption.get
    found.map(m => (m.qualifiedName, m.startLine, m.lines)) shouldBe Seq(("Outer.Inner.f", 4, 2))
  }

  "a val, lazy val, var or given alias with a body" should "be measured as a method, its lambda's branches counted (a lambda-valued val went unmeasured)" in {
    val found = MethodMeasures.of("Example.scala",
      """object Example {
        |  val classify: Int => String = n =>
        |    if (n > 0) "positive"
        |    else if (n < 0) "negative"
        |    else "zero"
        |  lazy val cached: Int = if (sys.props.contains("x")) 1 else 2
        |  var counter: Int = 0
        |  val (left, right) = if (counter > 0) (1, 2) else (2, 1)
        |  given Ordering[String] = (a, b) => if (a == b) 0 else 1
        |  given byLength: Ordering[Int] = Ordering.by(identity)
        |}""".stripMargin).toOption.get
    found.map(m => (m.qualifiedName, m.cognitive, m.cyclomatic, m.startLine, m.lines)) shouldBe Seq(
      ("Example.classify", 4, 3, 2, 4),
      ("Example.cached", 2, 2, 6, 1),
      ("Example.counter", 0, 1, 7, 1),
      ("Example.left,right", 2, 2, 8, 1),
      ("Example.given", 3, 2, 9, 1),
      ("Example.byLength", 0, 1, 10, 1))
  }

  "a val inside a method" should "fold into it; a member val's def counts as its inner def" in {
    val found = MethodMeasures.of("Example.scala",
      """object Example {
        |  def outer(x: Int): Int = {
        |    val f: Int => Int = y => if (y > x) y else x
        |    f(x)
        |  }
        |  val handler: Int => Int = { n =>
        |    def helper(m: Int): Int = if (m > 0) m else 0
        |    helper(n)
        |  }
        |}""".stripMargin).toOption.get
    found.map(m => (m.qualifiedName, m.cognitive, m.innerDefs)) shouldBe Seq(("Example.outer", 3, 0), ("Example.handler", 4, 1))
  }

  "an unparseable source" should "say where" in {
    MethodMeasures.of("Broken.scala", "object { def").left.toOption.get should startWith("Broken.scala:1:")
  }
}
