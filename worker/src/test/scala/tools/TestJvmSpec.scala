package tools

/** Worker specs run unforked inside the sbt JVM, so this guards `.jvmopts`. */
class TestJvmSpec extends TestJvmContract
