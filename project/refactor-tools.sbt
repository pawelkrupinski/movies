// THE ON-DEMAND REFACTORING TOOLS' PLUGINS, in a file of their own: scripts/touched-tree.sh lays this file over an older
// commit's build to measure a range there. None of them instruments, mutates or rewrites the normal build -- each is
// off until a tool's own sbt command turns it on, in a throwaway worktree (or, for scalafix, on the sources it is asked to
// rewrite):
//   sbt-scoverage  scripts/coverage-touched     (`coverageEnabled` false unless set)
//   sbt-stryker4s  scripts/mutation-check       (`stryker` only when run)
//   sbt-scalafix   scripts/scalafix-organize, scripts/scalafix-move (SemanticDB only when a tool sets it)
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
addSbtPlugin("io.stryker-mutator" % "sbt-stryker4s" % "1.1.1")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.10")
