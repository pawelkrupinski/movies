package tools

/** How many writes a store took since a starting point, as a fixpoint check reads them: what the convergence storage
 *  hands its passes (`ConvergenceStorage.corpusWrites`). Mongo's is itkit's `OplogWrites`, counted off the replica
 *  set's oplog; an in-memory store has none (its change streams count exactly). An interface of its own so the unit
 *  specs' in-memory storages implement the storage without itkit on their classpath. */
trait CorpusWrites extends AutoCloseable {

  /** Writes since this counter was created. */
  def count(): Long

  /** The writes, described for a failure message: at most `limit` of them. */
  def describe(limit: Int): String
}
