package tools.contracts

/**
 * A test double that breaks its trait's contract ON PURPOSE — a read that reports itself
 * incomplete, a write that never lands — so a spec can prove its caller survives that.
 * [[Implementations.of]] leaves these out: a contract suite holding them to the contract
 * would only restate that they were built to fail it.
 */
trait FailsOnPurpose
