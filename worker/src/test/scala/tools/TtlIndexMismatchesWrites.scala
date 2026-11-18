package tools

/** The reconciler's two writes to [[TtlIndexMismatches]] -- package-private to `tools`, where `MongoTtlIndex` is, so
 *  no production caller can invent a mismatch -- for a spec outside the package that needs one recorded
 *  (`services.metrics.TtlIndexMetricsSpec`). */
object TtlIndexMismatchesWrites {
  extension (mismatches: TtlIndexMismatches) {
    def recordForSpec(namespace: String): Unit = mismatches.record(namespace)
    def resolvedForSpec(namespace: String): Unit = mismatches.resolved(namespace)
  }
}
