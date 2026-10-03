package deploy

/**
 * The first `<key>:` under a manifest's `requests:` block, verbatim (quotes stripped), for the node
 * budget specs. Every manifest they read carries one container, so the first block is the only
 * one. `None` when the manifest declares no such request — the specs fail on that rather than
 * default it, because a request missing from the sum is the one thing they must never miss.
 */
object ManifestRequests {

  def declared(path: String, key: String): Option[String] =
    RepoFile.read(path).linesIterator.map(_.trim).toList
      .dropWhile(_ != "requests:").drop(1)
      .takeWhile(_ != "limits:")
      .collectFirst { case l if l.startsWith(s"$key:") => l.stripPrefix(s"$key:").trim.replace("\"", "").replace("'", "") }

  /** Workloads on k3s-worker-1 that are not kinowo tiers but reserve real resources there: their
   *  requests count against the same ceiling. `filmowo` is a separate product (the recommend repo)
   *  pinned to the node; its one replica rolls with `Recreate`, so it never surges. */
  val OtherWorkloads: Seq[String] = Seq("infra/kubernetes/filmowo/all.yaml")
}
