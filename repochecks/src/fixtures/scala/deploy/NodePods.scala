package deploy

/**
 * The ONE reader of what every pod on k3s-worker-1 requests, for [[NodeMemoryBudgetSpec]] and
 * [[NodeCpuBudgetSpec]]. Both ledgers ask the same question of the same pods -- only the resource
 * differs -- so the pod list lives here once, and neither spec can count a pod the other misses.
 * Until 2026-11-18 each kept its own hand-set "system reserve" (576Mi, 250m), and both had left out
 * coredns: the memory margin read 128Mi where the node had 58, and the CPU reserve was 250m against
 * 595m actually requested.
 *
 * The pods: the kinowo tiers (one overlay per tier and country), `OtherWorkloads`, the GitOps
 * system manifests, and every k3s packaged add-on not in k3s-server.nix's `disable` list.
 */
object NodePods {

  /** A resource both ledgers count: the manifest key, and its quantity parsed to the ledger's unit. */
  sealed abstract class Resource(val key: String, val unit: String) {
    def parse(raw: String): Int
  }

  /** Mebibytes. Binary suffixes only — a decimal `1G` would be a different number and is not a
   *  spelling this repo uses, so reject it rather than round it. */
  case object Memory extends Resource("memory", "Mi") {
    def parse(raw: String): Int = raw match {
      case s"${n}Gi" => n.toInt * 1024
      case s"${n}Mi" => n.toInt
      case other     => throw new IllegalArgumentException(s"unrecognised memory quantity `$other` — expected a Gi or Mi suffix")
    }
  }

  /** Millicores: `"500m"`, or a bare `"1"` in cores. */
  case object Cpu extends Resource("cpu", "m") {
    def parse(raw: String): Int = if (raw.endsWith("m")) raw.dropRight(1).toInt else (raw.toDouble * 1000).toInt
  }

  private def unquote(s: String) = s.trim.replace("\"", "").replace("'", "")

  /** The first `<key>:` under a manifest's `requests:` block, verbatim (quotes stripped). Every
   *  kinowo overlay carries one container, so the first block is the only one. `None` when the
   *  manifest declares no such request — the specs fail on that rather than default it, because a
   *  request missing from the sum is the one thing they must never miss. */
  def declared(path: String, key: String): Option[String] =
    RepoFile.read(path).linesIterator.map(_.trim).toList
      .dropWhile(_ != "requests:").drop(1)
      .takeWhile(_ != "limits:")
      .collectFirst { case l if l.startsWith(s"$key:") => unquote(l.stripPrefix(s"$key:")) }

  /** Every container's request of `resource` in one manifest, 0 for a container that declares
   *  none. A file with several Deployments — Flux ships five controllers in one — contributes all
   *  of them, or the ledger is under-counted by whatever the first block happens to be. */
  def requestsIn(path: String, resource: Resource): Seq[Int] =
    RepoFile.read(path).linesIterator.map(_.trim).toList.sliding(6).collect {
      case window if window.head == "requests:" =>
        window.tail.takeWhile(l => !l.startsWith("limits:")).collectFirst {
          case l if l.startsWith(s"${resource.key}:") => resource.parse(unquote(l.stripPrefix(s"${resource.key}:")))
        }.getOrElse(0)
    }.toSeq

  val Tiers: Seq[String]     = Seq("worker", "web")
  val Countries: Seq[String] = Seq("pl", "de", "uk", "us", "es")

  def overlay(tier: String, cc: String): String = s"infra/kubernetes/$tier/overlays/$cc/patch.yaml"

  /** Workloads on k3s-worker-1 that are not kinowo tiers but reserve real resources there: their
   *  requests count against the same ceiling. `filmowo` is a separate product (the recommend repo)
   *  pinned to the node; its one replica rolls with `Recreate`, so it never surges. */
  val OtherWorkloads: Seq[String] = Seq("infra/kubernetes/filmowo/all.yaml")

  /** The non-kinowo GitOps manifests that schedule pods on k3s-worker-1, besides `OtherWorkloads`. */
  val SystemManifests: Seq[(String, String)] = Seq(
    "kube-state-metrics" -> "infra/kubernetes/kube-state-metrics/deployment.yaml",
    "headlamp"           -> "infra/kubernetes/headlamp/deployment.yaml",
    // Flux's controllers (source, kustomize, notification, image-reflector, image-automation),
    // vendored from `flux install --export`.
    "flux"               -> "infra/kubernetes/flux/gotk-components.yaml",
    "reloader"           -> "infra/kubernetes/reloader/deployment.yaml",
  )

  /** One pod a k3s add-on schedules: what it requests. */
  final case class AddOnPod(memoryMib: Int, cpuMillis: Int) {
    def of(resource: Resource): Int = resource match {
      case Memory => memoryMib
      case Cpu    => cpuMillis
    }
  }

  /** The k3s packaged add-ons, by the name `services.k3s.disable` takes, and the pods each one
   *  ENABLED schedules -- requests read from the manifests k3s writes to
   *  /var/lib/rancher/k3s/server/manifests (v1.35.8+k3s1, checked against `kubectl describe node
   *  k3s-worker-1` on 2026-11-18). Every name k3s can deploy is a row, so turning one back on counts
   *  its pods here instead of nowhere. `None` is an add-on whose pods nobody has read yet: on, it
   *  fails the ledger until someone does. The pods carry no node affinity and the control plane is
   *  tainted, so they land on k3s-worker-1 -- coredns does today. */
  val K3sAddOns: Map[String, Option[Seq[AddOnPod]]] = Map(
    "coredns"        -> Some(Seq(AddOnPod(70, 100))),   // coredns.yaml: requests memory 70Mi, cpu 100m
    "local-storage"  -> Some(Seq(AddOnPod(0, 0))),      // local-path-provisioner: no requests at all
    "runtimes"       -> Some(Nil),                      // RuntimeClass objects only, no pod
    "metrics-server" -> Some(Seq(AddOnPod(70, 100))),   // metrics-server.yaml: requests memory 70Mi, cpu 100m
    "traefik"        -> None,                           // a HelmChart (traefik + a svclb DaemonSet); read it before enabling
    "servicelb"      -> None,                           // one svclb DaemonSet pod per LoadBalancer Service
  )

  val K3sServerNix = "infra/nix/modules/roles/k3s-server.nix"

  /** The add-ons k3s-server.nix turns off. */
  lazy val K3sDisabled: Set[String] = {
    val nix  = RepoFile.read(K3sServerNix)
    val list = """(?m)^\s*disable\s*=\s*\[([^\]]*)\]""".r.findFirstMatchIn(nix).map(_.group(1))
      .getOrElse(throw new IllegalStateException(s"$K3sServerNix sets no `disable = [ ... ];` for services.k3s"))
    """"([^"]+)"""".r.findAllMatchIn(list).map(_.group(1)).toSet
  }

  /** The k3s add-ons left on, with their pods. An enabled add-on nobody has read is an error. */
  lazy val EnabledAddOns: Seq[(String, Seq[AddOnPod])] =
    K3sAddOns.toSeq.sortBy(_._1).collect {
      case (name, pods) if !K3sDisabled(name) =>
        name -> pods.getOrElse(throw new IllegalStateException(
          s"k3s add-on $name is enabled (not in $K3sServerNix's disable list) but nobody has read what its pods request"))
    }

  /** Everything on k3s-worker-1 that is neither a kinowo tier nor `OtherWorkloads`, one entry per
   *  manifest or add-on, each with its containers' requests of `resource`. */
  def systemPods(resource: Resource): Seq[(String, Seq[Int])] =
    SystemManifests.map { case (name, path) => name -> requestsIn(path, resource) } ++
      EnabledAddOns.map { case (name, pods) => s"k3s $name" -> pods.map(_.of(resource)) }

  /** Every manifest under the GitOps checkout that declares a pod-scheduling kind. */
  lazy val WorkloadManifests: Set[String] = {
    val workload = """(?m)^kind:\s*(Deployment|StatefulSet|DaemonSet|ReplicaSet|Job|CronJob|Pod)\s*$""".r
    def walk(dir: java.io.File): Seq[java.io.File] =
      Option(dir.listFiles).toSeq.flatten.flatMap(f => if (f.isDirectory) walk(f) else Seq(f))
    walk(new java.io.File("infra/kubernetes"))
      .map(_.getPath)
      .filter(p => p.endsWith(".yaml") || p.endsWith(".yml"))
      .filter(p => workload.findFirstIn(RepoFile.read(p)).isDefined)
      .toSet
  }

  /** The manifests the ledger reads. */
  lazy val CountedManifests: Set[String] =
    (for (tier <- Tiers; cc <- Countries) yield overlay(tier, cc)).toSet ++
      Tiers.map(t => s"infra/kubernetes/$t/base/all.yaml") ++ OtherWorkloads ++ SystemManifests.map(_._2)

  /** One resource's ledger on k3s-worker-1: every pod's request, the largest web surge on top, and
   *  what is left. `floor` is the largest single system pod: the web tier is not the only thing that
   *  rolls -- Flux's, coredns's and kube-state-metrics' Deployments surge one pod too -- and an image
   *  bump landing beside a Flux upgrade surges both at once, so the margin must hold one of them. */
  final case class Budget(resource: Resource, allocatable: Int, kinowo: Seq[((String, String), Int)], others: Int,
                          system: Seq[(String, Seq[Int])]) {
    val systemTotal: Int = system.map(_._2.sum).sum
    val total: Int       = kinowo.map(_._2).sum + others + systemTotal
    val surge: Int       = kinowo.filter(_._1._1 == "web").map(_._2).max   // maxSurge: 1 on the web tier
    val margin: Int      = allocatable - total - surge
    val floor: Int       = system.flatMap(_._2).max
    private def u(n: Int) = s"$n${resource.unit}"
    def explain: String =
      s"kinowo ${u(kinowo.map(_._2).sum)} + other workloads ${u(others)} + system pods ${u(systemTotal)} " +
        s"(${system.map { case (n, pods) => s"$n ${u(pods.sum)}" }.mkString(", ")}) = ${u(total)}, + a ${u(surge)} surge pod = " +
        s"${u(total + surge)} of ${u(allocatable)} allocatable: ${u(margin)} margin against a ${u(floor)} floor (the largest " +
        "single system pod, which can roll at the same time)"
  }

  /** The `resource` ledger against `allocatable`. A kinowo overlay or other workload with no request
   *  of its own is an error: inheriting the base's would be invisible to the sum. */
  def budget(resource: Resource, allocatable: Int): Budget = {
    def own(path: String): Int = resource.parse(declared(path, resource.key).getOrElse(
      throw new IllegalStateException(s"$path declares no ${resource.key} request under `requests:`")))
    Budget(resource, allocatable,
      for (tier <- Tiers; cc <- Countries) yield (tier, cc) -> own(overlay(tier, cc)),
      OtherWorkloads.map(own).sum,
      systemPods(resource))
  }
}
