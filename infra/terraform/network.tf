# The private network every host on this fleet talks over. Mongo listens on it, Prometheus scrapes
# over it, and k3s runs its control plane across it -- none of those are reachable from the public
# internet on any host here.
#
# 10.20.0.0/16 rather than bitcashier's 10.0.0.0/8: these are different Hetzner projects and their
# networks never peer, so the ranges need not agree -- but mongo-1 also joins Fly's 6PN over
# WireGuard (see infra/nix/modules/roles/wireguard-fly.nix), and picking a range that cannot
# collide with anything already routed keeps that routing table trivially readable.
resource "hcloud_network" "kinowo" {
  name              = "kinowo"
  ip_range          = "10.20.0.0/16"
  delete_protection = true

  labels = {
    network = "private"
  }
}

# ONE subnet, and it spans both datacentres. fsn1 (monitoring-1, k3s-worker-1) and nbg1 (mongo-1)
# are both in the `eu-central` network zone, which is the unit a cloud subnet is scoped to -- so the
# database in Nuremberg sits on the same 10.20.0.0/24 as the cluster in Falkenstein with no
# peering, no routes and no second subnet. Had they landed in different zones this design would not
# work at all and the worker would need a public-network k3s join instead.
resource "hcloud_network_subnet" "kinowo_cloud" {
  network_id   = hcloud_network.kinowo.id
  type         = "cloud"
  network_zone = "eu-central"
  ip_range     = "10.20.0.0/24"
}
