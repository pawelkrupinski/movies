# The k3s worker. The only cx43 on the fleet, and the only host in fsn1.
#
# MOVED HERE FROM hel1 ON 2026-09-01, ON THE TRIGGER THIS COMMENT USED TO NAME. It said: leave it in
# Helsinki until a workload lands that talks to mongo-1 per request, and THEN move it. That workload
# landed -- the six app pods (web-pl/de/uk and the country workers) all run here and all read mongo-1
# in nbg1 on the request path, so the link was being paid per query rather than per kubelet
# heartbeat. Measured before the move: hel1 -> nbg1 24.1ms, fsn1 -> nbg1 4.5ms. Same `eu-central`
# network zone either way, so the private subnet, the k3s join and the routes are untouched
# (see network.tf); the only thing that changed is the distance.
#
# THE MOVE WAS A SNAPSHOT AND A REBUILD, not a re-conversion: Hetzner cannot move a server between
# locations, so the hel1 machine was snapshotted live and that image was rebuilt onto a cx43 already
# standing in fsn1. Root is mounted `by-partlabel` (see disko.nix), so it boots on a disk with a
# different serial; the host ssh keys, the sops age identity, the k3s node identity and Caddy's
# certificate store all travelled inside the image, which is why no re-key and no fresh ACME
# issuance was needed. THE PUBLIC IP COULD NOT TRAVEL -- Hetzner primary IPs are location-scoped --
# so 204.168.140.213 was released and the A records moved to 2.28.47.31 (see docs/domain-cutover.md).
#
# IT CARRIES THE WHOLE PRODUCT. It joins the cluster and runs every app pod, which is the state
# until there is a workload to put on it.
module "k3s_worker_1" {
  # PROTECTED NOW THAT IT IS CONVERTED. The module defaults this to FALSE, unlike bitcashier's,
  # because Hetzner's rebuild protection also refuses `enable_rescue` -- and rescue is how NixOS gets
  # onto these machines. That default is right for a host being built; it is wrong for one that is
  # serving, and leaving it is how a `terraform destroy` typo or a bad `-target` takes a live host.
  #
  # THE COST IS DELIBERATE FRICTION: re-running `convert-host` against this machine now requires
  # setting this back to false and applying first. That is a second look before repartitioning a
  # host that is in service, which is the point.
  delete_protection = true

  source = "./modules/server"

  name        = "k3s-worker-1"
  server_type = "cx43"
  image       = "ubuntu-26.04" # bootstrap only; the machine runs NixOS. See modules/server/vars.tf.
  location    = "fsn1"
  network_ids = [hcloud_network.kinowo.id]
  private_ip  = "10.20.0.12"

  primary_ipv4_id = hcloud_primary_ip.fleet["k3s_worker_1_ipv4"].id
  primary_ipv6_id = hcloud_primary_ip.fleet["k3s_worker_1_ipv6"].id

  labels = {
    role = "k3s-worker"
    env  = "prod"
  }

  # No volume. This host holds no state worth keeping -- container images and ephemeral pod storage
  # rebuild themselves, and its 160GB root disk is four times mongo-1's. A workload that needs a
  # persistent disk should get one through the hcloud CSI driver in the cluster, not a volume pinned
  # to one node here, or it can never be rescheduled.
  #
  # BACKUPS OFF for the same reason: seven rotating snapshots of a stateless node is 20% of a cx43's
  # price to preserve nothing that the flake does not already describe.
  backups = false
}

# Adopted 2026-08-29. All three servers were created by hand that morning as stock Ubuntu 26.04 and
# are brought under Terraform at the names, types and locations they already had -- except `name`,
# which is an in-place update: Hetzner's generated names (`ubuntu-4gb-nbg1-1`, `ubuntu-16gb-hel1-7`)
# say nothing about what the machine does, and `role`-keyed Ansible groups and the flake attribute
# names both read better against `mongo-1`.
#
# `image` is pinned to what each was built from precisely BECAUSE it is ForceNew: declaring anything
# else here proposes destroying all three. See modules/server/vars.tf.
import {
  to = module.mongo_1.hcloud_server.default
  id = "163926647"
}

import {
  to = module.monitoring_1.hcloud_server.default
  id = "164234644"
}

import {
  to = module.k3s_worker_1.hcloud_server.default
  id = "164234589"
}
