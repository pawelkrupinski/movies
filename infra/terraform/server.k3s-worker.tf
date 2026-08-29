# The k3s worker. The only cx43 on the fleet, and the only host in hel1.
#
# hel1 rather than nbg1 is where Hetzner put it and it is left there deliberately: hel1 and nbg1 are
# both in the `eu-central` network zone, so it shares the private subnet with the control plane with
# no peering and no extra routes (see network.tf), and the ~20ms between Helsinki and Nuremberg is
# paid by kubelet heartbeats and image pulls, not by anything a user waits on. If a workload lands
# here that talks to mongo-1 per request, THAT is the point to move this machine to nbg1 -- not
# before.
#
# NOTHING IS SCHEDULED ON IT YET. It joins the cluster and sits idle, which is the intended state
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
  location    = "hel1"
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
  id = "163926648"
}

import {
  to = module.k3s_worker_1.hcloud_server.default
  id = "163926716"
}
