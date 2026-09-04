# The database host. It took over from the self-hosted `kinowo-mongo` Fly app in arn.
#
# WHY MOVING IT OFF FLY WAS SAFE DESPITE THE ~27ms IT ADDED between the web tier (then Fly, arn) and
# the database (here, nbg1): the web tier does ZERO Mongo round trips on a page render. It hydrates
# the whole read model into two ConcurrentHashMaps at boot and keeps them fresh over Mongo change
# streams (common/src/main/scala/services/readmodel/WebReadModel.scala), then serves responses out
# of a versioned gzip cache. The measured exposure is the boot hydrate (~13 paged round trips at 500
# docs a page, +0.35s) and change-stream delta propagation (+27ms per event, unobservable). No Mongo
# timeout in the app is tighter than 30s.
#
# The one place that DOES pay is the worker's ReadModelProjector, which writes one `replaceOne` per
# changed document sequentially under a lock -- normal churn is a handful of docs, but a full
# ~6.5k-document reconcile costs about three extra minutes. That is a batch job with no user waiting
# on it, and the fix if it ever matters is a bulkWrite, not a datacentre.
# THE DATABASE, IN fsn1 SINCE 2026-09-02. It replaced a machine of the same name in nbg1 and took
# its name once the corpus was here; it kept its own private address, 10.20.0.13, because reclaiming
# .10 would have meant a second migration to undo a difference nothing reads.
#
# IT MOVED BY REPLICATION, NOT BY A SNAPSHOT, unlike k3s-worker-1 and monitoring-1 the day before.
# Those could be snapshotted and rebuilt because their state either did not matter or fitted in an
# rsync during a maintenance window. Neither was true here: the requirement was NO DOWNTIME, and a
# Hetzner volume cannot leave its location, so the disk could not follow and no window could be
# taken. The replacement joined `rs0` as a non-voting secondary, caught up, and was handed the
# primary by an election -- see infra/ansible/playbooks/migrate-mongo-replica.yml.
#
# `rs0` IS A ONE-MEMBER SET AGAIN AND IS MEANT TO BE. Two voting members need BOTH up to reach a
# majority, so a pair is strictly less available than a single node unless there is a third to break
# ties. The second member existed only for the length of the move.
module "mongo_1" {
  # PROTECTED NOW THAT IT IS CONVERTED AND HOLDS THE DATABASE. The module defaults this to false
  # because Hetzner's rebuild protection also refuses `enable_rescue`, and rescue is how NixOS gets
  # onto a machine -- right for a host being built, wrong for the one serving the corpus.
  delete_protection = true

  source = "./modules/server"

  name        = "mongo-1"
  server_type = "cx23"
  image       = "ubuntu-24.04" # bootstrap only; the machine runs NixOS. See modules/server/vars.tf.
  location    = "fsn1"
  network_ids = [hcloud_network.kinowo.id]
  private_ip  = "10.20.0.13"

  primary_ipv4_id = hcloud_primary_ip.fleet["mongo_1_ipv4"].id
  primary_ipv6_id = hcloud_primary_ip.fleet["mongo_1_ipv6"].id

  labels = {
    role = "mongo"
    env  = "prod"
  }

  # SAME 10GB AS mongo-1'S, because it holds the same corpus with the same 4GB oplog -- see the
  # budget note on that volume. It is EMPTY at creation and is filled by the initial sync, which is
  # the entire point: the data arrives over the network from the primary, not from a copied disk.
  volumes = {
    mongo-data = {
      size        = 10
      location    = "fsn1"
      mount_point = "/var/lib/mongodb"

      # Independently protected for the same reason mongo-1's is: clearing the SERVER's protection
      # to re-run a conversion must not be able to take the database's disk with it.
      delete_protection = true
    }
  }
}

# Adopted 2026-09-01. The machine was created by hand as stock Ubuntu and is brought under Terraform
# at the name, type and location it already had -- except `name`, which is an in-place update from
# Hetzner's generated `ubuntu-4gb-fsn1-1`.
import {
  to = module.mongo_1.hcloud_server.default
  id = "164241785"
}

import {
  to = module.mongo_1.hcloud_volume.default["mongo-data"]
  id = "106767580"
}
