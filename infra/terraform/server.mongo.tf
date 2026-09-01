# The database host. Takes over from the self-hosted `kinowo-mongo` Fly app in arn.
#
# WHY MOVING IT OFF FLY IS SAFE DESPITE THE ~27ms IT ADDS between the web tier (still Fly, arn) and
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
module "mongo_1" {
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

  name        = "mongo-1"
  server_type = "cx23"
  image       = "ubuntu-26.04" # bootstrap only; the machine runs NixOS. See modules/server/vars.tf.
  location    = "nbg1"
  network_ids = [hcloud_network.kinowo.id]
  private_ip  = "10.20.0.10"

  primary_ipv4_id = hcloud_primary_ip.fleet["mongo_1_ipv4"].id
  primary_ipv6_id = hcloud_primary_ip.fleet["mongo_1_ipv6"].id

  labels = {
    role = "mongo"
    env  = "prod"
  }

  # A SEPARATE VOLUME FOR THE DATABASE, not the 40GB root disk, for two reasons that both bite
  # later rather than now. It can be grown without touching the machine (Hetzner resizes a volume in
  # place; a server's root disk only grows by resizing the whole server type, and `keep_disk` means
  # it never shrinks back). And it survives the host: a `convert-host` re-run, a server-type change,
  # or a rebuild onto a fresh image all repartition the root disk, and the database is not on it.
  #
  # 10GB, WHICH IS HETZNER'S MINIMUM AND ~140x THE LIVE DATA. 5GB was asked for and is not
  # available: the API refuses anything outside 10-10240GB with `invalid input in field 'size':
  # Must be between 10 and 10240`, and it refuses it at CREATE time, after Terraform has already
  # destroyed the volume it was replacing. So 10 is the floor, not a preference.
  #
  # Measured against the running Fly database on 2026-08-29:
  # dataSize 0.07GB, storageSize 0.05GB, indexSize 0.01GB, 90,522 documents. This volume was
  # created at 80GB on the assumption that a year-old database must be large; it is not, and 80GB
  # was about EUR 4.60 a month to store nothing.
  #
  # THE OPLOG IS WHAT THIS NUMBER ACTUALLY CONSTRAINS, not the data. roles/mongodb.nix carried a 4GB
  # oplog -- most of this disk -- and was cut to 1GB in the same change. That is a real trade and it
  # is recorded there: the oplog is the window a disconnected change-stream consumer can resume
  # from, so a smaller one means a shorter worker outage survivable without a full re-read. At a
  # 70MB database 1GB is still a long window, but it is now bounded by a guess, not a measurement.
  #
  # Remaining budget at 5GB: ~1GB oplog, ~0.1GB data, the rotating mongodumps the backup timer
  # keeps, and WiredTiger's headroom to checkpoint before it can free anything. GROWING IS THE EASY
  # DIRECTION (in place, one apply), so starting small is the cheap mistake to make.

  # `mount_point` set, so Hetzner's automount is OFF and the mount is declared in
  # infra/nix/hosts/mongo-1/default.nix against /dev/disk/by-id/scsi-0HC_Volume_<id>. SETTING IT
  # HERE MOUNTS NOTHING -- if that `fileSystems` entry is missing, mongod silently writes the
  # production database to the root disk instead and looks perfectly healthy doing it.
  volumes = {
    mongo-data = {
      size        = 10
      location    = "nbg1"
      mount_point = "/var/lib/mongodb"

      # Independently protected, so that clearing the SERVER's delete_protection to re-run a
      # conversion cannot take the database's disk with it. It was briefly false to let the
      # 80GB -> 5GB shrink through, since a protected volume cannot be destroyed and a shrink is a
      # destroy-and-create.
      delete_protection = true
    }
  }
}

# THE SECOND DATABASE MACHINE, AND IT IS A MIGRATION IN PROGRESS RATHER THAN A STANDING PAIR.
#
# `rs0` is a one-member replica set and is meant to stay that way: two voting members need BOTH up
# to reach a majority, so a pair is strictly less available than a single node unless there is a
# third to break ties. mongo-2 exists to receive the database and then to BE the database, after
# which mongo-1 is retired -- see infra/ansible/playbooks/migrate-mongo-replica.yml, which adds this
# host with `votes: 0, priority: 0` precisely so the majority stays at one for as long as the copy
# takes.
#
# WHY REPLICATION AND NOT A SNAPSHOT, which is how k3s-worker-1 and monitoring-1 moved to fsn1 on
# 2026-09-01. Those hosts could be snapshotted and rebuilt because their state either did not matter
# or fitted in an rsync during a maintenance window. Neither is true here: the requirement was NO
# DOWNTIME, and a Hetzner volume cannot leave its location, so the disk could not follow and a
# window could not be taken. Replication is the only mechanism that moves a live database without
# one -- which is also why this host gets its own private address rather than inheriting 10.20.0.10.
module "mongo_2" {
  # FALSE UNTIL IT IS CONVERTED, unlike its neighbours, and this is the one moment that default is
  # right: Hetzner's rebuild protection also refuses `enable_rescue`, and rescue is how NixOS gets
  # onto the machine. Set it true once `convert-host mongo-2` has run.
  delete_protection = false

  source = "./modules/server"

  name        = "mongo-2"
  server_type = "cx23"
  image       = "ubuntu-24.04" # bootstrap only; the machine runs NixOS. See modules/server/vars.tf.
  location    = "fsn1"
  network_ids = [hcloud_network.kinowo.id]
  private_ip  = "10.20.0.13"

  primary_ipv4_id = hcloud_primary_ip.fleet["mongo_2_ipv4"].id
  primary_ipv6_id = hcloud_primary_ip.fleet["mongo_2_ipv6"].id

  labels = {
    role = "mongo"
    env  = "prod"
  }

  # SAME 10GB AS mongo-1'S, because it holds the same corpus with the same 4GB oplog -- see the
  # budget note on that volume. It is EMPTY at creation and is filled by the initial sync, which is
  # the entire point: the data arrives over the network from the primary, not from a copied disk.
  volumes = {
    mongo-data-fsn1 = {
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
  to = module.mongo_2.hcloud_server.default
  id = "164241785"
}

import {
  to = module.mongo_2.hcloud_volume.default["mongo-data-fsn1"]
  id = "106767580"
}
