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
  # 80GB, AND THAT IS KNOWN TO BE ~1100x THE LIVE DATA. Measured against the running Fly database on
  # 2026-08-29, AFTER this volume had already been created: dataSize 0.07GB, storageSize 0.05GB,
  # indexSize 0.01GB, 90,522 documents, mongod 7.0.39. The 80 was a guess made on the assumption
  # that a year-old database must be large. It is not.
  #
  # WHAT WOULD ACTUALLY SIZE IT is not the data at all: the 4GB oplog the replica set needs (see
  # `oplogSizeMB` in roles/mongodb.nix), the seven rotating mongodumps the backup timer keeps, and
  # WiredTiger's headroom to checkpoint before it can free anything. 20GB covers all of that with
  # room to spare.
  #
  # IT IS LEFT AT 80 DELIBERATELY RATHER THAN QUIETLY CORRECTED, because correcting it is not an
  # edit -- it is a destroy-and-create of the database's disk across two applies (see below), and
  # that is a decision to take on purpose rather than fold into an unrelated change. The standing
  # cost of not doing it is roughly EUR 3.40 a month. Do it while the volume is still empty or not
  # at all; once the migration has run, the same correction means moving live data.
  #
  # THE PROVIDER WILL NOT WARN YOU ABOUT ANY OF THAT. A shrink PLANS as an ordinary in-place update
  # -- `~ size = 80 -> 20`, `0 to add, 1 to change, 0 to destroy` -- and only Hetzner's API refuses
  # it, mid-apply, with `volume size is too small (invalid_input): size needs to be larger than
  # current volume size`. Observed on this exact volume on 2026-08-29.
  #
  # So do not leave a smaller number here in the hope a future apply picks it up: it never will, and
  # a config that plans a change it can never make is worse than an oversized disk, because a
  # permanently dirty plan is where real drift goes to hide.
  #
  # `mount_point` set, so Hetzner's automount is OFF and the mount is declared in
  # infra/nix/hosts/mongo-1/default.nix against /dev/disk/by-id/scsi-0HC_Volume_<id>. SETTING IT
  # HERE MOUNTS NOTHING -- if that `fileSystems` entry is missing, mongod silently writes the
  # production database to the root disk instead and looks perfectly healthy doing it.
  volumes = {
    mongo-data = {
      size        = 80
      location    = "nbg1"
      mount_point = "/var/lib/mongodb"

      # Independently protected, so that clearing the SERVER's delete_protection to re-run a
      # conversion cannot take the database's disk with it.
      delete_protection = true
    }
  }
}
