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
