# Monitoring and the k3s control plane, on one machine, as specified.
#
# THOSE TWO ROLES ON ONE BOX IS A REAL TRADE and it is being made knowingly. The upside is that it
# costs nothing extra and the control plane is watched by a Prometheus that does not depend on it.
# The downside is that a k3s control plane under load is the loudest neighbour on a 2-core box, and
# it is sharing with the one service whose whole job is to still be answering when everything else
# is not. `fleet.observability` therefore gives Prometheus and Alertmanager an IOSchedulingClass and
# a CPUWeight above k3s's, so metrics keep being written while the apiserver is starving -- see
# infra/nix/modules/roles/prometheus.nix. If this host ever needs to be split, the k3s server is the
# half that moves; monitoring's storage is the half with state.
module "monitoring_1" {
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

  name        = "monitoring-1"
  server_type = "cx23"
  image       = "ubuntu-26.04" # bootstrap only; the machine runs NixOS. See modules/server/vars.tf.
  location    = "nbg1"
  network_ids = [hcloud_network.kinowo.id]
  private_ip  = "10.20.0.11"

  primary_ipv4_id = hcloud_primary_ip.fleet["monitoring_1_ipv4"].id
  primary_ipv6_id = hcloud_primary_ip.fleet["monitoring_1_ipv6"].id

  labels = {
    role = "monitoring"
    env  = "prod"
  }

  # Prometheus TSDB and Grafana's sqlite on their own disk, for the same reasons as mongo-1's -- and
  # one more that is specific to monitoring: the metrics that explain an incident are written during
  # the incident, and a monitoring host whose root disk filled is a host that stopped recording at
  # exactly the moment the recording mattered. 40GB holds a comfortable retention for three
  # node_exporters plus the Fly-side scrape; see the retention setting in roles/prometheus.nix,
  # which is what actually bounds it.
  volumes = {
    monitoring-data = {
      size              = 40
      location          = "nbg1"
      mount_point       = "/var/lib/monitoring"
      delete_protection = true
    }
  }
}
