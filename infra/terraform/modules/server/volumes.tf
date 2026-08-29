resource "hcloud_volume" "default" {
  for_each = var.volumes

  # The key alone, not "<host>-<key>" -- the server a volume is attached to is already visible in
  # the API and in state. Volume names are unique per PROJECT, so two hosts declaring the same key
  # collide; give one a distinct key rather than reaching for the hostname.
  name              = each.key
  size              = each.value.size
  format            = "ext4"
  location          = each.value.location
  delete_protection = each.value.delete_protection != null ? each.value.delete_protection : var.delete_protection

  labels = {
    server = hcloud_server.default.name
    name   = each.key
  }
}

resource "hcloud_volume_attachment" "default" {
  for_each = var.volumes

  # Off when the caller names a mount point, because then the host's nix config owns the mount and
  # two fstab entries for one device is nobody's idea of clarity.
  automount = each.value.mount_point == null
  volume_id = hcloud_volume.default[each.key].id
  server_id = hcloud_server.default.id
}
