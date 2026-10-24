# THE IMAGE REGISTRY, registry.kinowo.net -- the replacement for ghcr.io/pawelkrupinski after the
# GitHub account was suspended (2026-10). CI (Forgejo Actions on bitcashier's ci-runner) pushes here;
# the cluster and Flux's image-reflector pull from here.
#
# READS ARE ANONYMOUS, WRITES NEED A PASSWORD -- the same shape ghcr gave us with public packages,
# so the cluster still holds no pull secret and nothing in movies-gitops changes but the image
# names. The push password is `REGISTRY_PUSH_PASSWORD` in the `movies` Touch ID vault and the
# `REGISTRY_PASSWORD` Actions secret; only its bcrypt hash is here.
#
# ON THE ROOT DISK, ACCEPTED AS LOSABLE (operator decision, 2026-10). This host is otherwise
# stateless and has no volume and no backups; every image here is rebuildable from git by CI, so a
# lost disk costs a rebuild, not data. That is also why only the last ten images are kept.
{ config, lib, pkgs, ... }:

let
  port = 5000;
  keep = 10;
  domain = "registry.kinowo.net";
  acmeWebroot = "/var/lib/acme/acme-challenge";
  reg = config.services.dockerRegistry;
  textfileDir = config.fleet.observability.textfileDirectory;
  # The module keeps its generated config file private; its ExecStart is the one place it is named.
  registryConfig = lib.last (lib.splitString " " config.systemd.services.docker-registry.serviceConfig.ExecStart);

  # KEEP THE NEWEST TEN `main-<14-digit UTC timestamp>-<sha7>` TAGS PER REPOSITORY, by digest, plus
  # whatever `latest` points at; delete every other manifest. Those tags are what Flux's image
  # policies order on (movies-gitops image-automation/automation.yaml), so whatever is deployed is
  # always among the ten. The fixed-width timestamp is why a plain reverse sort is a time sort.
  #
  # A REPOSITORY WITH NO main-* TAG IS LEFT ALONE: no ordering means no notion of "newest", and
  # guessing is how a prune deletes the one image something runs.
  #
  # Known race: ci.yml pushes `<sha>` and main.yml re-tags it as main-* a few minutes later. A prune
  # landing in between deletes the not-yet-tagged image and that one deploy's re-tag fails; re-run it.
  prune = pkgs.writeShellApplication {
    name = "registry-prune";
    runtimeInputs = with pkgs; [ curl jq coreutils gnugrep ];
    text = ''
      api=http://127.0.0.1:${toString port}/v2
      accept='application/vnd.oci.image.index.v1+json,application/vnd.oci.image.manifest.v1+json,application/vnd.docker.distribution.manifest.list.v2+json,application/vnd.docker.distribution.manifest.v2+json'
      digest() {
        curl -fsSI -H "Accept: $accept" "$api/$1/manifests/$2" |
          tr -d '\r' | grep -i '^docker-content-digest:' | cut -d' ' -f2
      }

      for repo in $(curl -fsS "$api/_catalog?n=1000" | jq -r '.repositories[]?'); do
        tags=$(curl -fsS "$api/$repo/tags/list" | jq -r '.tags[]?')
        newest=$(grep -E '^main-[0-9]{14}-[0-9a-f]{7}$' <<<"$tags" | sort -r | head -n ${toString keep} || true)
        if [ -z "$newest" ]; then
          echo "$repo: no main-* tags, left alone"
          continue
        fi
        keepers=$(for t in $newest $(grep -x latest <<<"$tags" || true); do digest "$repo" "$t"; done | sort -u)
        deleted=0
        for t in $tags; do
          d=$(digest "$repo" "$t") || continue
          if [ -z "$d" ] || grep -qx "$d" <<<"$keepers"; then continue; fi
          # A digest shared by several tags goes with the first; the rest then 404, harmlessly.
          if curl -fsS -o /dev/null -X DELETE "$api/$repo/manifests/$d"; then deleted=$((deleted + 1)); fi
        done
        echo "$repo: kept $(wc -l <<<"$keepers") image(s), deleted $deleted"
      done
    '';
  };
in
{
  services.dockerRegistry = {
    enable = true;
    listenAddress = "127.0.0.1";
    inherit port;
    enableDelete = true;
    # NOT the module's own collector: it runs without --delete-untagged, so the per-platform
    # manifests under a deleted multi-arch index would keep their layers for ever.
    enableGarbageCollect = false;
  };

  # distribution 3 exports OpenTelemetry traces by default, to a collector this host does not run: a
  # "traces export: ... connection refused" error in the journal every 10 s, nothing else.
  systemd.services.docker-registry.environment.OTEL_TRACES_EXPORTER = "none";

  # Bouncing the pruner disturbs nothing that serves, so an edit to it must not hold back every
  # other change staged onto this host (see the same list in fleet/container-image-gc.nix).
  fleet.autoApply.restartableUnits = [
    "registry-prune.service"
    "registry-prune.timer"
  ];

  # Prune, then collect with the registry STOPPED -- the collector is not safe against a concurrent
  # push. A CI push in that minute fails and is retried by re-running the job.
  systemd.services.registry-prune = {
    description = "Keep the newest ${toString keep} images per repository, then garbage-collect";
    # NOT `requires`: Requires= propagates an explicit stop of docker-registry to this unit, so
    # the `systemctl stop` below would kill this very script -- no GC, and the registry left down.
    after = [ "docker-registry.service" ];
    startAt = "*-*-* 04:00:00";
    serviceConfig.Type = "oneshot";
    script = ''
      ${lib.getExe prune}
      # However the collector ends, the registry comes back up: the cluster pulls from it.
      trap 'systemctl start docker-registry.service' EXIT
      systemctl stop docker-registry.service
      ${pkgs.util-linux}/bin/runuser -u docker-registry -- \
        ${reg.package}/bin/registry garbage-collect --delete-untagged ${registryConfig}

      # THAT IT RAN, for `RegistryPruneStale` (files/monitoring/rules/k3s.rules): a timer that
      # stops starting this unit fails nothing, so `SystemdUnitFailed` never hears of it. Written
      # last (only a completed prune counts) and through a temporary, for ../fleet/nix-gc.nix's
      # reason: node_exporter drops every series of a textfile it cannot parse.
      tmp="${textfileDir}/registry-prune.prom.tmp"
      {
        echo "# HELP kinowo_registry_prune_last_success_timestamp_seconds When this host last completed a registry prune and garbage-collect."
        echo "# TYPE kinowo_registry_prune_last_success_timestamp_seconds gauge"
        echo "kinowo_registry_prune_last_success_timestamp_seconds $(date +%s)"
      } > "$tmp"
      chmod 0644 "$tmp"
      mv "$tmp" "${textfileDir}/registry-prune.prom"
    '';
  };

  # Served straight, NOT behind Cloudflare: the A record at OVH points at this host and is not
  # proxied, so the certificate must be a public one, and multi-hundred-MB layer pushes do not meet
  # Cloudflare's 100 MB request-body cap.
  #
  # THE CERTIFICATE COMES FROM security.acme (lego, HTTP-01 by webroot), NOT CADDY'S OWN ACME.
  # Caddy skips automating any name a manually loaded certificate already covers, and the
  # `*.kinowo.net` Cloudflare Origin certificate the proxied vhosts load covers this one -- so
  # Caddy served that origin certificate here (trusted by Cloudflare only; docker and curl reject
  # it) and never even tried ACME. Neither an explicit `tls { issuer acme }` nor anything short of
  # the global `auto_https ignore_loaded_certs` changes that, and the global option would put the
  # proxied names on Let's Encrypt too. An exact-name certificate loaded by file wins the SNI
  # lookup over the wildcard, and the caddy module wires the group and the reload on renewal.
  security.acme.certs.${domain}.webroot = acmeWebroot;
  services.caddy.virtualHosts.${domain}.useACMEHost = domain;
  # Port 80 for this name is ours to define once the certificate is not Caddy's: the challenge
  # files from the webroot, everything else to https as Caddy's automatic redirect would.
  services.caddy.virtualHosts."http://${domain}".extraConfig = ''
    handle /.well-known/acme-challenge/* {
      root * ${acmeWebroot}
      file_server
    }
    handle {
      redir https://{host}{uri} permanent
    }
  '';

  fleet.publicProxy.vhosts.${domain} = {
    upstream = "127.0.0.1:${toString port}";
    extraConfig = ''
      @write not method GET HEAD OPTIONS
      basic_auth @write {
        ci $2a$14$f0wQs4GtJqdm4cfxGj1TBe/.XXbfShe71.8TZ9d5uKkAW95G2pWhG
      }
    '';
  };
}
