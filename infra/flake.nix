{
  description = "NixOS hosts for the kinowo fleet: the MongoDB primary, the monitoring node that also carries the k3s control plane, and the k3s worker.";

  # THE FLAKE ROOT IS infra/, NOT THE REPOSITORY ROOT, and that is the one structural difference
  # from bitcashier/infra worth understanding before copying anything else across. There, the flake
  # sits at the repo root because host configs reference scripts under puppet-src/ and pure
  # evaluation refuses any path outside the flake. Here the repository is a Scala application, not
  # an infrastructure repo: nothing under web/, worker/, common/ or ios/ is ever referenced by a
  # NixOS config, and rooting the flake at the top would drag ~18k tracked files into the store on
  # every evaluation to reach the handful under infra/.
  #
  # THE ONE THING THAT CROSSES THAT BOUNDARY is Grafana's provisioning, which lives at
  # ../fly/grafana/provisioning because the Fly deployment still reads it and is being KEPT as the
  # rollback. It is vendored into infra/nix/files/monitoring/grafana/ rather than referenced across
  # the boundary, and infra/bin/sync-grafana-provisioning is what keeps the copy honest. Vendoring
  # that drifts is a real cost; a flake that cannot evaluate is a worse one.

  inputs = {
    # PIN, not a moving branch. Matches bitcashier/infra so that a module ported between the two
    # fleets is evaluated against the same nixpkgs and a fix found on one applies unchanged to the
    # other.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    # Declarative partitioning. Load-bearing here in a way it is not on a fleet built from a
    # prebuilt NixOS snapshot: these three hosts are converted from stock Hetzner Ubuntu by
    # nixos-anywhere, which means disko IS the installer. The layout in each host's disko.nix is
    # what the machine gets, and re-running the conversion repartitions from the same declaration
    # rather than from whatever the last operator typed.
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Secrets. age-based, sealed to an operator key plus one key per host derived from that host's
    # SSH ed25519 host key. Same shape and same .sops.yaml conventions as bitcashier/infra.
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Deployment. Chosen over deploy-rs for the same reason bitcashier chose it: the per-host
    # `deployment` block maps onto a per-host directory layout without a second source of truth.
    colmena = {
      url = "github:zhaofengli/colmena";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, disko, sops-nix, colmena, ... }@inputs:
    let
      system = "x86_64-linux";

      # THE SYSTEMS PEOPLE WORK ON, which is not the system the hosts are built for. The operator
      # here runs aarch64-darwin, and a `devShells.${system}`-only flake answers "does not provide
      # attribute devShells.aarch64-darwin.default" to the one person who needs the shell -- which
      # bitcashier discovered mid-cutover, at the step that needs ssh-to-age. A tool added to a
      # shell nobody can enter has not been added.
      developmentSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      # Every host is built from the same three layers: disko for the disk, sops-nix for the
      # secrets, and infra/nix/modules/fleet for everything true of all three machines. Roles are
      # imported by the host itself, because which roles a host carries is the one thing that
      # actually distinguishes them.
      #
      # THE LIST IS A FUNCTION, AND BOTH CONSUMERS CALL IT, so `nixosConfigurations.<name>` and the
      # colmena node of the same name cannot drift apart. That matters more than it looks: a host
      # deployed by colmena and the same host built by the closure-staging workflow (which reads
      # `nixosConfigurations`) must be the SAME system, or auto-apply activates a closure nobody
      # deployed and no `colmena apply` ever converges. Restating the module list twice is the
      # obvious way to break that, so it is stated once.
      hostModules = name: [
        disko.nixosModules.disko
        sops-nix.nixosModules.sops
        ./nix/modules/fleet
        ./nix/hosts/${name}
      ];

      host = name: nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = hostModules name;
      };

      # A colmena node is just a module list plus a `deployment` block; everything but the tags is
      # identical across the three.
      node = name: tags: { config, ... }: {
        imports = hostModules name;

        deployment = {
          inherit tags;

          # READ OFF THE HOST'S OWN CONFIGURATION rather than repeated here. These three addresses
          # were literals in this file until 2026-08-29, at which point they existed in three
          # places -- here, each host's `fleet.publicAddress`, and terraform/primary_ips.tf -- and
          # the host files did not have them at all, so the staging workflow failed on its first
          # run with `unreachable-by-declaration` for all three. One definition, and colmena and
          # bin/stage-nixos-closures now necessarily agree about where a host is.
          targetHost = config.fleet.publicAddress;

          # ssh as whoever the operator is rather than forcing root, with sudo for the activation
          # itself -- so a deploy is attributable to a person in the host's journal instead of
          # arriving as an anonymous root session.
          targetUser = null;
          privilegeEscalationCommand = [ "sudo" "-H" "--" ];

          # NOT AN OPTIMISATION. The operator's machine is aarch64-darwin and cannot build
          # x86_64-linux closures at all without a remote builder, so a local build is not a slower
          # path here, it is no path.
          buildOnTarget = true;
        };
      };

    in
    {
      # `nixos-anywhere --flake .#<name>`, `nixos-rebuild --flake .#<name>` and the closure builder
      # in .github/workflows/nix-stage-closures.yaml all read these. The colmena hive below reads
      # the same definitions. One definition, four consumers -- which is the property that makes
      # "auto apply" safe to leave running: every path that can change a host is evaluating the
      # identical expression.
      nixosConfigurations = {
        mongo-1 = host "mongo-1";
        mongo-2 = host "mongo-2";
        monitoring-1 = host "monitoring-1";
        k3s-worker-1 = host "k3s-worker-1";
      };

      colmenaHive = colmena.lib.makeHive {
        meta = {
          nixpkgs = import nixpkgs { inherit system; };
          specialArgs = { inherit inputs; };
        };

        # THE ONE DELIBERATE DIVERGENCE FROM bitcashier's HIVE: it sets
        # `targetHost = config.fleet.privateAddress`, because that fleet is administered from inside
        # its own private network through a jump host. This fleet has no jump host and no bastion --
        # three machines reached from a laptop over the public internet -- so colmena must dial the
        # PUBLIC address or it cannot connect at all.
        #
        # WHAT THAT COSTS, stated plainly: deployment traffic crosses the internet rather than the
        # private network. It is SSH either way, so confidentiality is unchanged; what is lost is
        # that a misconfigured public firewall now breaks deploys as well as everything else, and
        # that these three addresses are the only place in the flake where a public IP is written
        # down. They are stable because primary_ips.tf pins them (`auto_delete = false`); if one
        # ever does change, this is the file that goes stale, and the symptom is a connection
        # timeout rather than a deploy landing on the wrong machine.
        #
        # THE DATABASE HOST IS TAGGED SEPARATELY AND IS NOT MEANT FOR A FLEET-WIDE PUSH. A rebuild
        # that restarts mongod stops the Fly-hosted web tier's change streams, so mongo-1 is
        # deployed on its own (`colmena apply --on @database`) rather than swept up in
        # `colmena apply`. The automated half of that same rule is `neverDisturbUnits` in
        # nix/modules/fleet/auto-apply.nix -- tags govern what a human does, that governs what the
        # timer is allowed to do.
        mongo-1 = node "mongo-1" [ "database" ];
        mongo-2 = node "mongo-2" [ "database" ];
        monitoring-1 = node "monitoring-1" [ "monitoring" "k3s" ];
        k3s-worker-1 = node "k3s-worker-1" [ "k3s" ];
      };

      devShells = nixpkgs.lib.genAttrs developmentSystems (s:
        let pkgs = nixpkgs.legacyPackages.${s};
        in {
          default = pkgs.mkShell {
            # ssh-to-age is the one that is genuinely load-bearing: .sops.yaml keys every host by an
            # age key DERIVED from that host's SSH ed25519 host key, so adding a host means running
            # it, and a host cannot be deployed until its secrets file is sealed to a key that
            # exists.
            packages = with pkgs; [
              sops
              age
              ssh-to-age
              colmena.packages.${s}.colmena
              nixos-anywhere
              terraform
              ansible
              mongosh
            ];
          };
        });
    };
}
