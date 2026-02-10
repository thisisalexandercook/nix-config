{ config, pkgs, ... }:

{

  environment.systemPackages = [ pkgs.forgejo ];

  services.forgejo = import ./forge-conf.nix {
    stateDir = "/mnt/data/forgejo";
    port = 3000;
    domain = "bytes";
    rootUrl = "http://bytes:3000/";
    extraServerSettings = {};
  };

  systemd.services.forgejo.unitConfig.RequiresMountsFor = "/mnt/data";
  systemd.services.forgejo-secrets.unitConfig.RequiresMountsFor = "/mnt/data";

systemd.services.forgejo-secrets.serviceConfig.ExecStartPre = "+${pkgs.bash}/bin/bash -c '${pkgs.coreutils}/bin/mkdir -p /mnt/data/forgejo/custom/conf && ${pkgs.coreutils}/bin/chown -R forgejo:forgejo /mnt/data/forgejo'";

  systemd.tmpfiles.rules = [
    "d /mnt/data/forgejo 0750 forgejo forgejo - -"
    "d /mnt/data/forgejo/custom 0750 forgejo forgejo - -"
    "d /mnt/data/forgejo/custom/conf 0750 forgejo forgejo - -"
  ];
}
