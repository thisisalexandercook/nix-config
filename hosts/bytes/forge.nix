{ config, pkgs, ... }:

{

  environment.systemPackages = [ pkgs.forgejo ];

  services.forgejo = {
    enable = true;
    database.type = "sqlite3";
    stateDir = "/mnt/data/forgejo";

    lfs.enable = true;

    settings = {
      server = {
        DOMAIN = "bytes";
        HTTP_ADDR = "0.0.0.0";
        HTTP_PORT = 3000;
        ROOT_URL = "http://bytes:3000/";
      };
      service.DISABLE_REGISTRATION = true;
    };
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
