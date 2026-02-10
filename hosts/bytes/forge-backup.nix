{ config, pkgs, ... }:

{
  containers.forge-backup = {
    autoStart = true;
    privateNetwork = false;

    bindMounts = {
      "/mnt/backup" = { hostPath = "/mnt/backup"; isReadOnly = false; };
    };

    config = { config, pkgs, ... }: {

      users.users.forgejo.uid = 990;
      users.groups.forgejo.gid = 986;

      services.forgejo = import ./forge-conf.nix {
        stateDir = "/mnt/backup/forgejo";
        port = 3001;
        domain = "bytes";
        rootUrl = "http://bytes:3001/";
        extraServerSettings = {
          SSH_PORT = 2222;
          SSH_LISTEN_PORT = 2222;
          START_SSH_SERVER = true;
        };
      };

      systemd.services.forgejo.unitConfig.RequiresMountsFor = "/mnt/backup";

      systemd.services.forgejo.serviceConfig.ExecStartPre = "+${pkgs.bash}/bin/bash -c '${pkgs.coreutils}/bin/mkdir -p /mnt/backup/forgejo/custom/conf && ${pkgs.coreutils}/bin/chown -R forgejo:forgejo /mnt/backup/forgejo'";
    };
  };
}
