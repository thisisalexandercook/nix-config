{ pkgs, ... }:

let
  backupRoot = "/mnt/backup";
  sourceDir = "/mnt/data/forgejo";
  mirrorDir = "${backupRoot}/forgejo";
  snapshotRoot = "${backupRoot}/forgejo-snapshots";
  keepSnapshots = 7;
  systemctl = "${pkgs.systemd}/bin/systemctl";
  rsync = "${pkgs.rsync}/bin/rsync";
  mkdir = "${pkgs.coreutils}/bin/mkdir";
  cp = "${pkgs.coreutils}/bin/cp";
  ln = "${pkgs.coreutils}/bin/ln";
  rm = "${pkgs.coreutils}/bin/rm";
  date = "${pkgs.coreutils}/bin/date";
  readlink = "${pkgs.coreutils}/bin/readlink";
  sync = "${pkgs.coreutils}/bin/sync";
  head = "${pkgs.coreutils}/bin/head";
  wc = "${pkgs.coreutils}/bin/wc";
  find = "${pkgs.findutils}/bin/find";
  sort = "${pkgs.coreutils}/bin/sort";
in
{
  systemd.services.forgejo-backup = {
    description = "Consistent snapshot backup of the primary Forgejo state";
    after = [ "mnt-data.mount" "mnt-backup.mount" ];
    wants = [ "mnt-data.mount" "mnt-backup.mount" ];
    unitConfig.RequiresMountsFor = [ "/mnt/data" "/mnt/backup" ];

    serviceConfig = {
      Type = "oneshot";
    };

    script = ''
      set -euo pipefail

      primary_was_active=0
      backup_was_active=0

      if ${systemctl} is-active --quiet forgejo.service; then
        primary_was_active=1
      fi

      if ${systemctl} is-active --quiet container@forge-backup.service; then
        backup_was_active=1
      fi

      restore_services() {
        if [ "$primary_was_active" -eq 1 ]; then
          ${systemctl} start forgejo.service
        fi

        if [ "$backup_was_active" -eq 1 ]; then
          ${systemctl} start container@forge-backup.service
        fi
      }

      trap restore_services EXIT

      if [ "$backup_was_active" -eq 1 ]; then
        ${systemctl} stop container@forge-backup.service
      fi

      if [ "$primary_was_active" -eq 1 ]; then
        ${systemctl} stop forgejo.service
      fi

      timestamp="$(${date} +%Y-%m-%dT%H-%M-%S)"
      snapshot_dir="${snapshotRoot}/$timestamp"
      latest_link="${snapshotRoot}/latest"

      ${mkdir} -p "${snapshotRoot}" "${mirrorDir}" "$snapshot_dir"

      if [ -L "$latest_link" ]; then
        latest_snapshot="$(${readlink} -f "$latest_link")"
        ${cp} -al "$latest_snapshot/." "$snapshot_dir/"
      fi

      ${rsync} -aHAX --delete --numeric-ids "${sourceDir}/" "$snapshot_dir/"
      ${rsync} -aHAX --delete --numeric-ids "$snapshot_dir/" "${mirrorDir}/"
      ${ln} -sfn "$snapshot_dir" "$latest_link"
      ${sync}

      snapshot_count="$(${find} "${snapshotRoot}" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | ${sort} | ${wc} -l)"
      if [ "$snapshot_count" -gt ${toString keepSnapshots} ]; then
        prune_count=$(( snapshot_count - ${toString keepSnapshots} ))
        ${find} "${snapshotRoot}" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' \
          | ${sort} \
          | ${head} -n "$prune_count" \
          | while IFS= read -r snapshot; do
              ${rm} -rf -- "${snapshotRoot}/$snapshot"
            done
      fi

      trap - EXIT
      restore_services
    '';
  };

  systemd.timers.forgejo-backup = {
    description = "Nightly backup of the primary Forgejo state";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
      RandomizedDelaySec = "30m";
      Unit = "forgejo-backup.service";
    };
  };
}
