# ubuntu-up

Scripts and configurations to booststrap Ubuntu installations.

## Invoke

```bash
# run from the internet:
bash <(curl -fsSL https://raw.githubusercontent.com/rriegs/ubuntu-up/main/init.sh)

# or, run from the cloned repo:
# git clone https://github.com/rriegs/ubuntu-up.git ~/.ubuntu-up
# bash ~/.ubuntu-up/init.sh
```

## What it does

- Adds VS Code and Steam APT repos/keys.
- Installs a small set of APT and Snap packages (see `init.sh` arrays).
- Loads `~/.ubuntu-up/gnome.dconf` into dconf if present.
- Symlinks files from `~/.ubuntu-up/dotfiles/` into `$HOME`.

## Notes

- Do not run as root; the script uses `sudo` when needed and will exit if EUID=0.
- Snap steps are skipped if `snap`/`snapd` isn't available.
- Dropbox and Steam may need a first interactive run to finish setup.

## Maintaining

- Edit `APT_PKGS`, `SNAP_PKGS`, or `SYMLINK_DIRS` in `init.sh` to change behavior.
- To capture GNOME settings:

```bash
dconf dump / > ~/.ubuntu-up/gnome.dconf
git -C ~/.ubuntu-up add gnome.dconf && git -C ~/.ubuntu-up commit -m "GNOME baseline"
```

That's it. Keep the repo in `~/.ubuntu-up` and re-run the script after changes.
