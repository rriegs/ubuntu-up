#!/usr/bin/env bash
# Safe to run via:
#   bash <(curl -fsSL https://raw.githubusercontent.com/rriegs/ubuntu-up/main/bootstrap.sh)
set -euo pipefail

# ── Config ────────────────────────────────────────────────────────────────────
GITHUB_REPO="rriegs/ubuntu-up"
REPO_SSH="git@github.com:$GITHUB_REPO.git"
REPO_HTTPS="https://github.com/$GITHUB_REPO.git"
CHECKOUT_DIR="$HOME/.ubuntu-up"

# ── Repo constants ────────────────────────────────────────────────────────────
# VS Code (Microsoft)
VSCODE_KEY_URL="https://packages.microsoft.com/keys/microsoft.asc"
VSCODE_REPO_URI="https://packages.microsoft.com/repos/code"
VSCODE_GPG="/usr/share/keyrings/packages.microsoft.gpg"
VSCODE_LIST="/etc/apt/sources.list.d/vscode.list"

# Steam (Valve)
STEAM_KEY_URL="https://repo.steampowered.com/steam/archive/ubuntu/steam.gpg"
STEAM_REPO_URI="https://repo.steampowered.com/steam/"
STEAM_GPG="/usr/share/keyrings/steam.gpg"
STEAM_LIST="/etc/apt/sources.list.d/steam.list"

# ── Desired packages ──────────────────────────────────────────────────────────
APT_PKGS=(aptitude code steam nautilus-dropbox)
SNAP_PKGS=(pied discord)

# ── Dotfiles ──────────────────────────────────────────────────────────────────
SYMLINK_DIRS=(.emacs.d/site-lisp)

# ── UI helpers ────────────────────────────────────────────────────────────────
say()  { printf "\n\033[1;34m==>\033[0m %s\n" "$*"; }
warn() { printf "\n\033[1;33m!!\033[0m %s\n" "$*"; }
die()  { printf "\n\033[1;31mXX\033[0m %s\n" "$*"; exit 1; }

# ── Safety checks ─────────────────────────────────────────────────────────────
if [[ "${EUID:-$(id -u)}" -eq 0 ]]; then
  die "Do NOT run as root. Run as your user (this script uses sudo when needed)."
elif ! command -v sudo >/dev/null 2>&1; then
  die "sudo is required but not found. Please install/configure sudo and re-run."
fi

# ── Steps ─────────────────────────────────────────────────────────────────────
ensure_tools() {
  say "Ensuring core tools (curl, git, gpg, apt transport/certs)…"
  local core_pkgs=(curl git gnupg apt-transport-https ca-certificates)
  sudo apt-get update -o=Dpkg::Use-Pty=0
  sudo DEBIAN_FRONTEND=noninteractive apt-get install -y "${core_pkgs[@]}"
}

clone_repo() {
  say "Cloning/updating repo → $CHECKOUT_DIR…"
  if [[ -d "$CHECKOUT_DIR/.git" ]]; then
    git -C "$CHECKOUT_DIR" fetch --all --prune
    git -C "$CHECKOUT_DIR" pull --ff-only || true
  else
    mkdir -p "$CHECKOUT_DIR"
    compgen -G "$HOME/.ssh/id_*" >/dev/null 2>&1 && ssh-add || true
    git clone "$REPO_SSH" "$CHECKOUT_DIR" || git clone "$REPO_HTTPS" "$CHECKOUT_DIR"
  fi
}

setup_vscode_repo() {
  say "Configuring official VS Code APT repository…"
  sudo install -m 0755 -d /usr/share/keyrings
  curl -fsSL "$VSCODE_KEY_URL" | gpg --dearmor | sudo tee "$VSCODE_GPG" >/dev/null
  sudo chmod 0644 "$VSCODE_GPG"
  echo "deb [arch=amd64,arm64,armhf signed-by=${VSCODE_GPG}] ${VSCODE_REPO_URI} stable main" \
    | sudo tee "$VSCODE_LIST" >/dev/null
}

setup_steam_repo() {
  say "Configuring official Steam APT repository…"
  sudo dpkg --add-architecture i386 || true
  sudo install -m 0755 -d /usr/share/keyrings
  curl -fsSL "$STEAM_KEY_URL" | gpg --dearmor | sudo tee "$STEAM_GPG" >/dev/null
  sudo chmod 0644 "$STEAM_GPG"
  echo "deb [arch=amd64,i386 signed-by=${STEAM_GPG}] ${STEAM_REPO_URI} stable steam" \
    | sudo tee "$STEAM_LIST" >/dev/null
}

apt_install_all() {
  # Repo setup lives here so ordering is enforced before the install.
  setup_vscode_repo
  setup_steam_repo

  say "APT refresh & installing packages…"
  sudo apt-get update -o=Dpkg::Use-Pty=0
  sudo DEBIAN_FRONTEND=noninteractive apt-get install -y "${APT_PKGS[@]}"
}

snap_install_all() {
  say "Installing Snaps and connecting useful interfaces…"
  if ! command -v snap >/dev/null 2>&1; then
    warn "snapd not found; skipping…"
    return 0
  fi

  for pkg in "${SNAP_PKGS[@]}"; do
    # Specifically allow string expansion to permit command-line arguments.
    sudo snap install $pkg || true
  done

  # Helpful Discord plugs (idempotent; harmless if already connected or unsupported)
  for pair in \
    "discord:system-observe :system-observe" \
    "discord:removable-media :removable-media" \
    "discord:camera :camera" \
    "discord:audio-record :audio-record"
  do
    sudo snap connect $pair || true
  done
}

load_gnome_dconf() {
  local dfile="$CHECKOUT_DIR/gnome.dconf"
  if [[ -f "$dfile" ]]; then
    say "Loading GNOME settings from gnome.dconf…"
    dconf load / < "$dfile"
  else
    warn "No gnome.dconf found. To capture your current GNOME settings for next time:"
    printf "  dconf dump / > %q\n" "$dfile"
  fi
}

symlink_dotfiles() {
  say "Symlinking dotfiles from repo…"
  local dfdir="$CHECKOUT_DIR/dotfiles"
  if [[ ! -d "$dfdir" ]]; then
    warn "No dotfiles directory yet. Create $dfdir and re-run."
    return 0
  fi

  # Special case for directories that should be symlinked wholesale.
  for dir in "${SYMLINK_DIRS[@]}"; do
    if [[ -e "$dfdir/$dir" && ! -e "$HOME/$dir" ]]; then
      mkdir -p "$(dirname "$HOME/$dir")"
      ln -s "$dfdir/$dir" "$HOME/$dir"
    fi
  done

  while IFS= read -r -d '' file; do
    local rel="${file#$dfdir/}"
    local dest="$HOME/$rel"
    mkdir -p "$(dirname "$dest")"

    if [[ "$(readlink -f "$file")" = "$(readlink -f "$dest")" ]]; then
      continue
    elif [[ -e "$dest" && ! -L "$dest" ]]; then
      say "Backing up existing $dest → $dest.bak.$(date +%s)"
      mv "$dest" "$dest.bak.$(date +%s)"
    fi

    ln -sf "$file" "$dest"
    say "Linked: $dest → $file"
  done < <(find "$dfdir" -type f -print0)
}

post_notes() {
  cat <<'EOF'

────────────────────────────────────────────────────────
Post-run notes:
  • GNOME settings → capture & commit:
      dconf dump / > ~/.ubuntu-up/gnome.dconf
      (then: git -C ~/.ubuntu-up add gnome.dconf && git -C ~/.ubuntu-up commit -m "GNOME baseline")
  • Put dotfiles into:  ~/.ubuntu-up/dotfiles/
      (this script symlinks them to $HOME; Emacs: whole site-lisp directory symlinked)
  • Dropbox: launch once to let it fetch its proprietary daemon.
  • Steam: first run completes runtime bits; i386 arch already enabled.
  • Re-run this script any time after you add config to the repo.
EOF
}

main() {
  ensure_tools
  clone_repo
  apt_install_all
  snap_install_all
  load_gnome_dconf
  symlink_dotfiles
  say "All done. 🧰 Fresh Ubuntu is ready to roll."
}

main "$@"
