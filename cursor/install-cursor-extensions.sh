#!/bin/bash

# Installs the Cursor extensions listed in extensions.txt & reports any that are
# installed locally but missing from that list. Don't uninstall anything, just
# report.
#
# Regenerate the list after installing something you want to keep with:
#   cursor --list-extensions > cursor/extensions.txt
#
# Run by dotbot from install.conf.yaml. Safe to re-run.

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
list_file="$script_dir/extensions.txt"

if ! command -v cursor >/dev/null; then
    echo "install-cursor-extensions: cursor CLI not found, skipping."
    exit 0
fi

if [[ ! -f $list_file ]]; then
    echo "install-cursor-extensions: no $list_file, skipping."
    exit 0
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

# `|| true` so an empty result doesn't trip `set -e`.
{ grep -vE '^[[:space:]]*(#|$)' "$list_file" || true; } | sort -u >"$tmp_dir/tracked"
cursor --list-extensions | sort -u >"$tmp_dir/installed"

comm -13 "$tmp_dir/tracked" "$tmp_dir/installed" >"$tmp_dir/untracked"
comm -23 "$tmp_dir/tracked" "$tmp_dir/installed" >"$tmp_dir/missing"

if [[ -s $tmp_dir/untracked ]]; then
    echo "install-cursor-extensions: installed but not in extensions.txt:"
    sed 's/^/  /' "$tmp_dir/untracked"
    echo "Add them to the list or uninstall them. Nothing was removed."
    echo
fi

if [[ ! -s $tmp_dir/missing ]]; then
    echo "install-cursor-extensions: all tracked extensions are installed."
    exit 0
fi

echo "install-cursor-extensions: in extensions.txt but not installed:"
sed 's/^/  /' "$tmp_dir/missing"

# dotbot runs this without a terminal, so don't install unattended there.
if [[ ! -t 0 ]]; then
    echo "Not an interactive terminal; run ./cursor/install-cursor-extensions.sh to install."
    exit 0
fi

read -r -p "Install these? [y/N]: " reply || reply=""
if [[ $reply != [Yy] ]]; then
    echo "Skipped."
    exit 0
fi

while read -r ext; do
    echo "  installing $ext"
    cursor --install-extension "$ext" >/dev/null || echo "  FAILED: $ext"
done <"$tmp_dir/missing"
