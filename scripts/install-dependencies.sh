#!/usr/bin/env bash
set -euo pipefail

# Configure paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CACHE_DIR="$REPO_ROOT/.cache"
mkdir -p "$CACHE_DIR"

# Dart package details
DART_VERSION="2.19.6"
DART_DEB="dart_${DART_VERSION}-1_amd64.deb"
DART_URL="https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_VERSION}/linux_packages/${DART_DEB}"
DEB_PATH="$CACHE_DIR/$DART_DEB"

echo "Using cache directory: $CACHE_DIR"

# Download once into cache if not present
if [[ -f "$DEB_PATH" ]]; then
	echo "Dart package already cached: $DEB_PATH"
else
	echo "Downloading Dart package to cache…"
	wget -O "$DEB_PATH" "$DART_URL"
fi

# Install Dart from cached .deb
sudo dpkg -i "$DEB_PATH"

# Ensure dart is on PATH for this session
export PATH="$PATH:/usr/lib/dart/bin"

# Install Racket and packages
sudo apt update -y
sudo apt install -y racket
raco pkg install racket-langserver --auto
raco pkg install 