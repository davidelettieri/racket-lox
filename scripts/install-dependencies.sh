#!/usr/bin/env bash
set -euo pipefail

# Ensure dart is on PATH for this session
export PATH="$PATH:/usr/lib/dart/bin"

# Check if Racket is already installed
if command -v racket &> /dev/null; then
	echo "Racket is already installed: $(racket --version)"
else
	echo "Racket not found, installing…"
	TEMP_INSTALLER=$(mktemp /tmp/racket-installer.XXXXXX.sh)
	curl -L -o "$TEMP_INSTALLER" https://download.racket-lang.org/installers/9.0/racket-9.0-x86_64-linux-buster-cs.sh
	bash "$TEMP_INSTALLER" --dest /usr/racket
	rm "$TEMP_INSTALLER"
fi

# Install local dependencies
raco pkg install --no-docs
