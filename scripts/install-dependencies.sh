#!/usr/bin/env bash
set -euo pipefail

# Ensure dart is on PATH for this session
export PATH="$PATH:/usr/lib/dart/bin"

# Check if Racket is already installed
if command -v racket &> /dev/null; then
	echo "Racket is already installed: $(racket --version)"
else
	echo "Racket not found, installing…"
	curl -L -o /tmp/racket-installer.sh https://download.racket-lang.org/installers/9.0/racket-9.0-x86_64-linux-buster-cs.sh
	bash /tmp/racket-installer.sh --dest /usr/racket
	rm /tmp/racket-installer.sh
fi

# Install Racket packages
if ! raco pkg show racket-langserver &> /dev/null; then
	echo "Installing racket-langserver…"
	raco pkg install racket-langserver --auto
else
	echo "racket-langserver is already installed"
fi

# Install local dependencies
raco pkg install 