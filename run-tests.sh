#!/usr/bin/env bash
set -euo pipefail

usage() {
	echo "Usage: ./run-tests.sh"
	echo
	echo "Options:"
	echo "  -h, --help    Show this help message"
	echo
	echo "Environment:"
	echo "  CONTAINER_CLI Optional runtime override (docker or podman)"
}

while [[ $# -gt 0 ]]; do
	case "$1" in
		-h|--help)
			usage
			exit 0
			;;
		*)
			echo "Unknown argument: $1" >&2
			usage >&2
			exit 1
			;;
	esac
done

CONTAINER_CLI="${CONTAINER_CLI:-}"
if [[ -z "${CONTAINER_CLI}" ]]; then
	if command -v podman >/dev/null 2>&1; then
		CONTAINER_CLI="podman"
	elif command -v docker >/dev/null 2>&1; then
		CONTAINER_CLI="docker"
	else
		echo "No container runtime found. Install podman or docker." >&2
		exit 1
	fi
fi

if ! command -v "${CONTAINER_CLI}" >/dev/null 2>&1; then
	echo "Configured container runtime '${CONTAINER_CLI}' is not available." >&2
	exit 1
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMAGE_NAME="racket-lox-test:latest"
DOCKERFILE="${ROOT_DIR}/.docker/test-runner.Dockerfile"

"${CONTAINER_CLI}" build \
	-f "${DOCKERFILE}" \
	-t "${IMAGE_NAME}" \
	"${ROOT_DIR}"


RUN_ARGS=(
	run --rm -t
	-e HOME=/tmp/racket-lox-home
	-v "${ROOT_DIR}:/workspace"
	-w /workspace
	"${IMAGE_NAME}"
	bash -c 'set -euo pipefail
		export PATH="/usr/racket/bin:/usr/lib/dart/bin:${PATH}"
		export PUB_CACHE="/opt/pub-cache"
		mkdir -p "$HOME"
		raco pkg install --auto --no-docs --link /workspace
		cd /opt/craftinginterpreters
		dart tool/bin/test.dart chap12_classes --interpreter racket'
)

if [[ "${CONTAINER_CLI}" == "podman" ]]; then
	RUN_ARGS=(run --rm -t --userns keep-id "${RUN_ARGS[@]:3}")
else
	RUN_ARGS=(run --rm -t --user "$(id -u):$(id -g)" "${RUN_ARGS[@]:3}")
fi

"${CONTAINER_CLI}" "${RUN_ARGS[@]}"
