Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Show-Usage {
    Write-Output 'Usage: ./run-tests.ps1'
    Write-Output ''
    Write-Output 'Options:'
    Write-Output '  -h, --help    Show this help message'
    Write-Output ''
    Write-Output 'Environment:'
    Write-Output '  CONTAINER_CLI Optional runtime override (docker or podman)'
}

if ($args.Count -gt 0) {
    foreach ($arg in $args) {
        switch ($arg) {
            '-h' {
                Show-Usage
                exit 0
            }
            '--help' {
                Show-Usage
                exit 0
            }
            default {
                Write-Error "Unknown argument: $arg"
                Show-Usage
                exit 1
            }
        }
    }
}

$containerCli = $env:CONTAINER_CLI
if ([string]::IsNullOrWhiteSpace($containerCli)) {
    if (Get-Command podman -ErrorAction SilentlyContinue) {
        $containerCli = 'podman'
    }
    elseif (Get-Command docker -ErrorAction SilentlyContinue) {
        $containerCli = 'docker'
    }
    else {
        Write-Error 'No container runtime found. Install podman or docker.'
        exit 1
    }
}

if (-not (Get-Command $containerCli -ErrorAction SilentlyContinue)) {
    Write-Error "Configured container runtime '$containerCli' is not available."
    exit 1
}

$rootDir = $PSScriptRoot
$imageName = 'racket-lox-test:latest'
$dockerfile = Join-Path $rootDir '.docker/test-runner.Dockerfile'

& $containerCli build `
    -f $dockerfile `
    -t $imageName `
    $rootDir
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$testCommand = @(
    'set -euo pipefail'
    'export PATH="/usr/racket/bin:/usr/lib/dart/bin:${PATH}"'
    'export PUB_CACHE="/opt/pub-cache"'
    'mkdir -p "$HOME"'
    'raco pkg install --auto --no-docs --link /workspace'
    'cd /opt/craftinginterpreters'
    'dart tool/bin/test.dart chap12_classes --interpreter racket'
) -join "`n"

$runArgs = @(
    'run', '--rm', '-t',
    '-e', 'HOME=/tmp/racket-lox-home',
    '-v', "${rootDir}:/workspace",
    '-v', '/workspace/compiled',
    '-v', '/workspace/lang/compiled',
    '-w', '/workspace'
)

if ($containerCli -eq 'podman') {
    $runArgs += @('--userns', 'keep-id')
}
elseif (-not $IsWindows) {
    $uid = (& id -u).Trim()
    $gid = (& id -g).Trim()
    $runArgs += @('--user', "$uid`:$gid")
}

$runArgs += @(
    $imageName,
    'bash', '-c', $testCommand
)

& $containerCli @runArgs
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
