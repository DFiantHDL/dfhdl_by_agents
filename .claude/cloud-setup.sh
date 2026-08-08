#!/bin/bash
# Reference copy of the Claude Code cloud environment setup script.
#
# NOT executed by this repository. The authoritative copy is pasted into the
# environment's configuration at claude.ai/code (the "Setup script" field).
# This file exists so the script is reviewable and version controlled; keep the
# two in sync by hand.
#
# Why the split between this and .claude/hooks/session-start.sh:
#
#   Setup script  runs once, before Claude Code launches. Anthropic then
#                 snapshots the filesystem and reuses it, so later sessions
#                 skip this entirely. Everything it writes to disk persists.
#                 It re-runs when the script or the allowed hosts change, and
#                 after the cache expires (roughly seven days). Budget: keep
#                 the total under about five minutes or the cache cannot build.
#
#   SessionStart  runs every session, including resumes. The snapshot keeps
#   hook          files but no running processes, so daemons live there.
#
# Measured on a 4 core / 15 GiB cloud VM: `sbt update` takes about 48 seconds
# from cold and leaves roughly 280 MB across ~/.cache/coursier and ~/.sbt.

set -euo pipefail

sudo apt-get update
sudo apt-get install -y openjdk-17-jdk-headless

# Match project/build.properties. The launcher downloads the pinned version
# regardless, so installing anything else just costs an extra download.
SBT_VERSION=1.12.14
curl -fL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" -o /tmp/sbt.tgz
mkdir -p ~/.local/opt ~/.local/bin ~/.local/lib
tar -xzf /tmp/sbt.tgz -C ~/.local/opt/
ln -sf ~/.local/opt/sbt/bin/sbt ~/.local/bin/sbt
ln -sf ~/.local/opt/sbt/bin/sbtn-x86_64-pc-linux ~/.local/bin/sbtn

# Warm the dependency cache into the snapshot so the first sbt invocation of
# every later session does not re-resolve the whole Scala toolchain.
# Failure here degrades to a cold cache rather than failing the environment.
if [ -f build.sbt ]; then
  sbt -batch update Test/update || echo "setup: dependency warm-up failed, continuing with a cold cache" >&2
fi

# scalafmt, which CLAUDE.md requires before committing but which ships with
# neither an sbt plugin nor a binary in the cloud image.
#
# Use the coursier JAR, never the native `cs` launcher. The native launcher is
# a GraalVM image with its truststore baked in at build time: it ignores the
# system CA store and -D overrides alike, so it cannot be taught the sandbox
# egress CA and fails every HTTPS fetch with a PKIX error. The JAR runs on the
# JDK and picks up the JVM truststore, so it just works.
#
# This step needs the JVM to trust the egress CA, which the environment
# normally arranges through JAVA_TOOL_OPTIONS. If it ever fails with a PKIX
# error during setup, import the CA into the JDK cacerts with keytool first.
curl -fL -o ~/.local/lib/coursier.jar \
  "https://github.com/coursier/coursier/releases/download/v2.1.24/coursier.jar" 2>/dev/null || true
if [ -f ~/.local/lib/coursier.jar ]; then
  java -jar ~/.local/lib/coursier.jar install scalafmt ||
    echo "setup: scalafmt install failed, format manually" >&2
  ln -sf ~/.local/share/coursier/bin/scalafmt ~/.local/bin/scalafmt 2>/dev/null || true
  # scalafmt resolves the version pinned in .scalafmt.conf at runtime, so
  # --version alone caches the wrong artifacts. Run it against a real file to
  # pull the pinned version into the snapshot.
  if [ -f .scalafmt.conf ]; then
    first_scala=$(find . -name '*.scala' -not -path './target/*' -print -quit 2>/dev/null)
    [ -n "$first_scala" ] && ~/.local/bin/scalafmt --test "$first_scala" >/dev/null 2>&1 || true
  fi
fi

# Export the interception CAs to a stable path, so a Dockerfile can COPY them
# in and run update-ca-certificates. Container builds reach the network through
# a TLS intercepting egress gateway that the stock base images do not trust,
# which otherwise fails every HTTPS fetch in the build with a PKIX error.
#
# Source these from /usr/local/share/ca-certificates rather than from
# $SSL_CERT_FILE. That directory is part of the base image and is therefore
# always populated, including while this script runs. $SSL_CERT_FILE belongs to
# the per-session proxy layer, which is provisioned at the end of container
# bring-up and so may not exist yet at setup time.
#
# Public certificates only, no secrets. The loop writes nothing on a machine
# with no interception, which is the normal case outside the sandbox.
mkdir -p ~/.local/share/extra-ca
rm -f ~/.local/share/extra-ca/*.crt
for cert in /usr/local/share/ca-certificates/*.crt; do
  [ -f "$cert" ] && cp "$cert" ~/.local/share/extra-ca/
done
