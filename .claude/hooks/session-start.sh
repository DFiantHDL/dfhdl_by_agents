#!/bin/bash
# SessionStart hook for Claude Code cloud sessions.
#
# The cloud environment cache is a filesystem snapshot: it keeps whatever the
# setup script writes to disk, but nothing that was merely running. Anything
# that has to be a live process therefore belongs here rather than in the
# setup script, which runs once and is then skipped for cached environments.
#
# Today that is the Docker daemon. The cloud image ships the Docker CLI but
# starts no daemon, so `docker` fails until dockerd is up. The DFHDL training
# workflow (dfhdl_training/scripts/init-learner.sh) needs it.
#
# This runs on every session start and resume, so it stays fast and idempotent,
# and it always exits 0: a hook failure must never block a session from
# starting.

set -uo pipefail

# Local terminal sessions manage their own Docker. Only act in the cloud.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

command -v dockerd >/dev/null 2>&1 || exit 0

# Already running (resumed session, or started by hand). Nothing to do.
docker info >/dev/null 2>&1 && exit 0

(dockerd >/tmp/dockerd.log 2>&1 &)

# Wait for the socket instead of leaving the first docker command to race it.
# Cold start measured at roughly 3 seconds; 15 is headroom, not an expectation.
for _ in $(seq 1 15); do
  docker info >/dev/null 2>&1 && break
  sleep 1
done

docker info >/dev/null 2>&1 ||
  echo "session-start: dockerd did not become ready, see /tmp/dockerd.log" >&2

exit 0
