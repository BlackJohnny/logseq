#!/usr/bin/env bash
# Full desktop (Electron, Linux) build: JS deps -> CSS/JS -> CLJS release -> packaging.
# Output: static/out/make/*.AppImage and static/out/make/zip/linux/x64/*.zip
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

# --- toolchain -------------------------------------------------------------
JDK="${JAVA_HOME:-/usr/lib/jvm/java-17-openjdk-amd64}"   # CI uses JDK 17
[ -x "$JDK/bin/java" ] || { echo "JDK 17 not found at $JDK (set JAVA_HOME)"; exit 1; }
export JAVA_HOME="$JDK"
export PATH="$JAVA_HOME/bin:$HOME/.local/bin:$ROOT/node_modules/.bin:$PATH"

if ! command -v yarn >/dev/null; then
  echo ">> enabling yarn 1.x via corepack"
  mkdir -p "$HOME/.local/bin"
  corepack enable --install-directory "$HOME/.local/bin"
  corepack prepare yarn@1.22.22 --activate
fi

# Debian's node-gyp lacks the python 'gyp' module; prefer an npm-installed one if present.
GYP="$HOME/.local/gyp/node_modules/node-gyp/bin/node-gyp.js"
[ -f "$GYP" ] && export npm_config_node_gyp="$GYP"

# --- build -----------------------------------------------------------------
echo ">> [1/4] yarn install (root)"
yarn install

echo ">> [2/4] gulp build (css, resources)"
yarn gulp:build

echo ">> [3/4] CLJS release (app + electron + publishing)"
yarn cljs:release-electron

echo ">> [4/4] package electron app"
cd "$ROOT/static"
# electron-deeplink's native build can fail without python3-gyp; packaging still works,
# but the logseq-og:// protocol handler may be affected. Install python3-gyp to fix.
yarn install || echo "!! static yarn install reported errors (see above); continuing"
yarn electron:make

echo
echo ">> done:"
find "$ROOT/static/out/make" -maxdepth 4 \( -name '*.AppImage' -o -name '*.zip' \) -print
