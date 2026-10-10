#!/usr/bin/env bash
# Build whisper.cpp (dictation backend, Linux) from source: a CPU binary always,
# plus a CUDA binary when the CUDA toolkit (nvcc) is available.
# Output: .whisper-build/dist/whisper/bin/{whisper-cli,whisper-server}-{cpu,cuda}
#   (kept out of resources/, which gulp copies wholesale into the app)
# Env:  WHISPER_REF=<tag|commit>   whisper.cpp version to build (default: pinned below)
#       CUDA_ARCHS="75;86;89"      CUDA architectures (default: Turing, Ampere, Ada)
#       WITH_CUDA=0|1              force-disable / require the CUDA build (default: auto)
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WHISPER_REF="${WHISPER_REF:-v1.9.5}"
CUDA_ARCHS="${CUDA_ARCHS:-75;86;89}"
WITH_CUDA="${WITH_CUDA:-auto}"

SRC="$ROOT/.whisper-build/whisper.cpp"
OUT="$ROOT/.whisper-build/dist/whisper/bin"
JOBS="$(nproc)"

for tool in git cmake g++; do
  command -v "$tool" >/dev/null || { echo "missing required tool: $tool"; exit 1; }
done

# --- source ----------------------------------------------------------------
if [ ! -d "$SRC/.git" ]; then
  echo ">> cloning whisper.cpp"
  mkdir -p "$(dirname "$SRC")"
  git clone https://github.com/ggml-org/whisper.cpp.git "$SRC"
fi
echo ">> checking out $WHISPER_REF"
git -C "$SRC" fetch --tags --quiet
git -C "$SRC" checkout --quiet "$WHISPER_REF"

mkdir -p "$OUT"

build() { # <name> <extra cmake args...>  (builds whisper-cli and whisper-server)
  local name="$1"; shift
  local bdir="$SRC/build-$name"
  echo ">> [$name] configure"
  cmake -S "$SRC" -B "$bdir" -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF \
    -DWHISPER_BUILD_TESTS=OFF -DWHISPER_BUILD_SERVER=ON "$@"
  echo ">> [$name] build"
  cmake --build "$bdir" --config Release -j "$JOBS" --target whisper-cli whisper-server
  install -m 0755 "$bdir/bin/whisper-cli" "$OUT/whisper-cli-$name"
  install -m 0755 "$bdir/bin/whisper-server" "$OUT/whisper-server-$name"
}

# --- CPU -------------------------------------------------------------------
# GGML_NATIVE=OFF so the binary runs on other machines, not only this CPU.
build cpu -DGGML_NATIVE=OFF -DGGML_AVX2=ON -DGGML_FMA=ON -DGGML_F16C=ON

# --- CUDA (optional) -------------------------------------------------------
NVCC="$(command -v nvcc || true)"
# Prefer the newest side-by-side toolkit (/usr/local/cuda-X.Y) over the /usr/local/cuda symlink.
for d in $(ls -d /usr/local/cuda-[0-9]* 2>/dev/null | sort -rV); do
  [ -x "$d/bin/nvcc" ] && { NVCC="$d/bin/nvcc"; break; }
done
[ -z "$NVCC" ] && [ -x /usr/local/cuda/bin/nvcc ] && NVCC=/usr/local/cuda/bin/nvcc
[ -n "${CUDA_HOME:-}" ] && [ -x "$CUDA_HOME/bin/nvcc" ] && NVCC="$CUDA_HOME/bin/nvcc"   # explicit override wins

if [ "$WITH_CUDA" = "0" ]; then
  echo ">> skipping CUDA build (WITH_CUDA=0)"
elif [ -n "$NVCC" ]; then
  # CUDA 12.x rejects host compilers newer than GCC 14, so pick an older g++ for it.
  CUDA_HOST_CXX="${CUDA_HOST_CXX:-}"
  if [ -z "$CUDA_HOST_CXX" ]; then
    for v in 14 13 12; do
      command -v "g++-$v" >/dev/null && { CUDA_HOST_CXX="$(command -v "g++-$v")"; break; }
    done
  fi
  if [ -z "$CUDA_HOST_CXX" ]; then
    MSG="no CUDA-compatible host compiler (g++-12..14). Install one (e.g. 'sudo apt install g++-14') or set CUDA_HOST_CXX."
    [ "$WITH_CUDA" = "1" ] && { echo "$MSG"; exit 1; }
    echo ">> skipping CUDA build: $MSG"
  else
    echo ">> CUDA host compiler: $CUDA_HOST_CXX"
    build cuda -DGGML_CUDA=ON -DCMAKE_CUDA_COMPILER="$NVCC" -DCMAKE_CUDA_HOST_COMPILER="$CUDA_HOST_CXX" \
      -DCMAKE_CUDA_ARCHITECTURES="$CUDA_ARCHS" -DGGML_NATIVE=OFF -DGGML_AVX2=ON -DGGML_FMA=ON -DGGML_F16C=ON
  fi
elif [ "$WITH_CUDA" = "1" ]; then
  echo "WITH_CUDA=1 but nvcc was not found"; exit 1
else
  echo ">> nvcc not found; skipping CUDA build (CPU only)"
fi

echo
echo ">> done:"
ls -lh "$OUT"
