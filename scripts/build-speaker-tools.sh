#!/usr/bin/env bash
# Build speaker-tool (diarization + voice embeddings for meeting notes, Linux): a small
# C helper linked against the prebuilt sherpa-onnx shared libraries.
# Output: .whisper-build/dist/speakers/{bin/speaker-tool,lib/*.so}
# Env:  SHERPA_VERSION=1.13.8   sherpa-onnx release to link against
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SHERPA_VERSION="${SHERPA_VERSION:-1.13.8}"
NAME="sherpa-onnx-v${SHERPA_VERSION}-linux-x64-shared-no-tts"
WORK="$ROOT/.whisper-build/sherpa"
OUT="$ROOT/.whisper-build/dist/speakers"

for tool in curl tar gcc; do
  command -v "$tool" >/dev/null || { echo "missing required tool: $tool"; exit 1; }
done

mkdir -p "$WORK" "$OUT/bin" "$OUT/lib"
if [ ! -d "$WORK/$NAME" ]; then
  echo ">> downloading $NAME"
  curl -fsSL -o "$WORK/$NAME.tar.bz2" "https://github.com/k2-fsa/sherpa-onnx/releases/download/v${SHERPA_VERSION}/$NAME.tar.bz2"
  tar xjf "$WORK/$NAME.tar.bz2" -C "$WORK"
fi

echo ">> compiling speaker-tool"
gcc -O2 -Wall -o "$OUT/bin/speaker-tool" "$ROOT/scripts/speaker-tool/speaker-tool.c" \
  -I"$WORK/$NAME/include" -L"$WORK/$NAME/lib" -lsherpa-onnx-c-api -lonnxruntime -lm \
  -Wl,-rpath,'$ORIGIN/../lib'
# only the libraries the helper needs (the C API wraps the rest)
cp -f "$WORK/$NAME/lib/libsherpa-onnx-c-api.so" "$WORK/$NAME/lib/libonnxruntime.so" "$OUT/lib/"

# Third-party licenses shipped with the binaries (see resources/forge.config.js)
curl -fsSL -o "$OUT/LICENSE-sherpa-onnx" "https://raw.githubusercontent.com/k2-fsa/sherpa-onnx/v${SHERPA_VERSION}/LICENSE"
curl -fsSL -o "$OUT/LICENSE-onnxruntime" "https://raw.githubusercontent.com/microsoft/onnxruntime/main/LICENSE"
cat > "$OUT/NOTICE.txt" <<NOTICE
Speaker identification in Logseq uses sherpa-onnx v${SHERPA_VERSION} (Apache License 2.0,
LICENSE-sherpa-onnx, https://github.com/k2-fsa/sherpa-onnx) and ONNX Runtime (MIT License,
LICENSE-onnxruntime, https://github.com/microsoft/onnxruntime).
The models downloaded on demand: pyannote segmentation 3.0 (MIT License) and
WeSpeaker ResNet34 (Apache License 2.0).
NOTICE

echo
echo ">> done:"
ls -lh "$OUT/bin" "$OUT/lib"
