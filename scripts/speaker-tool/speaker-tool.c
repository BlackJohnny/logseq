/* speaker-tool: speaker diarization and voice embeddings on top of the sherpa-onnx
 * C API, used by Logseq's meeting notes. Built by scripts/build-speaker-tools.sh.
 * (A native helper instead of the sherpa-onnx-node addon: Electron forbids the
 * external buffers that addon returns for embeddings.)
 *
 *   speaker-tool diarize --wav f.wav --segmentation model.onnx --embedding model.onnx
 *                        [--num-speakers N] [--threshold T] [--threads N]
 *   speaker-tool embed   --wav f.wav --embedding model.onnx [--start S] [--end E] [--threads N]
 *
 * Input: 16 kHz mono WAV. Output: one JSON document on stdout; progress on stderr. */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sherpa-onnx/c-api/c-api.h"

#define MAX_EMBED_SECONDS 40.0f
#define MIN_SEGMENT_SECONDS 0.8f

static const char *arg(int argc, char **argv, const char *name, const char *def) {
  for (int i = 2; i < argc - 1; i++)
    if (strcmp(argv[i], name) == 0) return argv[i + 1];
  return def;
}

static void print_embedding(const float *v, int dim) {
  printf("[");
  for (int i = 0; i < dim; i++) printf(i ? ",%.6g" : "%.6g", v[i]);
  printf("]");
}

/* Embedding of samples[0..n); returns a malloc'ed vector of `dim` floats or NULL. */
static float *embed(const SherpaOnnxSpeakerEmbeddingExtractor *ex, int sr, const float *samples, int n) {
  const SherpaOnnxOnlineStream *s = SherpaOnnxSpeakerEmbeddingExtractorCreateStream(ex);
  SherpaOnnxOnlineStreamAcceptWaveform(s, sr, samples, n);
  SherpaOnnxOnlineStreamInputFinished(s);
  float *out = NULL;
  if (SherpaOnnxSpeakerEmbeddingExtractorIsReady(ex, s)) {
    const float *v = SherpaOnnxSpeakerEmbeddingExtractorComputeEmbedding(ex, s);
    int dim = SherpaOnnxSpeakerEmbeddingExtractorDim(ex);
    out = malloc(sizeof(float) * dim);
    memcpy(out, v, sizeof(float) * dim);
    SherpaOnnxSpeakerEmbeddingExtractorDestroyEmbedding(v);
  }
  SherpaOnnxDestroyOnlineStream(s);
  return out;
}

static const SherpaOnnxSpeakerEmbeddingExtractor *make_extractor(const char *model, int threads) {
  SherpaOnnxSpeakerEmbeddingExtractorConfig c;
  memset(&c, 0, sizeof(c));
  c.model = model;
  c.num_threads = threads;
  c.provider = "cpu";
  return SherpaOnnxCreateSpeakerEmbeddingExtractor(&c);
}

static int on_progress(int32_t done, int32_t total, void *unused) {
  (void)unused;
  fprintf(stderr, "progress %d\n", total > 0 ? (int)(100.0 * done / total) : 0);
  return 0;
}

static int cmp_dur_desc(const void *a, const void *b) {
  const SherpaOnnxOfflineSpeakerDiarizationSegment *x = a, *y = b;
  float dx = x->end - x->start, dy = y->end - y->start;
  return (dy > dx) - (dy < dx);
}

static int run_embed(int argc, char **argv) {
  const char *wav = arg(argc, argv, "--wav", NULL), *model = arg(argc, argv, "--embedding", NULL);
  if (!wav || !model) { fprintf(stderr, "embed: --wav and --embedding are required\n"); return 2; }
  const SherpaOnnxWave *w = SherpaOnnxReadWave(wav);
  if (!w) { fprintf(stderr, "cannot read %s\n", wav); return 1; }
  const SherpaOnnxSpeakerEmbeddingExtractor *ex = make_extractor(model, atoi(arg(argc, argv, "--threads", "2")));
  if (!ex) { fprintf(stderr, "cannot load embedding model\n"); return 1; }
  int from = (int)(atof(arg(argc, argv, "--start", "0")) * w->sample_rate);
  int to = w->num_samples;
  const char *end = arg(argc, argv, "--end", NULL);
  if (end && (int)(atof(end) * w->sample_rate) < to) to = (int)(atof(end) * w->sample_rate);
  if (from < 0) from = 0;
  if (to <= from) { fprintf(stderr, "empty range\n"); return 1; }
  float *v = embed(ex, w->sample_rate, w->samples + from, to - from);
  if (!v) { fprintf(stderr, "audio too short for an embedding\n"); return 1; }
  int dim = SherpaOnnxSpeakerEmbeddingExtractorDim(ex);
  printf("{\"dim\":%d,\"seconds\":%.2f,\"embedding\":", dim, (double)(to - from) / w->sample_rate);
  print_embedding(v, dim);
  printf("}\n");
  free(v);
  SherpaOnnxDestroySpeakerEmbeddingExtractor(ex);
  SherpaOnnxFreeWave(w);
  return 0;
}

static int run_diarize(int argc, char **argv) {
  const char *wav = arg(argc, argv, "--wav", NULL), *seg = arg(argc, argv, "--segmentation", NULL),
             *emb = arg(argc, argv, "--embedding", NULL);
  if (!wav || !seg || !emb) { fprintf(stderr, "diarize: --wav, --segmentation, --embedding are required\n"); return 2; }
  int threads = atoi(arg(argc, argv, "--threads", "4"));

  SherpaOnnxOfflineSpeakerDiarizationConfig cfg;
  memset(&cfg, 0, sizeof(cfg));
  cfg.segmentation.pyannote.model = seg;
  cfg.segmentation.num_threads = threads;
  cfg.segmentation.provider = "cpu";
  cfg.embedding.model = emb;
  cfg.embedding.num_threads = threads;
  cfg.embedding.provider = "cpu";
  cfg.clustering.num_clusters = atoi(arg(argc, argv, "--num-speakers", "-1"));
  cfg.clustering.threshold = (float)atof(arg(argc, argv, "--threshold", "0.5"));
  cfg.min_duration_on = 0.3f;
  cfg.min_duration_off = 0.5f;

  const SherpaOnnxWave *w = SherpaOnnxReadWave(wav);
  if (!w) { fprintf(stderr, "cannot read %s\n", wav); return 1; }
  const SherpaOnnxOfflineSpeakerDiarization *sd = SherpaOnnxCreateOfflineSpeakerDiarization(&cfg);
  if (!sd) { fprintf(stderr, "cannot create the diarizer (check the model paths)\n"); return 1; }
  if (w->sample_rate != SherpaOnnxOfflineSpeakerDiarizationGetSampleRate(sd)) {
    fprintf(stderr, "expected %d Hz audio, got %d Hz\n", SherpaOnnxOfflineSpeakerDiarizationGetSampleRate(sd), w->sample_rate);
    return 1;
  }
  const SherpaOnnxOfflineSpeakerDiarizationResult *r =
      SherpaOnnxOfflineSpeakerDiarizationProcessWithCallback(sd, w->samples, w->num_samples, on_progress, NULL);
  int n = SherpaOnnxOfflineSpeakerDiarizationResultGetNumSegments(r);
  int nspk = SherpaOnnxOfflineSpeakerDiarizationResultGetNumSpeakers(r);
  const SherpaOnnxOfflineSpeakerDiarizationSegment *segs = SherpaOnnxOfflineSpeakerDiarizationResultSortByStartTime(r);

  const SherpaOnnxSpeakerEmbeddingExtractor *ex = make_extractor(emb, threads);
  int dim = ex ? SherpaOnnxSpeakerEmbeddingExtractorDim(ex) : 0;

  printf("{\"sampleRate\":%d,\"dim\":%d,\"segments\":[", w->sample_rate, dim);
  for (int i = 0; i < n; i++)
    printf("%s{\"start\":%.3f,\"end\":%.3f,\"speaker\":%d}", i ? "," : "", segs[i].start, segs[i].end, segs[i].speaker);
  printf("],\"speakers\":[");

  /* One embedding per speaker, from their longest segments (at most MAX_EMBED_SECONDS). */
  int first = 1;
  for (int spk = 0; spk < nspk; spk++) {
    SherpaOnnxOfflineSpeakerDiarizationSegment *mine = malloc(sizeof(*mine) * (n ? n : 1));
    int m = 0;
    float total = 0;
    for (int i = 0; i < n; i++)
      if (segs[i].speaker == spk) { mine[m++] = segs[i]; total += segs[i].end - segs[i].start; }
    if (!m) { free(mine); continue; }
    qsort(mine, m, sizeof(*mine), cmp_dur_desc);
    float *buf = malloc(sizeof(float) * (size_t)(MAX_EMBED_SECONDS * w->sample_rate + w->sample_rate));
    size_t len = 0;
    for (int i = 0; i < m; i++) {
      float dur = mine[i].end - mine[i].start;
      if (dur < MIN_SEGMENT_SECONDS && len > 0) continue;
      int a = (int)(mine[i].start * w->sample_rate), b = (int)(mine[i].end * w->sample_rate);
      if (b > w->num_samples) b = w->num_samples;
      if (b <= a) continue;
      if ((len + (b - a)) > (size_t)(MAX_EMBED_SECONDS * w->sample_rate)) b = a + (int)((size_t)(MAX_EMBED_SECONDS * w->sample_rate) - len);
      memcpy(buf + len, w->samples + a, sizeof(float) * (b - a));
      len += b - a;
      if (len >= (size_t)(MAX_EMBED_SECONDS * w->sample_rate)) break;
    }
    float *v = ex && len ? embed(ex, w->sample_rate, buf, (int)len) : NULL;
    printf("%s{\"id\":%d,\"seconds\":%.2f,\"embedding\":", first ? "" : ",", spk, total);
    if (v) print_embedding(v, dim); else printf("null");
    printf("}");
    first = 0;
    free(v); free(buf); free(mine);
  }
  printf("]}\n");

  SherpaOnnxOfflineSpeakerDiarizationDestroySegment(segs);
  SherpaOnnxOfflineSpeakerDiarizationDestroyResult(r);
  SherpaOnnxDestroyOfflineSpeakerDiarization(sd);
  if (ex) SherpaOnnxDestroySpeakerEmbeddingExtractor(ex);
  SherpaOnnxFreeWave(w);
  return 0;
}

int main(int argc, char **argv) {
  if (argc >= 2 && strcmp(argv[1], "diarize") == 0) return run_diarize(argc, argv);
  if (argc >= 2 && strcmp(argv[1], "embed") == 0) return run_embed(argc, argv);
  fprintf(stderr, "usage: speaker-tool diarize|embed ... (see the header of speaker-tool.c)\n");
  return 2;
}
