#' Hardware tiers: recommended models and backend for a run
#'
#' The protocol is the same at every tier; only the models and the backend
#' change. In the validation corpus a small model (Gemma 4 E4B) agreed with
#' human codes at least as well as 27-35B models, so the local tier is a real
#' option, not a toy. Every tier uses first-token logprobs at temperature 0.
#'
#' @param tier `"local"` (laptop; Ollama or llama.cpp, quantized),
#'   `"workstation"` (one 24 GB GPU; vLLM or Ollama), or `"hpc"` (A40/A100
#'   on SLURM via [oc_slurm_job()], plus optional API models).
#' @return A list: `tier`, `models` (Hugging Face / Ollama names), `backend`
#'   (how to call them), and `notes`.
oc_profile <- function(tier = c("local", "workstation", "hpc")) {
  tier <- match.arg(tier)
  switch(tier,
    local = list(tier = tier,
      models = c(primary = "gemma4:e4b", alternative = "qwen3.5:9b"),
      backend = 'oc_backend_openai("http://127.0.0.1:11434/v1", "gemma4:e4b", extra = list(reasoning_effort = "none"))',
      notes = "Ollama (or llama.cpp server) on a laptop; about 4-8 GB RAM/VRAM. Run oc_classify() directly; no batch jobs needed for a few thousand responses. Thinking must be off (reasoning_effort = \"none\" on Ollama): with it on, the short answer comes back empty and the label is read from the wrong token."),
    workstation = list(tier = tier,
      models = c(primary = "google/gemma-4-12B-it", alternative = "Qwen/Qwen3.6-27B-FP8"),
      backend = 'oc_backend_openai("http://127.0.0.1:8000/v1", "google/gemma-4-12B-it")  # vLLM server',
      notes = "One 24 GB GPU. Start a vLLM OpenAI-compatible server, or run the offline runner (inst/python/oc_vllm_runner.py) on a request bundle."),
    hpc = list(tier = tier,
      models = c(primary = "Qwen/Qwen3.6-35B-A3B-FP8", second_family = "google/gemma-4-31B-it",
                 api = "Jev (TypeSafe) for pairwise; GPT-6 Luna gives hard labels only"),
      backend = "oc_export_requests() -> oc_slurm_job() -> oc_import_results()",
      notes = "A40 (48 GB) for FP8 checkpoints up to ~35B; A100-80G for bf16 31B. Use two model families and report agreement as reliability.")
  )
}
