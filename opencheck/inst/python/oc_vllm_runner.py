"""Offline vLLM runner for opencheck request bundles.

Reads a JSON-lines request bundle written by opencheck::oc_export_requests()
(one line per response: id, prompt_hash, system, user, codes) and appends one
result line per request to --out: id, prompt_hash, model, raw (the generated
text) and top (the first token's top alternatives with logprobs). Thinking is
disabled so the first generated token is the answer.

Resumable: requests already present in --out (same id and prompt_hash) are
skipped, and results are written after every shard. Standard library only; the
`stub` backend runs without vllm for laptop checks.

    python3 oc_vllm_runner.py --requests req.jsonl --out res.jsonl --backend stub --pilot 20
    python oc_vllm_runner.py --requests req.jsonl --out res.jsonl --model Qwen/Qwen3.8-27B-FP8
"""
import argparse
import hashlib
import json
import os
import math
import random
from pathlib import Path


def read_jsonl(path: Path) -> list:
    if not path.exists():
        return []
    return [json.loads(l) for l in path.read_text(encoding="utf-8").split("\n") if l.strip()]


def append_jsonl(path: Path, rows: list) -> None:
    with path.open("a", encoding="utf-8") as f:
        for r in rows:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")


def run_stub(shard: list) -> list:
    out = []
    for r in shard:
        h = hashlib.sha256(f"{r['id']}:{r['prompt_hash']}:stub".encode()).hexdigest()
        codes = r["codes"]
        k = int(h[:8], 16) % len(codes)
        p = 0.5 + 0.5 * int(h[8:16], 16) / 0xFFFFFFFF
        rest = (1 - p) / max(1, len(codes) - 1)
        top = [{"token": codes[k], "logprob": math.log(p)}] + [
            {"token": c, "logprob": math.log(rest)} for i, c in enumerate(codes) if i != k and rest > 0]
        out.append({"id": r["id"], "prompt_hash": r["prompt_hash"], "model": "stub",
                    "raw": codes[k], "top": top})
    return out


def load_vllm(model: str, max_len: int, top: int, seed: int, max_tokens: int = 1,
              quantization: str = "", enforce_eager: bool = False, tensor_parallel: int = 1):
    if tensor_parallel > 1:
        # PCIe GPUs without NVLink (e.g. 2x A40) hang in NCCL peer-to-peer setup
        # and in vLLM's custom all-reduce; route through host memory instead.
        os.environ.setdefault("NCCL_P2P_DISABLE", "1")
    from vllm import LLM, SamplingParams  # deferred so the stub runs without vllm
    kw = {"quantization": quantization} if quantization else {}
    if tensor_parallel > 1:
        kw["disable_custom_all_reduce"] = True
    if enforce_eager:
        kw["enforce_eager"] = True  # skip torch.compile (inductor fails on some new architectures)
    llm = LLM(model=model, dtype="auto", max_model_len=max_len, gpu_memory_utilization=0.90,
              tensor_parallel_size=tensor_parallel, enable_prefix_caching=True, max_num_seqs=64,
              limit_mm_per_prompt={"image": 0, "video": 0}, **kw)
    # Generation mode (max_tokens > 1) keeps no logprobs; the answer is the text.
    sp = SamplingParams(temperature=0, max_tokens=max_tokens,
                        logprobs=top if max_tokens == 1 else None, seed=seed)
    return llm, llm.get_tokenizer(), sp


def run_vllm(llm, tok, sp, shard: list, model: str, max_len: int) -> list:
    texts, keep, out = [], [], []
    for r in shard:
        msgs = [{"role": "system", "content": r["system"]}, {"role": "user", "content": r["user"]}]
        try:
            text = tok.apply_chat_template(msgs, tokenize=False, add_generation_prompt=True,
                                           enable_thinking=False)
        except TypeError:
            text = tok.apply_chat_template(msgs, tokenize=False, add_generation_prompt=True)
        n = len(tok(text, add_special_tokens=False).input_ids)
        if n > max_len - 8:
            out.append({"id": r["id"], "prompt_hash": r["prompt_hash"], "model": model,
                        "raw": None, "top": [], "skipped": True, "n_tokens": n})
            continue
        texts.append(text)
        keep.append((r, n))
    for (r, n), o in zip(keep, llm.generate(texts, sp)):
        gen = o.outputs[0]
        lp0 = gen.logprobs[0] if gen.logprobs else {}
        top = sorted(({"token": getattr(lp, "decoded_token", "") or "", "logprob": lp.logprob}
                      for lp in lp0.values()), key=lambda x: -x["logprob"])
        out.append({"id": r["id"], "prompt_hash": r["prompt_hash"], "model": model,
                    "raw": gen.text, "top": top, "n_tokens": n})
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--requests", type=Path, required=True)
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--backend", choices=["vllm", "stub"], default="vllm")
    ap.add_argument("--model", default="Qwen/Qwen3.8-27B-FP8")
    ap.add_argument("--max-model-len", type=int, default=4096)
    ap.add_argument("--top-logprobs", type=int, default=20)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--pilot", type=int, default=0, help="a random sample of N requests (seed --seed)")
    ap.add_argument("--shard-size", type=int, default=2000)
    ap.add_argument("--enforce-eager", action="store_true")
    ap.add_argument("--tensor-parallel", type=int, default=1, help="GPUs to split the model across")
    ap.add_argument("--quantization", default="", help="e.g. fp8 for weight-only FP8 of a bf16 model on an A40")
    ap.add_argument("--max-tokens", type=int, default=1,
                    help="1 for first-token classification; larger for generation (codebook induction)")
    a = ap.parse_args()

    reqs = read_jsonl(a.requests)
    done = {(r["id"], r["prompt_hash"]) for r in read_jsonl(a.out)}
    todo = [r for r in reqs if (r["id"], r["prompt_hash"]) not in done]
    if a.pilot:
        random.Random(a.seed).shuffle(todo)
        todo = todo[: a.pilot]
    print(f"[runner] {len(reqs):,} requests, {len(done):,} done, {len(todo):,} to run "
          f"({a.backend}, {a.model})", flush=True)
    a.out.parent.mkdir(parents=True, exist_ok=True)
    if a.backend == "vllm" and todo:
        llm, tok, sp = load_vllm(a.model, a.max_model_len, a.top_logprobs, a.seed, a.max_tokens,
                                 a.quantization, a.enforce_eager, a.tensor_parallel)
    for s in range(0, len(todo), a.shard_size):
        shard = todo[s: s + a.shard_size]
        rows = run_stub(shard) if a.backend == "stub" else run_vllm(llm, tok, sp, shard, a.model,
                                                                    a.max_model_len)
        append_jsonl(a.out, rows)
        print(f"[runner] {s + len(shard):,}/{len(todo):,}", flush=True)


if __name__ == "__main__":
    main()
