"""Jev (TypeSafe typed-decision endpoint) client for opencheck request bundles.

Reads the same JSON-lines bundle as oc_vllm_runner.py (id, prompt_hash, system,
user, codes) and asks each request as one Jev Choice question over the codes.
Jev returns a probability for every option, written as `top` (token = code,
logprob = log p), so opencheck::oc_import_results() reads the output unchanged.
Request design follows the Jev replication of DiGiuseppe & Flynn (kjyg Stage 6):
the study instructions go in the state, and the codes become worded criteria.

Resumable (skips id + prompt_hash already in --out); results are appended as
they return. --endpoint typesafe (default) calls TypeSafe directly with
TYPESAFE_API_KEY or Secret Service (service typesafe-jev); --endpoint openrouter
calls OpenRouter's System One API with OPENROUTER_API_KEY or Secret Service
(service openrouter; override with --key-service) and model typesafe/jev-1.13.
Cost is reported from usage at the posted price ($0.042 per 1M input tokens,
output free; checked 2026-09) and the run stops at --max-usd.

    python3 -I oc_jev_client.py --requests req.jsonl --out res.jsonl --dry-run
    python3 -I oc_jev_client.py --requests req.jsonl --out res.jsonl --pilot 10
    python3 -I oc_jev_client.py --requests req.jsonl --out res.jsonl --workers 16 --max-usd 20
"""
import argparse
import http.client
import json
import math
import os
import ssl
import subprocess
import sys
import threading
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

ENDPOINTS = {
    "typesafe": ("https://api.typesafe.ai/v1/systemone", "TYPESAFE_API_KEY", "typesafe-jev", "jev-1.13.0"),
    "openrouter": ("https://openrouter.ai/api/v1/systemone", "OPENROUTER_API_KEY", "openrouter", "typesafe/jev-1.13"),
}
ENDPOINT = ENDPOINTS["typesafe"][0]
USD_PER_M_INPUT = 0.042


def api_key(env: str = "TYPESAFE_API_KEY", service: str = "typesafe-jev") -> str:
    key = os.environ.get(env, "").strip()
    if not key:
        try:
            key = subprocess.run(["secret-tool", "lookup", "service", service, "username",
                                  os.environ.get("USER", "")], capture_output=True, text=True,
                                 timeout=10).stdout.strip()
        except (OSError, subprocess.TimeoutExpired):
            key = ""
    if not key:
        sys.exit(f"No Jev key: set {env} or store it with secret-tool (service {service}).")
    return key


def criteria(r: dict) -> dict:
    codes = r["codes"]
    if set(codes) == {"yes", "no"}:
        return {"yes": "The response expresses the consideration described in the instructions.",
                "no": "The response does not express that consideration."}
    if codes == [str(i) for i in range(10)]:
        return {c: f"Rating {c} on the 0-9 scale described in the instructions." for c in codes}
    if codes == ["1", "2"] and "Response 1:" in r["user"]:
        return {"1": "Response 1 expresses the consideration more than Response 2.",
                "2": "Response 2 expresses the consideration more than Response 1."}
    menu = {}
    for line in r["user"].splitlines():
        if line.startswith("Codes: "):
            for part in line[len("Codes: "):].split(", "):
                if " = " in part:
                    k, v = part.split(" = ", 1)
                    menu[k.strip()] = v.strip()
    return {c: f"Code {c}" + (f" ({menu[c]})" if c in menu else "") + " as defined in the instructions." for c in codes}


def payload(r: dict, model: str) -> dict:
    return {"state": {"instructions": r["system"], "response": r["user"]},
            "model": model,
            "questions": {"answer": {"type": "choice",
                                     "instructions": "Choose the answer the instructions ask for.",
                                     "criteria": criteria(r)}}}


_opener = urllib.request.build_opener(urllib.request.HTTPSHandler(context=ssl.create_default_context()))


def post(body: dict, key: str, attempts: int = 5) -> dict:
    data = json.dumps(body, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
    req = urllib.request.Request(ENDPOINT, data=data, method="POST", headers={
        "Authorization": f"Bearer {key}", "Content-Type": "application/json",
        "Accept": "application/json", "User-Agent": "opencheck-jev/0.1"})
    for a in range(attempts):
        try:
            with _opener.open(req, timeout=60) as resp:
                return json.loads(resp.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            if e.code in (429, 500, 502, 503, 504) and a < attempts - 1:
                time.sleep(2 ** a + 0.5)
                continue
            raise RuntimeError(f"HTTP {e.code}: {e.read(300).decode('utf-8', 'replace')}") from None
        except (urllib.error.URLError, http.client.HTTPException, ConnectionError, TimeoutError, ssl.SSLError) as e:
            if a < attempts - 1:
                time.sleep(2 ** a + 0.5)
                continue
            raise RuntimeError(f"network: {e}") from None


def to_result(r: dict, resp: dict, model: str) -> dict:
    ans = resp["answers"]["answer"]
    probs = ans.get("probabilities", {})
    top = [{"token": c, "logprob": math.log(max(float(p), 1e-12))} for c, p in probs.items()]
    top.sort(key=lambda x: -x["logprob"])
    return {"id": r["id"], "prompt_hash": r["prompt_hash"], "model": resp.get("model", model),
            "raw": ans.get("choice", ""), "top": top,
            "confidence": ans.get("confidence"),
            "usage_in": int(resp.get("usage", {}).get("input_tokens", 0))}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--requests", type=Path, required=True)
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--endpoint", choices=sorted(ENDPOINTS), default="typesafe")
    ap.add_argument("--key-service", default=None)
    ap.add_argument("--model", default=None)
    ap.add_argument("--workers", type=int, default=8)
    ap.add_argument("--pilot", type=int, default=0)
    ap.add_argument("--max-usd", type=float, default=25.0)
    ap.add_argument("--dry-run", action="store_true")
    a = ap.parse_args()
    global ENDPOINT
    ENDPOINT, key_env, key_service, default_model = ENDPOINTS[a.endpoint]
    a.model = a.model or default_model
    reqs = [json.loads(l) for l in a.requests.read_text(encoding="utf-8").split("\n") if l.strip()]
    done = set()
    if a.out.exists():
        done = {(x["id"], x["prompt_hash"]) for x in
                (json.loads(l) for l in a.out.read_text(encoding="utf-8").split("\n") if l.strip())}
    todo = [r for r in reqs if (r["id"], r["prompt_hash"]) not in done]
    if a.pilot:
        todo = todo[: a.pilot]
    chars = sum(len(r["system"]) + len(r["user"]) for r in todo)
    print(f"[jev] {len(reqs):,} requests, {len(done):,} done, {len(todo):,} to run; "
          f"~{chars / 3.5 / 1e6:.2f}M input tokens, ~${chars / 3.5 / 1e6 * USD_PER_M_INPUT:.2f}", flush=True)
    if a.dry_run:
        if todo:
            print(json.dumps(payload(todo[0], a.model)["questions"], ensure_ascii=False)[:600])
        return
    key = api_key(key_env, a.key_service or key_service)
    a.out.parent.mkdir(parents=True, exist_ok=True)
    lock = threading.Lock()
    spent = {"tokens": 0}
    stop = threading.Event()

    def one(r):
        if stop.is_set():
            return None
        return to_result(r, post(payload(r, a.model), key), a.model)

    n = 0
    with ThreadPoolExecutor(max_workers=a.workers) as ex, a.out.open("a", encoding="utf-8") as f:
        futs = [ex.submit(one, r) for r in todo]
        for fu in as_completed(futs):
            try:
                res = fu.result()
            except RuntimeError as e:
                print(f"[jev] error: {e}", flush=True)
                continue
            if res is None:
                continue
            with lock:
                f.write(json.dumps(res, ensure_ascii=False) + "\n")
                spent["tokens"] += res["usage_in"]
                n += 1
                usd = spent["tokens"] / 1e6 * USD_PER_M_INPUT
                if n % 500 == 0:
                    f.flush()
                    print(f"[jev] {n:,}/{len(todo):,}; ${usd:.2f}", flush=True)
                if usd > a.max_usd and not stop.is_set():
                    print(f"[jev] stopping: spend ${usd:.2f} > ${a.max_usd}", flush=True)
                    stop.set()
    print(f"[jev] done {n:,}; input tokens {spent['tokens']:,}; ${spent['tokens'] / 1e6 * USD_PER_M_INPUT:.2f}")


if __name__ == "__main__":
    main()
