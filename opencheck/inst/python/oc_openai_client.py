"""OpenAI chat client for opencheck request bundles (label-only models).

For models such as GPT-6 Luna that accept neither temperature 0 nor logprobs:
each request is sent with reasoning_effort "none" and the model's answer is
stored as a hard label (top = [{token: answer, logprob: 0}]), so
opencheck::oc_import_results() gives probability 1 to the chosen code. These
labels are not deterministic run to run; report them as such.

Resumable (skips id + prompt_hash already in --out). Key: OPENAI_API_KEY.

    python3 -I oc_openai_client.py --requests req.jsonl --out res.jsonl --model gpt-6-luna --pilot 20
"""
import argparse, http.client, json, os, sys, threading, time, urllib.error, urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

URL = "https://api.openai.com/v1/chat/completions"
PRICE = {"gpt-6-luna": (0.10, 0.50)}  # USD per 1M input / output tokens (secondary sources, Oct 2026)


def post(body, key, attempts=6):
    data = json.dumps(body, ensure_ascii=False).encode("utf-8")
    req = urllib.request.Request(URL, data=data, method="POST", headers={
        "Authorization": f"Bearer {key}", "Content-Type": "application/json"})
    for a in range(attempts):
        try:
            with urllib.request.urlopen(req, timeout=60) as r:
                return json.loads(r.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            if e.code in (429, 500, 502, 503, 504) and a < attempts - 1:
                time.sleep(2 ** a + 0.5); continue
            raise RuntimeError(f"HTTP {e.code}: {e.read(300).decode('utf-8', 'replace')}") from None
        except (urllib.error.URLError, http.client.HTTPException, ConnectionError, TimeoutError) as e:
            if a < attempts - 1:
                time.sleep(2 ** a + 0.5); continue
            raise RuntimeError(f"network: {e}") from None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--requests", type=Path, required=True)
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--model", default="gpt-6-luna")
    ap.add_argument("--workers", type=int, default=32)
    ap.add_argument("--pilot", type=int, default=0)
    ap.add_argument("--max-usd", type=float, default=40.0)
    a = ap.parse_args()
    key = os.environ.get("OPENAI_API_KEY", "")
    if not key:
        sys.exit("OPENAI_API_KEY not set")
    reqs = [json.loads(l) for l in a.requests.read_text(encoding="utf-8").split("\n") if l.strip()]
    done = set()
    if a.out.exists():
        done = {(x["id"], x["prompt_hash"]) for x in (json.loads(l) for l in a.out.read_text(encoding="utf-8").split("\n") if l.strip())}
    todo, seen = [], set(done)
    for r in reqs:
        k = (r["id"], r["prompt_hash"])
        if k not in seen:
            seen.add(k); todo.append(r)
    if a.pilot:
        todo = todo[: a.pilot]
    print(f"[openai] {len(reqs):,} requests, {len(done):,} done, {len(todo):,} to run ({a.model})", flush=True)
    pin, pout = PRICE.get(a.model, (0.0, 0.0))
    usage = {"in": 0, "out": 0}; stop = threading.Event(); lock = threading.Lock()

    def one(r):
        if stop.is_set():
            return None
        body = {"model": a.model, "reasoning_effort": "none", "max_completion_tokens": 16,
                "messages": [{"role": "system", "content": r["system"]}, {"role": "user", "content": r["user"]}]}
        res = post(body, key)
        ans = (res["choices"][0]["message"].get("content") or "").strip()
        u = res.get("usage", {})
        return {"id": r["id"], "prompt_hash": r["prompt_hash"], "model": res.get("model", a.model), "raw": ans,
                "top": [{"token": ans, "logprob": 0.0}], "label_only": True,
                "usage_in": u.get("prompt_tokens", 0), "usage_out": u.get("completion_tokens", 0)}

    a.out.parent.mkdir(parents=True, exist_ok=True)
    n = 0
    with ThreadPoolExecutor(max_workers=a.workers) as ex, a.out.open("a", encoding="utf-8") as f:
        for fu in as_completed([ex.submit(one, r) for r in todo]):
            try:
                res = fu.result()
            except RuntimeError as e:
                print(f"[openai] error: {e}", flush=True); continue
            if res is None:
                continue
            with lock:
                f.write(json.dumps(res, ensure_ascii=False) + "\n")
                usage["in"] += res["usage_in"]; usage["out"] += res["usage_out"]; n += 1
                usd = usage["in"] / 1e6 * pin + usage["out"] / 1e6 * pout
                if n % 2000 == 0:
                    f.flush(); print(f"[openai] {n:,}/{len(todo):,}; ${usd:.2f}", flush=True)
                if usd > a.max_usd and not stop.is_set():
                    print(f"[openai] stopping at ${usd:.2f}", flush=True); stop.set()
    print(f"[openai] done {n:,}; tokens in {usage['in']:,} out {usage['out']:,}; ${usage['in']/1e6*pin + usage['out']/1e6*pout:.2f}")


if __name__ == "__main__":
    main()
