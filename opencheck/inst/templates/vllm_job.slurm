#!/bin/bash
# opencheck: first-token classification with offline vLLM on ALICE.
# Written by opencheck::oc_slurm_job(). Copy the runner, the request bundle and
# this script to {{ROOT}}, then: sbatch {{ROOT}}/{{JOB_NAME}}.slurm
# Resumable: resubmit to continue; finished requests are skipped.
#
#SBATCH --job-name={{JOB_NAME}}
#SBATCH --account={{ACCOUNT}}
#SBATCH --partition={{PARTITION}}
#SBATCH --gres={{GRES}}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={{CPUS}}
#SBATCH --mem={{MEM}}
#SBATCH --time={{TIME}}
#SBATCH --output={{ROOT}}/logs/%j.out
#SBATCH --error={{ROOT}}/logs/%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user={{EMAIL}}

set -uo pipefail

ROOT={{ROOT}}
VENV={{VENV}}   # shared; never pip install into it
PY="${VENV}/bin/python"
mkdir -p "${ROOT}/logs" "${ROOT}/results"

export HF_HOME={{HF_HOME}}
export HF_HUB_OFFLINE=1 TRANSFORMERS_OFFLINE=1
export PYTORCH_ALLOC_CONF=expandable_segments:True
export VLLM_USE_FLASHINFER_SAMPLER=0   # avoids a JIT compile that needs nvcc (newer vLLM)
CACHE="${TMPDIR:-/tmp/oc_${SLURM_JOB_ID}}/cache"
export VLLM_CACHE_ROOT="${CACHE}/vllm" TORCHINDUCTOR_CACHE_DIR="${CACHE}/inductor" TRITON_CACHE_DIR="${CACHE}/triton"
mkdir -p "${CACHE}"

echo "Job ${SLURM_JOB_ID} on ${SLURMD_NODENAME:-$(hostname)} | $(date)"
echo "GPUs: $(nvidia-smi --query-gpu=name,memory.total --format=csv,noheader | paste -sd';')"
echo "MODEL={{MODEL}} PILOT={{PILOT}}"

"${PY}" "${ROOT}/oc_vllm_runner.py" --requests "${ROOT}/{{REQUESTS}}" \
  --out "${ROOT}/results/{{RESULTS}}" --model "{{MODEL}}" \
  --max-model-len {{MAX_LEN}} --top-logprobs {{TOP}} --seed {{SEED}} --pilot {{PILOT}} \
  --max-tokens {{MAX_TOKENS}} {{QUANT_ARG}} {{EAGER_ARG}} {{TP_ARG}}
EXIT=$?
echo "Finished $(date) | exit ${EXIT}"
exit ${EXIT}
