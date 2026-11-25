# STM versus zero-shot LLM classification (205 primary contrasts)

## Detection of the theorized shift

| Arm | Covered | Detected (covered) | Detected (all) | Theorized sign (covered) |
|---|---|---|---|---|
| LLM yes/no, 35B | 205 | 67% (138/205) | 67% (138/205) | 90% (184/205) |
| LLM yes/no, 4B | 205 | 62% (127/205) | 62% (127/205) | 87% (178/205) |
| STM, no covariate | 182 | 41% (74/182) | 36% (74/205) | 66% (121/182) |
| STM, treatment covariate | 188 | 42% (79/188) | 39% (79/205) | 74% (139/188) |

## Detection by design feature (all contrasts; no matched topic counts as undetected)

| Arm | Smaller arm < 200 | Smaller arm >= 200 | Own-answer prompts | Other prompts |
|---|---|---|---|---|
| LLM yes/no, 35B | 57% (26/46) | 70% (112/159) | 70% (78/112) | 65% (59/91) |
| LLM yes/no, 4B | 48% (22/46) | 66% (105/159) | 65% (73/112) | 58% (53/91) |
| STM, no covariate | 17% (8/46) | 42% (66/159) | 33% (37/112) | 40% (36/91) |
| STM, treatment covariate | 28% (13/46) | 42% (66/159) | 37% (41/112) | 42% (38/91) |

## Agreement with the reference LLM, contrast by contrast

| Arm | Same detection call | LLM detects, arm misses | Arm detects, LLM misses | r (standardized effects) |
|---|---|---|---|---|
| LLM yes/no, 4B | 89% (182/205) | 17 | 6 | 0.98 |
| STM, no covariate | 67% (122/182) | 57 | 3 | 0.72 |
| STM, treatment covariate | 70% (131/188) | 54 | 3 | 0.76 |

## Against the original authors' human codes

- STM, no covariate: median AUC 0.64 over 40 matched targets (LLM 0.95); detects 16 of 46 human-coded contrasts (human codes 30, LLM 31)
- STM, treatment covariate: median AUC 0.63 over 41 matched targets (LLM 0.95); detects 20 of 46 human-coded contrasts (human codes 30, LLM 31)

## Compute

- STM fit with the covariate, one CPU core, K fixed: median 2.3 s per study, max 16.1 s, 7.0 CPU-minutes for 139 studies. Vocabulary identical to the original fit in 133 of 139.
- STM choice of K (searchK over K = 4..12), 139 studies, one core: median 8 s, max 66 s per study.
- STM topic labeling, gemma4_12b: 8 jobs, 2.8 GPU-hours (l4) for 205 requests (49907.3 ms per request, model loading included)
- STM topic labeling, qwen36_35b: 1 jobs, 0.3 GPU-hours (a40) for 85 requests (11800.0 ms per request, model loading included)
- gemma4_12b: 1 jobs, 1.0 GPU-hours (a40) for 284,440 requests (12.8 ms per request, model loading included)
- gemma4_31b: 1 jobs, 0.8 GPU-hours (a100) for 85,576 requests (32.9 ms per request, model loading included)
- gemma4_e4b: 8 jobs, 3.1 GPU-hours (a40) for 1,282,642 requests (8.7 ms per request, model loading included)
- qwen36_27b: 1 jobs, 1.3 GPU-hours (a40) for 41,860 requests (115.0 ms per request, model loading included)
- qwen36_35b: 14 jobs, 22.4 GPU-hours (a40) for 3,135,200 requests (25.7 ms per request, model loading included)
- Binary yes/no labels on the estimation samples: 702,487 (3,951 per study, median). At the measured rates: 35B model 5.0 A40-hours for the corpus, 1.7 GPU-minutes per median study; 4B model 1.7 hours, 0.6 minutes.
