---
name: opencheck
description: >
  Runs a split-sample construct check on the open-text answers of a survey
  experiment with the opencheck R package: reads the design, drafts a study
  card with the researcher, splits the sample, explores the discovery fold
  with a topic model, freezes yes/no questions, and estimates the treatment
  effect on what respondents wrote in the held-out fold. Use when a researcher
  wants to know whether a treatment made respondents mention the consideration
  the design assumes, asks to validate a construct or a mechanism with
  open-ended answers, or asks to "run opencheck". Not for general text
  classification, sentiment coding, or exploring answers with no treatment.
---

# opencheck: did the treatment change what respondents wrote?

A survey experiment assumes that a treatment brings a particular consideration
to mind. If it does, respondents who received the treatment should mention that
consideration more often in an open-text answer than those who did not. This
skill estimates that difference with the `opencheck` R package and keeps the
measure separate from the test: the coding scheme is settled on one part of the
sample (the discovery fold) and the effect is estimated on the rest (the
estimation fold).

You guide the researcher through seven steps. You draft; the researcher
decides. Ask one question at a time, and when you ask, show the passage from
their own documents that your draft is based on.

The exact R calls for every step are in [`reference/pipeline.R`](reference/pipeline.R),
a script that runs from start to finish on a small built-in example. Copy it
into the researcher's project and adapt it; do not write the calls from memory.
The package also installs it at `system.file("skills", "opencheck",
"reference", "pipeline.R", package = "opencheck")`.
How to read the results is in [`reference/interpreting.md`](reference/interpreting.md).

## Rules that hold in every step

1. **The card comes before the text.** The treatment contrast, the target
   construct, its expected direction and the placebos are written down before
   any answer is read. If the researcher has already read the answers or built
   a codebook from them, record that on the card; do not hide it.
2. **One split, never redrawn.** Draw it once with a seed and write it to disk.
3. **Before the lock exists, only the discovery fold.** Do not read, print,
   summarize or tabulate estimation-fold answers, and do not compute anything
   on them by treatment arm. Use `oc_discovery_data()` for every look at the
   text.
4. **The topic model sees the discovery fold only and no treatment variable.**
   A topic model fit to all answers with treatment as a covariate, and then
   tested on the same answers, reports an effect most of the time when there is
   none.
5. **Freeze, then estimate.** Prompts, model and settings go into the lockfile.
   The estimation fold opens only with that lock. After it has opened, the
   measure does not change.
6. **Report everything that was specified.** Every target and every placebo
   under every contrast on the card, not the ones that came out as hoped.
7. **Text stays on the researcher's machine by default.** Use a local model.
   Send answers to a commercial API only if the researcher says their consent
   form and ethics approval allow it.
8. **Do not invent a construct.** The target comes from what the researcher's
   theory, pre-analysis plan or paper says the treatment should do. If those
   documents state no expectation, say that the check does not apply and offer
   an exploratory description of the discovery fold instead, labeled as
   exploratory.

## Step 0. Check the setup

- R with `opencheck` installed, plus `stm`, `quanteda` and `stopwords` for the
  topic model. For Korean answers, also Python with `kiwipiepy`, used through
  `oc_tokenizer_kiwi()`.
- A model endpoint. The default is the local tier: `oc_profile("local")`
  recommends a small model (Gemma 4 E4B) served by Ollama on the researcher's
  own computer through an OpenAI-compatible address, and the backend is built
  with `oc_backend_openai()`. Any OpenAI-compatible endpoint works. Turn off
  the model's thinking mode (on Ollama, `extra = list(reasoning_effort =
  "none")`); otherwise the short answer comes back empty. Send one test
  request before going on and check that it returns a plain yes or no. If the
  endpoint does not return first-token log-probabilities, the yes/no answers
  are still read as labels and the estimates do not change, but the
  probability of "yes" is missing; tell the researcher. An endpoint
  that rejects the request for log-probabilities with an error cannot be used
  through `oc_backend_openai()`.
- A data file with one row per respondent (or per task, in a conjoint), an id
  column, the open-text column, the treatment variable, and any blocking
  variables used in the randomization.

## Step 1. Read the design and draft the study card

Ask for the questionnaire and for the pre-analysis plan or the paper. Read them
and fill in the template (`system.file("templates/study_card.yaml", package =
"opencheck")`). For each entry, quote the source passage.

- **Treatment and contrast.** Which arms are compared, and which comparison is
  the one the theory is about (the primary contrast).
- **The open-text item.** Its exact wording and where it appears. It must come
  after the treatment in every arm that is compared.
- **Target construct.** What a respondent would write if the treatment worked
  as intended: a short name, a one- or two-sentence definition in the
  researcher's own terms, the passage that states it, and the expected
  direction (the treatment raises it, lowers it, or should not change it).
- **Placebo constructs.** One or two considerations the treatment should not
  raise, each with a reason.
- **Provenance.** Whether any answers have been read, and whether an existing
  codebook was built with them in view.

Then confirm with the researcher, one question at a time:

1. Is this the comparison your theory is about?
2. Is this what a respondent would write if the treatment worked as you intend?
3. Should the treatment make respondents mention it more often, or less?
4. What should the treatment not make them mention? (the placebos)
5. Have you read the answers already, or coded any of them?

In a conjoint, define the contrast on the profiles a respondent was shown,
never on the profile they chose, because the choice comes after the treatment.
An answer given once at the end of the experiment responds to the whole
sequence of profiles.

Validate and register the card (`oc_study_read()`, `oc_study_register()`).
Register each later change with `oc_study_register(..., amend = "reason")`,
which keeps a copy of every version and logs the reason in the registry. The
package does not refuse an amendment after the lock is written, so make
changes only before Step 5; the card is frozen in the lock, and a card changed
after that keeps the estimation fold closed.

## Step 2. Draw the split

Split once, stratified on the treatment and on any blocking variable, with 30
percent of respondents in the discovery fold unless the researcher has a reason
for another share. Report the size of each arm in the estimation fold. If the
smaller arm of the primary contrast has fewer than about 200 answers, tell the
researcher now that the check will have little power: in the evaluation of 139
experiments, the expected effect was recovered in 57 percent of such contrasts
against 70 percent of the others.

## Step 3. Explore the discovery fold

Fit a topic model to the discovery fold (`oc_topics_fit()`; read the fold
with `oc_discovery_data()` and pass `log =` so the access is recorded) and
show the researcher each topic's words and example answers. Reading
discovery-fold answers is allowed and useful. Three questions for the
researcher:

1. Do respondents mention the target consideration at all?
2. Is there a consideration here that the treatment should not raise and that
   is missing from the placebos?
3. Does the definition of the target match how respondents put it?

The topic model is for exploring. It is a weak measure of a construct named in
advance, so do not use a topic's share as the outcome unless the researcher
asks for it as a comparison.

## Step 4. Write the questions and try them on the discovery fold

Write one yes/no question per target and placebo from the card's definitions.
Run them on a sample of discovery-fold answers. Check the share of valid
answers and the share of "yes", and read with the researcher a few answers the
model marked yes and a few it marked no. If the model misreads the construct,
revise the definition on the card, register the amendment, and try again. All
of this happens on the discovery fold.

Use the yes/no question as the measure. With a small model, do not use 0 to 9
ratings: they let placebo constructs rise more often.

## Step 5. Freeze

Write the lockfile with `oc_freeze()`. Its `components` are files: the study
card, which the yes/no questions are built from (and the topic model saved
with `oc_topics_save()`, if it will be used as a comparison). The questions
themselves go in `prompts`, and the model name and the decoding settings in
`models` and `decoding`; these two are recorded as given, so take them from
the backend that will be used. Tell the researcher that from here on the
measure is fixed.

## Step 6. Estimate on the estimation fold

Open the estimation fold with the lock (`oc_estimation_data()`, again with
`log =`), run the frozen questions on every answer (`oc_classify()` with
`lock =`), and estimate each target and each placebo under each contrast with
`oc_effect()`: a difference in means, blocked as the randomization was, with
randomization inference. The outcome is 1 for "yes" and 0 for "no";
`oc_effect()` takes a 0/1 treatment, so code each contrast as its treated arm
against its comparison arm.

## Step 7. Report

Give the researcher one table with every target and placebo: the estimate, its
standard error and interval, the randomization p-value, the number of answers
per arm, the share of "yes" in the control arm, and the expected direction.
State the result in one sentence per target, following
`reference/interpreting.md`. Then list the files that reproduce it: the study
card and its registry entry, the split, the lockfile, the access log, the
classification cache and the results table.

Anything done after the estimation fold was opened (a revised definition, a
new construct, a subgroup) is exploratory. Label it that way in the report.
