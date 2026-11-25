# Coding rule: does the target restate content the treatment displayed?

Written 24 November 2026, before any coding, after the pre-submission review. The coding is post hoc
(all estimates existed). Coders read the registered study cards only. They do not open any file under
`analysis/output/`, `data/`, or `paper/`, and they do not see any estimate.

Unit: one primary contrast (dataset, contrast, target construct), 205 rows in `primary_contrasts.csv`.

## Variable 1: `displayed` (1, 0)

Code 1 when both hold:

1. The materials shown to one arm of the contrast state or depict the content that the target construct
   names (a fact, an attribute and its value, an actor, an argument, a frame's key consideration), and
2. the materials shown to the other arm do not.

A respondent in the first arm could then score on the target by repeating what was shown. The arm that
displays the content is the arm expected to score higher. For a target expected to fall, apply the same
test to the comparison arm.

Code 0 when the target names something the treatment is expected to bring about without stating it:
an evaluation, an emotion, an attribution of responsibility, a belief about a third party, a reason for a
choice, or any consideration the respondent must supply. Code 0 also when both arms display the content.

Decision rules for common cases:

- Conjoint or vignette with an attribute shown in one arm and absent in the other, target = mention of
  that attribute or its content: 1.
- Information treatment, target = the information provided: 1. Target = a conclusion drawn from it: 0.
- Frame or argument treatment, target = the consideration the frame text itself spells out: 1.
  Target = a downstream reaction (support, anger, blame) that the text does not state: 0.
- Both arms show a value of the same attribute and the target is one of the two values (for example
  "rural" against "urban"): 1, because only one arm displays the target value.
- The target definition is broader than the displayed content but includes it by name: 1.
- The arms answered different writing prompts and the prompt names the target: 1.
- If the card does not describe the stimulus well enough to decide: 0, with `confidence = low`.

## Variable 2: `card_note` (1, 0)

1 if the card itself says, for this target or this contrast, that the shift is a positive control,
mechanical, a carry-over of wording, a manipulation check, or close to one. 0 otherwise, including when
the card uses such a phrase only for a different contrast or target. Give the quoted phrase.

## Variable 3: `routed` (1, 0), one value per card

1 if the open-text item is asked only of respondents selected by an answer they gave after treatment
(for example "asked only of those who said no"). 0 otherwise. Give the quoted phrase.

## Variable 4: `examples_read` (1, 0, ambiguous), one value per card, public and own cards alike

1 if `provenance` (or the card header comment) records that the drafter or searcher read example answers
to this item before the card was drafted: quotations printed in the publication, sample responses in a
codebook, or values in the data file. 0 if it records that none was read or says nothing. "ambiguous" if
example answers were read but the card does not say for which item.

## Variable 5: `results_known` (1, 0), one value per card

1 if `provenance` records that the item's own text results by arm (a published table or figure of coded
open answers by condition, or a by-arm tabulation of a human code) were seen before drafting. 0 otherwise.

## Output

One CSV row per primary contrast: `dataset, contrast, construct, displayed, confidence (high|low),
reason (25 words at most, no response text), card_note, card_note_quote, routed, routed_quote,
examples_read, examples_quote, results_known, results_quote`. Card-level variables repeat on each row of
the card. For a card with no primary contrast (`hartnett2022_whytrump`), one row with the card-level
variables and `displayed = NA`.
