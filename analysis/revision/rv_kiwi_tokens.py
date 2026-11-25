"""Korean content-word tokens with kiwipiepy, for the STM arm (rv_13_stm_arm.R).

Same parts of speech and length rule as analysis/R/02_preprocess.R:
NNG, NNP, VV, VA, MM, MAG, XR; forms longer than one character.
Reads a JSON list of strings from argv[1], writes a JSON list of token lists
to argv[2]. Nothing is printed.
"""
import json
import sys

from kiwipiepy import Kiwi

KEEP = {"NNG", "NNP", "VV", "VA", "MM", "MAG", "XR"}


def main():
    texts = json.load(open(sys.argv[1], encoding="utf-8"))
    kiwi = Kiwi()
    out = [[t.form for t in kiwi.tokenize(x or "") if t.tag in KEEP and len(t.form) > 1] for x in texts]
    json.dump(out, open(sys.argv[2], "w", encoding="utf-8"), ensure_ascii=False)


if __name__ == "__main__":
    main()
