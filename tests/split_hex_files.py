#!/usr/bin/env python3
# This script iterates over all *.hex files in the directory
# and turns them into versions that have 32 / 64 bits on every
# line and saves them in the corresponding directories.
from pathlib import Path
from glob import glob
import textwrap

_script_dir = Path( __file__ ).parent


def convert(words: list, chars: int):
    divisor = len(words[0]) // chars
    assert divisor * chars == len(words[0]), f"{divisor} * {chars} != {len(words[0])}"
    out = ""
    for word in words:
        new_words = "\n".join(reversed(textwrap.wrap(word, chars)))
        out += new_words + "\n"
    return out

if __name__ == "__main__":
    files = glob("*.hex", root_dir=_script_dir)
    for name in files:
        with open(_script_dir / name) as ff:
            lines = [l.strip() for l in ff.readlines()]
        for bits in [32, 64]:
            with open(_script_dir / str(bits) / name, "w") as ff:
                ff.write(convert(lines, bits // 4))
